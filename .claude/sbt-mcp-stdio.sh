#!/usr/bin/env bash
# stdio MCP bridge to this build's sbt-mcp server (http://127.0.0.1:<mcpPort>/).
# Source of truth: ~/projects/projects/sbt-mcp-stdio.sh (FACTORY.md). Copy it verbatim
# to .claude/sbt-mcp-stdio.sh; the port is read from `mcpPort := <port>` in build.sbt.
#
# Claude Code connects to MCP servers while the session starts, before sbt could be
# running, so a plain HTTP entry in .mcp.json fails with "connection refused". This
# script is a stdio MCP server instead: it starts sbt when needed (cloud sessions only),
# waits for sbt-mcp to listen, then relays each JSON-RPC message to it over HTTP.
# The relay is stateless (one HTTP request per message) and waits out sbt reloads:
# editing build.sbt makes sbt reload, which restarts the sbt-mcp HTTP server, and a
# persistent client such as mcp-remote never recovers from that ("fetch failed").
# stdout carries the MCP protocol only. Progress goes to $diag (and stderr); sbt's own
# output goes to $log. If the server doesn't connect, read both files.

set -u
cd "$(dirname "$0")/.." || exit 1

port="$(grep -hoE 'mcpPort[[:space:]]*:=[[:space:]]*[0-9]+' build.sbt | grep -oE '[0-9]+$' | head -1)"
[ -n "$port" ] || { echo "no mcpPort in build.sbt" >&2; exit 1; }
log="${TMPDIR:-/tmp}/sbt-mcp-server.log"
diag="${TMPDIR:-/tmp}/sbt-mcp-stdio.log"

say() { echo "$(date -u +%H:%M:%S) $*" | tee -a "$diag" >&2; }
listening() { curl -s -o /dev/null --max-time 2 --noproxy '*' "http://127.0.0.1:${port}/"; }

# Cloud sessions route traffic through an HTTP proxy; never send loopback traffic to it.
export NO_PROXY="127.0.0.1,localhost${NO_PROXY:+,$NO_PROXY}"
export no_proxy="$NO_PROXY"

say "start: pwd=$PWD CLAUDE_CODE_REMOTE=${CLAUDE_CODE_REMOTE:-} java=$(java -version 2>&1 | grep -m1 version)"

if ! listening; then
  if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
    say "sbt-mcp is not running on 127.0.0.1:${port}. Start sbt in this project, then reconnect."
    exit 1
  fi
  say "starting sbt (output: $log)"
  # Run sbt in the foreground (`--server`) with a stdin that never closes, detached with
  # setsid. sbt-mcp's sbt-task needs this attached console channel: a daemon started by a
  # one-off `./sbt <cmd>` has none ("no sbt channel available yet"). Later `./sbt <task>`
  # client calls connect to this same server.
  setsid nohup bash -c 'tail -f /dev/null | ./sbt --server --no-colors --supershell=false' > "$log" 2>&1 < /dev/null &
  for _ in $(seq 1 270); do
    listening && break
    sleep 2
  done
  listening || { say "sbt-mcp did not start; last lines of $log:"; tail -20 "$log" | tee -a "$diag" >&2; exit 1; }
fi

say "sbt-mcp is listening on ${port}; relaying stdio to http://127.0.0.1:${port}/"
exec python3 -u -c '
import json, sys, threading, time, urllib.error, urllib.request

url, diag = sys.argv[1], sys.argv[2]
opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))  # never use a proxy for loopback
out_lock = threading.Lock()

def log(msg):
    line = time.strftime("%H:%M:%S", time.gmtime()) + " relay: " + msg
    sys.stderr.write(line + "\n")
    with open(diag, "a") as f:
        f.write(line + "\n")

def send(obj):
    with out_lock:
        sys.stdout.write(json.dumps(obj) + "\n")
        sys.stdout.flush()

def wait_until_listening(deadline):
    import socket
    host, port = url.split("//")[1].rstrip("/").split(":")
    while time.time() < deadline:
        try:
            socket.create_connection((host, int(port)), timeout=2).close()
            time.sleep(2)  # let the restarted server finish starting
            return True
        except OSError:
            time.sleep(2)
    return False

def error(msg_id, text):
    if msg_id is not None:
        send({"jsonrpc": "2.0", "id": msg_id, "error": {"code": -32000, "message": text}})

def handle(line):
    try:
        msg_id = json.loads(line).get("id")
    except Exception:
        return
    headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}
    deadline = time.time() + 600
    while True:
        try:
            req = urllib.request.Request(url, data=line.encode(), headers=headers)
            with opener.open(req) as r:
                body, ctype = r.read().decode(), r.headers.get("Content-Type", "")
            break
        except urllib.error.HTTPError as e:
            body, ctype = e.read().decode(), e.headers.get("Content-Type", "")
            break
        except Exception as e:
            reason = getattr(e, "reason", e)
            refused = isinstance(reason, ConnectionRefusedError)
            if refused and time.time() < deadline:
                # Not delivered: sbt is (re)starting its MCP server, e.g. after a reload. Wait and resend.
                time.sleep(2)
                continue
            log("request failed: %r; waiting for sbt-mcp to come back" % (reason,))
            back = wait_until_listening(deadline)
            error(msg_id, "sbt-mcp connection was lost (%s), most likely because sbt reloaded the build "
                          "mid-request (a reload restarts the sbt-mcp server; sbt also reloads by itself before the "
                          "first command after build.sbt changes). The server is %s. The command may or may "
                          "not have completed: check its effect, and run it again as a separate call if "
                          "needed." % (reason, "back" if back else "still down"))
            return
    if not body.strip():
        return  # e.g. 202 Accepted for a notification
    try:
        if "text/event-stream" in ctype:
            for l in body.splitlines():
                if l.startswith("data:") and l[5:].strip():
                    send(json.loads(l[5:]))
        else:
            send(json.loads(body))
    except Exception:
        error(msg_id, "unexpected sbt-mcp response: " + body[:500])

for line in sys.stdin:
    if line.strip():
        threading.Thread(target=handle, args=(line,), daemon=True).start()
' "http://127.0.0.1:${port}/" "$diag"
