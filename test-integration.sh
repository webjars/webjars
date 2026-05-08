#!/usr/bin/env bash
#
# External integration tests for WebJars
# Validates HTTP endpoints against a running dev server.
# Usage: ./test-integration.sh [BASE_URL]
#
set -uo pipefail

BASE_URL="${1:-http://localhost:9000}"
PASS=0
FAIL=0
SKIP=0
FAILURES=()

# ── Helpers ──────────────────────────────────────────────────────────────────

red()   { printf '\033[0;31m%s\033[0m' "$*"; }
green() { printf '\033[0;32m%s\033[0m' "$*"; }
yellow(){ printf '\033[0;33m%s\033[0m' "$*"; }
bold()  { printf '\033[1m%s\033[0m' "$*"; }

pass() { PASS=$((PASS + 1)); echo "  $(green PASS) $1"; }
fail() { FAIL=$((FAIL + 1)); FAILURES+=("$1: $2"); echo "  $(red FAIL) $1 — $2"; }
skip() { SKIP=$((SKIP + 1)); echo "  $(yellow SKIP) $1 — $2"; }

# Run a curl and capture status code + body. Sets $HTTP_STATUS and $HTTP_BODY.
# Usage: http GET /path [extra-curl-args...]
http() {
  local method="$1"; shift
  local path="$1"; shift
  local url="${BASE_URL}${path}"
  local tmpfile
  tmpfile=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpfile" -w '%{http_code}' -X "$method" "$@" "$url") || true
  HTTP_BODY=$(cat "$tmpfile")
  rm -f "$tmpfile"
}

# Assert HTTP status code
assert_status() {
  local test_name="$1" expected="$2"
  if [ "$HTTP_STATUS" = "$expected" ]; then
    return 0
  else
    fail "$test_name" "expected status $expected, got $HTTP_STATUS"
    return 1
  fi
}

# Assert body contains a string
assert_body_contains() {
  local test_name="$1" needle="$2"
  if grep -qF "$needle" <<< "$HTTP_BODY"; then
    return 0
  else
    fail "$test_name" "body does not contain '$needle'"
    return 1
  fi
}

# Assert body matches a regex
assert_body_matches() {
  local test_name="$1" pattern="$2"
  if grep -qE "$pattern" <<< "$HTTP_BODY"; then
    return 0
  else
    fail "$test_name" "body does not match pattern '$pattern'"
    return 1
  fi
}

# Assert a response header is present (case-insensitive)
# Uses a separate curl to capture headers.
assert_header() {
  local test_name="$1" header_name="$2" expected_value="$3" method="$4" path="$5"
  shift 5
  local url="${BASE_URL}${path}"
  local headers
  headers=$(curl -s -D - -o /dev/null -X "$method" "$@" "$url") || true
  if grep -iqF "${header_name}: ${expected_value}" <<< "$headers"; then
    return 0
  else
    fail "$test_name" "header '${header_name}: ${expected_value}' not found"
    return 1
  fi
}

section() {
  echo ""
  bold "━━ $1 ━━"
}

# ── Wait for server ──────────────────────────────────────────────────────────

wait_for_server() {
  echo "Waiting for server at ${BASE_URL} ..."
  local max_wait=300  # 5 minutes
  local waited=0
  while ! curl -s -o /dev/null -w '' --max-time 5 "${BASE_URL}/documentation" 2>/dev/null; do
    sleep 3
    waited=$((waited + 3))
    if [ "$waited" -ge "$max_wait" ]; then
      echo "$(red ERROR): Server did not become ready within ${max_wait}s"
      exit 1
    fi
    printf "."
  done
  echo ""
  echo "Server is ready (waited ${waited}s)"
}

# ── Tests ────────────────────────────────────────────────────────────────────

test_pages() {
  section "Pages"

  # GET / — home page
  http GET /
  if assert_status "GET / returns 200" "200"; then
    assert_body_contains "GET / contains WebJars heading" "WebJars"
    pass "GET / returns 200"
  fi

  # GET /documentation
  http GET /documentation
  if assert_status "GET /documentation returns 200" "200"; then
    assert_body_contains "GET /documentation has content" "Documentation"
    pass "GET /documentation returns 200"
  fi

  # GET /all — HTML
  http GET /all -H "Accept: text/html"
  if assert_status "GET /all (HTML) returns 200" "200"; then
    assert_body_contains "GET /all (HTML) contains WebJars" "WebJars"
    pass "GET /all (HTML) returns 200"
  fi

  # GET /all — JSON
  http GET /all -H "Accept: application/json"
  if assert_status "GET /all (JSON) returns 200" "200"; then
    assert_body_matches "GET /all (JSON) is a JSON array" '^\['
    pass "GET /all (JSON) returns 200"
  fi
}

test_popular() {
  section "Popular WebJars"

  # GET /popular — JSON
  http GET /popular -H "Accept: application/json"
  if assert_status "GET /popular (JSON) returns 200" "200"; then
    assert_body_matches "GET /popular (JSON) is a JSON array" '^\['
    # Verify groupId fields are present
    assert_body_contains "GET /popular contains groupId" '"groupId"'
    assert_body_contains "GET /popular contains artifactId" '"artifactId"'
    pass "GET /popular (JSON) returns 200"
  fi

  # GET /popular — HTML
  http GET /popular -H "Accept: text/html"
  assert_status "GET /popular (HTML) returns 200" "200" && pass "GET /popular (HTML) returns 200"
}

test_search() {
  section "Search"

  # Search for jquery across npm
  http GET "/search?query=jquery&groupId=org.webjars.npm" -H "Accept: application/json"
  if assert_status "GET /search jquery (JSON) returns 200" "200"; then
    assert_body_matches "GET /search jquery is JSON array" '^\['
    pass "GET /search jquery (JSON) returns 200"
  fi

  # Search with HTML accept
  http GET "/search?query=jquery&groupId=org.webjars.npm" -H "Accept: text/html"
  assert_status "GET /search jquery (HTML) returns 200" "200" && pass "GET /search jquery (HTML) returns 200"
}

test_list() {
  section "List WebJars"

  # GET /list/org.webjars.npm
  http GET "/list/org.webjars.npm" -H "Accept: application/json"
  if assert_status "GET /list/org.webjars.npm returns 200" "200"; then
    assert_body_matches "GET /list/org.webjars.npm is JSON array" '^\['
    assert_body_contains "GET /list/org.webjars.npm has versions" '"versions"'
    pass "GET /list/org.webjars.npm returns 200"
  fi

  # GET /list/org.webjars
  http GET "/list/org.webjars" -H "Accept: application/json"
  if assert_status "GET /list/org.webjars returns 200" "200"; then
    assert_body_matches "GET /list/org.webjars is JSON array" '^\['
    pass "GET /list/org.webjars returns 200"
  fi
}

test_package_exists() {
  section "Package Exists"

  # Known NPM package
  http GET "/exists?webJarType=npm&name=jquery"
  if assert_status "GET /exists npm jquery returns 200" "200"; then
    assert_body_contains "GET /exists npm jquery returns XML root" "<root"
    pass "GET /exists npm jquery returns 200"
  fi

  # Invalid webjar type
  http GET "/exists?webJarType=invalid&name=jquery"
  assert_status "GET /exists invalid type returns 400" "400" && pass "GET /exists invalid type returns 400"
}

test_versions() {
  section "Package Versions"

  # NPM package versions
  http GET "/versions?webJarType=npm&name=jquery"
  if assert_status "GET /versions npm jquery returns 200" "200"; then
    assert_body_matches "GET /versions npm jquery is JSON array" '^\['
    # jquery should have well-known versions
    assert_body_contains "GET /versions npm jquery includes versions" '"3.'
    pass "GET /versions npm jquery returns 200"
  fi

  # Git repo with branch
  http GET "/versions?webJarType=npm&name=https%3A%2F%2Fgithub.com%2Fjindw%2Fxmldom.git&branch=master"
  if assert_status "GET /versions git+branch returns 200" "200"; then
    assert_body_matches "GET /versions git+branch is JSON array" '^\['
    pass "GET /versions git+branch returns 200"
  fi

  # Invalid webjar type
  http GET "/versions?webJarType=invalid&name=jquery"
  assert_status "GET /versions invalid type returns 400" "400" && pass "GET /versions invalid type returns 400"
}

test_listfiles() {
  section "List Files"

  # List files for a well-known webjar (uses external file service)
  http GET "/listfiles/org.webjars.npm/jquery/3.7.1" -H "Accept: application/json"
  if [ "$HTTP_STATUS" = "200" ]; then
    assert_body_matches "GET /listfiles jquery 3.7.1 is JSON array" '^\['
    pass "GET /listfiles jquery 3.7.1 returns 200"
  else
    skip "GET /listfiles jquery 3.7.1" "file service may be unavailable (status: $HTTP_STATUS)"
  fi

  # Default groupId form
  http GET "/listfiles/jquery/3.7.1" -H "Accept: application/json"
  if [ "$HTTP_STATUS" = "200" ]; then
    pass "GET /listfiles (default groupId) jquery 3.7.1 returns 200"
  else
    skip "GET /listfiles (default groupId)" "file service may be unavailable (status: $HTTP_STATUS)"
  fi
}

test_file_redirect() {
  section "File Redirects"

  # File redirect should return 301
  HTTP_STATUS=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 -X GET "${BASE_URL}/files/org.webjars.npm/jquery/3.7.1/dist/jquery.min.js") || true
  if [ "$HTTP_STATUS" = "301" ]; then
    pass "GET /files/.../jquery.min.js returns 301 redirect"
  else
    fail "GET /files/.../jquery.min.js returns 301 redirect" "got status $HTTP_STATUS"
  fi

  # Verify redirect location
  local location
  location=$(curl -s -D - -o /dev/null --max-time 10 "${BASE_URL}/files/org.webjars.npm/jquery/3.7.1/dist/jquery.min.js" | grep -i "^location:" | tr -d '\r') || true
  if echo "$location" | grep -q "webjars-file-service"; then
    pass "File redirect points to file service"
  else
    fail "File redirect points to file service" "location: $location"
  fi

  # Default groupId file redirect
  HTTP_STATUS=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 -X GET "${BASE_URL}/files/jquery/3.7.1/dist/jquery.min.js") || true
  if [ "$HTTP_STATUS" = "301" ]; then
    pass "GET /files (default groupId) returns 301"
  else
    fail "GET /files (default groupId) returns 301" "got status $HTTP_STATUS"
  fi
}

test_cors() {
  section "CORS"

  # OPTIONS preflight on /files/*
  http OPTIONS "/files/test"
  if assert_status "OPTIONS /files/* returns 200" "200"; then
    pass "OPTIONS /files/* returns 200"
  fi
  assert_header "OPTIONS /files/* has CORS Allow-Origin" \
    "Access-Control-Allow-Origin" "*" OPTIONS "/files/test"  && \
    pass "OPTIONS /files/* has Access-Control-Allow-Origin: *"

  # OPTIONS preflight on arbitrary path
  http OPTIONS "/some/path"
  if assert_status "OPTIONS /* returns 200" "200"; then
    pass "OPTIONS /* returns 200"
  fi
  assert_header "OPTIONS /* has CORS Allow-Origin" \
    "Access-Control-Allow-Origin" "*" OPTIONS "/some/path" && \
    pass "OPTIONS /* has Access-Control-Allow-Origin: *"
  assert_header "OPTIONS /* has CORS Allow-Methods" \
    "Access-Control-Allow-Methods" "GET" OPTIONS "/some/path" && \
    pass "OPTIONS /* has Access-Control-Allow-Methods: GET"

  # GET /all should have CORS header
  assert_header "GET /all has CORS header" \
    "Access-Control-Allow-Origin" "*" GET "/all" -H "Accept: application/json" && \
    pass "GET /all has Access-Control-Allow-Origin: *"
}

test_static_assets() {
  section "Static Assets"

  # favicon.ico
  HTTP_STATUS=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 "${BASE_URL}/favicon.ico") || true
  if [ "$HTTP_STATUS" = "200" ]; then
    pass "GET /favicon.ico returns 200"
  else
    fail "GET /favicon.ico returns 200" "got status $HTTP_STATUS"
  fi

  # robots.txt
  http GET /robots.txt
  assert_status "GET /robots.txt returns 200" "200" && pass "GET /robots.txt returns 200"

  # /files/robots.txt (alternate path)
  http GET /files/robots.txt
  assert_status "GET /files/robots.txt returns 200" "200" && pass "GET /files/robots.txt returns 200"
}

test_create_npm() {
  section "Create NPM WebJar"

  # Create a small, well-known npm webjar (jquery 3.3.0 — same as ApplicationSpec)
  local tmpjar
  tmpjar=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpjar" -w '%{http_code}' --max-time 300 \
    -X POST "${BASE_URL}/create?webJarType=npm&nameOrUrlish=jquery&version=3.3.0") || true

  if [ "$HTTP_STATUS" = "200" ]; then
    local size
    size=$(wc -c < "$tmpjar" | tr -d ' ')
    if [ "$size" -gt 0 ]; then
      pass "POST /create npm jquery 3.3.0 returns 200 (${size} bytes)"
      # Verify it's actually a JAR (ZIP magic bytes PK\x03\x04)
      local magic
      magic=$(xxd -l 2 -p "$tmpjar")
      if [ "$magic" = "504b" ]; then
        pass "POST /create npm jquery 3.3.0 returns valid JAR"
      else
        fail "POST /create npm jquery 3.3.0 returns valid JAR" "bad magic: $magic"
      fi
    else
      fail "POST /create npm jquery 3.3.0 body not empty" "empty response"
    fi
  else
    fail "POST /create npm jquery 3.3.0 returns 200" "got status $HTTP_STATUS"
  fi
  rm -f "$tmpjar"
}

test_create_classic() {
  section "Create Classic WebJar"

  # Normal properties (name + repo) — mirrors ApplicationSpec
  local tmpjar
  tmpjar=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpjar" -w '%{http_code}' --max-time 300 \
    -X POST "${BASE_URL}/create/classic?nameOrUrlish=bootstrap&version=v5.3.0" \
    -H "Content-Type: text/plain" \
    -d $'name=Bootstrap\nrepo=twbs/bootstrap\n') || true

  if [ "$HTTP_STATUS" = "200" ]; then
    local size
    size=$(wc -c < "$tmpjar" | tr -d ' ')
    pass "POST /create/classic bootstrap v5.3.0 returns 200 (${size} bytes)"
  else
    fail "POST /create/classic bootstrap v5.3.0 returns 200" "got status $HTTP_STATUS, body: $(echo "$HTTP_BODY" | head -c 200)"
  fi
  rm -f "$tmpjar"

  # NPM-based properties
  tmpjar=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpjar" -w '%{http_code}' --max-time 300 \
    -X POST "${BASE_URL}/create/classic?nameOrUrlish=jquery&version=3.7.0" \
    -H "Content-Type: text/plain" \
    -d $'npm=jquery\nlicense.name=MIT\nlicense.url=https://jquery.org/license/\n') || true

  if [ "$HTTP_STATUS" = "200" ]; then
    local size
    size=$(wc -c < "$tmpjar" | tr -d ' ')
    pass "POST /create/classic jquery 3.7.0 (npm) returns 200 (${size} bytes)"
  else
    fail "POST /create/classic jquery 3.7.0 (npm) returns 200" "got status $HTTP_STATUS"
  fi
  rm -f "$tmpjar"

  # Missing required fields → 400
  http POST "/create/classic?nameOrUrlish=bootstrap&version=v5.3.0" \
    -H "Content-Type: text/plain" \
    -d $'name=Bootstrap\n'
  if assert_status "POST /create/classic missing repo returns 400" "400"; then
    assert_body_contains "POST /create/classic missing repo says Invalid" "Invalid properties"
    pass "POST /create/classic missing repo returns 400"
  fi

  # Missing body → 400
  http POST "/create/classic?nameOrUrlish=bootstrap&version=v5.3.0"
  if assert_status "POST /create/classic no body returns 400" "400"; then
    assert_body_contains "POST /create/classic no body message" "Expected text/plain body"
    pass "POST /create/classic no body returns 400"
  fi

  # Properties with base.dir
  tmpjar=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpjar" -w '%{http_code}' --max-time 300 \
    -X POST "${BASE_URL}/create/classic?nameOrUrlish=bootstrap&version=v5.3.0" \
    -H "Content-Type: text/plain" \
    -d $'name=Bootstrap\nrepo=twbs/bootstrap\nbase.dir=dist/**\n') || true

  if [ "$HTTP_STATUS" = "200" ]; then
    pass "POST /create/classic with base.dir returns 200"
  else
    fail "POST /create/classic with base.dir returns 200" "got status $HTTP_STATUS"
  fi
  rm -f "$tmpjar"

  # Properties with custom download URL
  tmpjar=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpjar" -w '%{http_code}' --max-time 300 \
    -X POST "${BASE_URL}/create/classic?nameOrUrlish=bootstrap&version=5.3.0" \
    -H "Content-Type: text/plain" \
    -d 'name=Bootstrap
repo=twbs/bootstrap
download=https://github.com/twbs/bootstrap/releases/download/v${version}/bootstrap-${version}-dist.zip
') || true

  if [ "$HTTP_STATUS" = "200" ]; then
    pass "POST /create/classic with download URL returns 200"
  else
    fail "POST /create/classic with download URL returns 200" "got status $HTTP_STATUS"
  fi
  rm -f "$tmpjar"
}

test_deploy_streaming() {
  section "Deploy (streaming)"

  # Test that deploy endpoint returns chunked streaming response (text/plain)
  # We use a known package but don't actually expect a full deploy to succeed
  # without Maven Central credentials — just verify the endpoint responds
  local tmpfile
  tmpfile=$(mktemp)
  HTTP_STATUS=$(curl -s -o "$tmpfile" -w '%{http_code}' --max-time 60 \
    -X POST "${BASE_URL}/deploy?webJarType=npm&nameOrUrlish=jquery&version=3.7.1" \
    -H "Accept: text/plain") || true

  if [ "$HTTP_STATUS" = "200" ]; then
    local body
    body=$(cat "$tmpfile")
    if [ -n "$body" ]; then
      pass "POST /deploy returns 200 with streaming body"
    else
      pass "POST /deploy returns 200"
    fi
  else
    # Deploy may fail due to missing credentials — that's OK, we just test the endpoint is wired up
    skip "POST /deploy" "returned status $HTTP_STATUS (credentials may be missing)"
  fi
  rm -f "$tmpfile"

  # Invalid type
  http POST "/deploy?webJarType=invalid&nameOrUrlish=test&version=1.0" -H "Accept: text/plain"
  assert_status "POST /deploy invalid type returns 400" "400" && pass "POST /deploy invalid type returns 400"
}

test_content_negotiation() {
  section "Content Negotiation"

  # GET / only serves HTML
  http GET / -H "Accept: text/html"
  assert_status "GET / with Accept: text/html returns 200" "200" && pass "GET / with Accept: text/html returns 200"

  # /popular serves both HTML and JSON
  http GET /popular -H "Accept: text/html"
  if assert_status "GET /popular Accept: text/html returns 200" "200"; then
    # HTML response should not start with [
    if ! echo "$HTTP_BODY" | head -c 1 | grep -q '\['; then
      pass "GET /popular text/html returns HTML"
    else
      fail "GET /popular text/html returns HTML" "got JSON instead"
    fi
  fi

  http GET /popular -H "Accept: application/json"
  if assert_status "GET /popular Accept: application/json returns 200" "200"; then
    assert_body_matches "GET /popular application/json returns JSON" '^\['
    pass "GET /popular application/json returns JSON array"
  fi
}

test_error_handling() {
  section "Error Handling"

  # Create with invalid webJarType
  http POST "/create?webJarType=invalid&nameOrUrlish=test&version=1.0"
  assert_status "POST /create invalid type returns 400" "400" && pass "POST /create invalid type returns 400"

  # Non-existent route returns 404
  http GET "/nonexistent-route-xyz"
  # Play may return 404 or handle via OPTIONS fallback
  if [ "$HTTP_STATUS" = "404" ] || [ "$HTTP_STATUS" = "400" ]; then
    pass "GET /nonexistent-route returns 4xx ($HTTP_STATUS)"
  else
    fail "GET /nonexistent-route returns 4xx" "got $HTTP_STATUS"
  fi
}

# ── Main ─────────────────────────────────────────────────────────────────────

main() {
  echo "$(bold 'WebJars Integration Tests')"
  echo "Target: ${BASE_URL}"
  echo ""

  wait_for_server

  test_pages
  test_popular
  test_search
  test_list
  test_package_exists
  test_versions
  test_listfiles
  test_file_redirect
  test_cors
  test_static_assets
  test_content_negotiation
  test_error_handling
  test_create_npm
  test_create_classic
  test_deploy_streaming

  # ── Summary ──────────────────────────────────────────────────────────────

  echo ""
  echo "$(bold '━━ Summary ━━')"
  echo "  $(green "Passed: $PASS")"
  if [ "$FAIL" -gt 0 ]; then
    echo "  $(red "Failed: $FAIL")"
  else
    echo "  Failed: $FAIL"
  fi
  if [ "$SKIP" -gt 0 ]; then
    echo "  $(yellow "Skipped: $SKIP")"
  fi

  if [ "$FAIL" -gt 0 ]; then
    echo ""
    echo "$(red 'Failures:')"
    for f in "${FAILURES[@]}"; do
      echo "  - $f"
    done
    exit 1
  fi

  echo ""
  echo "$(green 'All tests passed!')"
}

main
