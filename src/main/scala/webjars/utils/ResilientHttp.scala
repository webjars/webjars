package webjars.utils

import io.netty.handler.codec.PrematureChannelClosureException
import zio.*
import zio.http.*

/** Shared hardening for the pooled zio-http client.
 *
 *  Every external host we talk to (api.github.com, registry.npmjs.org,
 *  semver.webjars.org, raw.githubusercontent.com, the file-service, and
 *  GitHub redirects) uses HTTP keep-alive, and all of them will
 *  eventually close an idle connection that our pooled Netty client then
 *  tries to reuse. zio-http surfaces that as a
 *  `PrematureChannelClosureException` ("Channel closed while executing the
 *  request") *before* the request reaches the server, so the call is safe
 *  to transparently retry.
 *
 *  This started as a private helper in `GitHubLive` — the paginating
 *  `allPages` reads flaked most often on CI because they fire the most
 *  sequential requests and are the most likely to draw a stale
 *  connection — and is hoisted here so every `client.batched` call site
 *  shares one policy.
 *
 *  NOTE: only use `batchedResilient` for idempotent requests (GET/HEAD).
 *  Retrying a mutating POST/PATCH on this fault is *usually* safe (the
 *  request never left the client), but the small ambiguity isn't worth it
 *  for the issue-mutating calls, which keep using plain `batched`. */
object ResilientHttp:

  private val transientConnectionRetry: Schedule[Any, Throwable, Any] =
    (Schedule.recurs(3) && Schedule.exponential(100.millis)).whileInput[Throwable]:
      case _: PrematureChannelClosureException => true
      case _                                   => false

  extension (client: Client)
    /** Like `client.batched`, but transparently retries the transient
     *  "channel closed" fault a reused keep-alive connection can throw
     *  before the request is sent. Idempotent requests only. */
    def batchedResilient(request: Request): ZIO[Scope, Throwable, Response] =
      client.batched(request).retry(transientConnectionRetry)
