package webjars.utils

import zio.*
import zio.cache.{Cache as ZCache, Lookup}

// A heterogeneous get-or-compute cache backed by zio-cache. Each key gets
// its own degenerate (capacity = 1) zio-cache, used as a memo cell with a
// TTL. The most recent `onMiss` for each key is held in a Ref so that calls
// after a TTL miss re-run the up-to-date lookup function.
//
// Failures are intentionally not cached: after the underlying lookup fails,
// the entry is invalidated so the very next call re-runs against the fresh
// `onMissRef`. The TTL therefore only governs successful values. This is
// what we want for upstream lookups whose "absent" answer (e.g. a missing
// `<name>.properties` in webjars-classic, or a transient GitHub 5xx) can
// flip the moment the next call is made.
trait Cache:
  def get[K](key: String, expiration: Duration)(onMiss: => ZIO[Scope, Throwable, K]): ZIO[Scope, Throwable, K]

class CacheLive private (
  memos: Ref.Synchronized[Map[String, CacheLive.Entry]],
) extends Cache:
  import CacheLive.Entry

  def get[K](key: String, expiration: Duration)(onMiss: => ZIO[Scope, Throwable, K]): ZIO[Scope, Throwable, K] =
    memos.modifyZIO { current =>
      current.get(key) match
        case Some(entry) =>
          // Update the entry's latest onMiss so the next zio-cache miss runs the fresh lookup.
          entry.onMissRef.set(() => ZIO.scoped(onMiss)).as((entry, current))
        case None =>
          for
            onMissRef <- Ref.make[() => ZIO[Any, Throwable, Any]](() => ZIO.scoped(onMiss))
            cache     <- ZCache.make(
                            capacity = 1,
                            timeToLive = expiration,
                            lookup = Lookup[Unit, Any, Throwable, Any](_ => onMissRef.get.flatMap(_())),
                         )
            entry      = Entry(onMissRef, cache)
          yield (entry, current.updated(key, entry))
    }.flatMap { entry =>
      // Don't cache failures: if the lookup fails, invalidate the cell so
      // the next call re-runs the (possibly updated) lookup. Successful
      // values still respect the TTL.
      entry.cache.get(()).onError(_ => entry.cache.invalidate(()))
    }.map(_.asInstanceOf[K])

object CacheLive:
  private final case class Entry(
    onMissRef: Ref[() => ZIO[Any, Throwable, Any]],
    cache: ZCache[Unit, Throwable, Any],
  )

  def apply(): CacheLive = Unsafe.unsafe { implicit u =>
    new CacheLive(Ref.Synchronized.unsafe.make(Map.empty))
  }

object Cache:
  val live: ZLayer[Any, Nothing, Cache] = ZLayer.succeed(CacheLive())
