# Valkey → Postgres

The WebJars cache, tombstones, pending deploys, deploy-failure issue index,
popularity scores and the GAV publication lease move from Heroku Key-Value
Store (Valkey) to Heroku Postgres. Nothing else uses Valkey, so the add-on goes
away at the end.

Delete this file once the migration is done.

## Status (2026-11-12)

- [x] Code: branch `postgres`. Postgres replaces Valkey everywhere, with the
  Flyway V1 schema and `release: bin/flyway-migrate` in the Procfile.
  `Test / compile`, `stage` and the database specs pass locally.
- [x] Backfill rehearsed against a local `postgres:18.4`: V1 applied and 18,300
  WebJars loaded from `/all` in 15 s (14 MB).
- [ ] Review and merge the branch. Don't merge before steps 1 and 2:
  the release phase fails without `DATABASE_URL`.
- [ ] 1. Provision Postgres, and match `PostgresContainer.image` to its version.
- [ ] 2. Backfill production.
- [ ] 3. Deploy.
- [ ] 4. Verify.
- [ ] 6. Remove the Valkey add-on (after a few days).

Not yet confirmed: whether CI passes on the branch (Testcontainers Postgres on
GitHub Actions), and whether to also carry over the 30 days of popularity
scores (export them from Valkey; not implemented).

## What is and isn't carried over

- **WebJars cache:** backfilled from the live site's `/all` (step 2). `/all`
  has every cached artifact with its versions and numFiles; `lastModified` is
  set to the backfill time (only the refresh cycle reads it).
- **Not carried over**, all self-healing:
  - Artifact and version tombstones: the first refresh cycle after deploy
    re-fetches the artifacts Maven Central lists that aren't in the cache, and
    re-tombstones the ones that fail. Expect a longer first cycle.
  - Pending deploys: a version deployed in the hour before the switch gets
    picked up by discovery's numFiles backfill instead.
  - Deploy-failure issue numbers: the tracker finds open issues by exact title
    when the index has no entry.
  - Popularity scores: the home page falls back to the first 20 WebJars per
    group until the first day of scores is in (`PopularRanking`).
  - GAV publication leases: only matter during an in-flight deploy. Avoid
    deploying a WebJar while the release runs.

## 1. Provision Postgres

```bash
heroku addons:create heroku-postgresql:essential-0 --as DATABASE
heroku pg:wait
heroku pg:info   # note the Postgres version
```

The data is a few MB, so Essential-0 (1 GB, 20 connections) is plenty. The
pools use at most 5 (web) + 3 (deploy or cleanup one-off) + 2 (release)
connections. Attaching sets `DATABASE_URL` and restarts the dynos; the
running (Valkey) release ignores it.

If `heroku pg:info` shows a version other than 18.4, set
`PostgresContainer.image` (`src/test/scala/webjars/PostgresContainer.scala`)
to match.

## 2. Backfill

```bash
./sbta "Test/runMain webjars.WebJarsBackfill heroku"
```

This applies the Flyway migrations (the same ones the release phase runs) and
upserts every WebJar from https://www.webjars.org/all. It is safe to re-run.

## 3. Deploy

Deploy as usual. The Procfile's `release: bin/flyway-migrate` runs in Heroku's
release phase before the new code serves traffic; a failed migration aborts
the release. Here it is a no-op because step 2 already applied V1.

WebJars that reach the Valkey cache between the backfill and the deploy are
picked up by the next refresh cycle.

## 4. Verify

- `heroku logs --tail | grep -E 'SearchIndex rebuilt|PopularRanking populated'`:
  the in-memory snapshots load from Postgres (~5000 artifacts).
- `heroku logs --tail | grep 'Refresh done for'`: the first refresh cycle
  writes new artifacts and tombstones.
- `heroku logs --tail | grep -i 'query'`: magnum logs failed queries.
- `heroku pg:psql -c "select count(*) from webjar"` and, after a day,
  `select count(*) from popular_daily`.

## 5. Roll back (if needed, before step 6)

`heroku rollback` returns to the Valkey release; Valkey still has its data
(minus writes since the deploy). Postgres stays attached and unused.

## 6. Clean up (after a few days)

```bash
heroku addons        # find the Key-Value Store add-on's name
heroku addons:destroy <redis-add-on>   # also removes REDIS_URL
```
