-- Replaces the Valkey layout. Rows that used to expire through a Redis TTL
-- carry an `expires_at` (or a `day` / `added_at`); readers ignore expired
-- rows and `Db.purgeExpired` deletes them every refresh cycle.

-- The cached WebJars: one row per groupId:artifactId (was a hash per groupId).
-- `versions` is the version list in Maven Central order:
-- [{"number": "3.7.1", "numFiles": 42}, {"number": "3.7.0"}].
create table webjar (
  group_id      text        not null,
  artifact_id   text        not null check (artifact_id <> ''),
  name          text        not null,
  source_url    text        not null,
  versions      jsonb       not null check (jsonb_typeof(versions) = 'array' and jsonb_array_length(versions) > 0),
  last_modified timestamptz not null,
  primary key (group_id, artifact_id)
);

-- Artifacts that failed to refresh (or are parent POMs), skipped by discovery until they expire.
create table artifact_tombstone (
  group_id    text        not null,
  artifact_id text        not null,
  expires_at  timestamptz not null,
  primary key (group_id, artifact_id)
);

-- Versions Maven Central lists but whose jar is missing or unusable.
create table version_tombstone (
  group_id    text        not null,
  artifact_id text        not null,
  version     text        not null,
  expires_at  timestamptz not null,
  primary key (group_id, artifact_id, version)
);

-- Versions this app published, awaiting Maven Central propagation.
create table pending_deploy (
  group_id    text        not null,
  artifact_id text        not null,
  version     text        not null,
  added_at    timestamptz not null,
  primary key (group_id, artifact_id, version)
);

-- The GitHub issue tracking a deploy's systemic failures.
create table deploy_failure_issue (
  group_id       text    not null,
  name_or_urlish text    not null,
  version        text    not null,
  issue_number   integer not null,
  primary key (group_id, name_or_urlish, version)
);

-- Popularity score per artifact per UTC day (see PopularMetrics).
create table popular_daily (
  day         date   not null,
  group_id    text   not null,
  artifact_id text   not null,
  score       bigint not null,
  primary key (day, group_id, artifact_id)
);

-- Cross-instance lease that keeps two deploys from publishing one GAV (see GavPublicationGuard).
create table gav_publication (
  group_id    text        not null,
  artifact_id text        not null,
  version     text        not null,
  state       text        not null check (state in ('publishing', 'complete', 'failed')),
  token       uuid        not null,
  expires_at  timestamptz not null,
  primary key (group_id, artifact_id, version)
);
