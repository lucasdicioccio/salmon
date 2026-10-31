-- salmon-toy-clickhouse: the events table, one replica per member.
-- {{on_cluster}} is replaced by ON CLUSTER `main`; IF NOT EXISTS lets a
-- partly applied run be applied again.
CREATE TABLE IF NOT EXISTS toy.events {{on_cluster}}
(
    at DateTime,
    kind LowCardinality(String),
    value UInt64
)
ENGINE = ReplicatedMergeTree
ORDER BY (kind, at);
