-- salmon-toy-clickhouse: a second migration, a column added on every member.
ALTER TABLE toy.events {{on_cluster}} ADD COLUMN IF NOT EXISTS source String DEFAULT '';
