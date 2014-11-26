# --- !Ups

ALTER TABLE connections RENAME COLUMN aggregated_connection_id TO connection_id;
ALTER TABLE messages DROP COLUMN protocol, DROP COLUMN direction;
ALTER TABLE messages ALTER COLUMN correlation_id DROP NOT NULL;

CREATE INDEX messages_connection_id_idx ON messages (connection_id, id);

# --- !Downs

DROP INDEX messages_connection_id_idx;

ALTER TABLE messages
  ADD COLUMN direction VARCHAR(10) NOT NULL CHECK (direction IN ('INBOUND', 'OUTBOUND')),
  ADD COLUMN protocol TEXT NOT NULL;
ALTER TABLE connections RENAME COLUMN connection_id TO aggregated_connection_id;
