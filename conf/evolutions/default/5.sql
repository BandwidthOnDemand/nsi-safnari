# --- !Ups

ALTER TABLE messages DROP COLUMN protocol, DROP COLUMN direction;

# --- !Downs

ALTER TABLE messages
  ADD COLUMN direction VARCHAR(10) NOT NULL CHECK (direction IN ('INBOUND', 'OUTBOUND')),
  ADD COLUMN protocol TEXT NOT NULL;
