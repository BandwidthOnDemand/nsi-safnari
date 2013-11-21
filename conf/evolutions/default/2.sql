# --- !Ups

CREATE UNIQUE INDEX messages_correlation_id ON messages (correlation_id, type);

ALTER TABLE messages
  ADD COLUMN inbound_message_id BIGINT,
  ADD COLUMN direction VARCHAR(10) CHECK (direction IN ('INBOUND', 'OUTBOUND'));

UPDATE messages SET direction =
  CASE type
    WHEN 'FromRequester' THEN 'INBOUND'
    WHEN 'FromProvider'  THEN 'INBOUND'
    WHEN 'ProviderAck'   THEN 'INBOUND'
    WHEN 'FromPce'       THEN 'INBOUND'
    WHEN 'MessageDeliveryFailure' THEN 'INBOUND'
    WHEN 'ToRequester'   THEN 'OUTBOUND'
    WHEN 'ToProvider'    THEN 'OUTBOUND'
    WHEN 'ToPce'         THEN 'OUTBOUND'
  END;

ALTER TABLE messages ALTER COLUMN direction SET NOT NULL;

CREATE INDEX messages_inbound_message_id_idx ON messages (inbound_message_id);
ALTER TABLE messages ADD CONSTRAINT inbound_message_id_fkey FOREIGN KEY (inbound_message_id) REFERENCES messages (id);

# --- !Downs

DROP INDEX messages_correlation_id;
ALTER TABLE messages DROP COLUMN inbound_message_id, DROP COLUMN direction;
