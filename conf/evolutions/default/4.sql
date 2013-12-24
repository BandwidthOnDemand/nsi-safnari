# --- !Ups

TRUNCATE messages;

ALTER TABLE messages ALTER COLUMN created_at TYPE TIMESTAMP WITH TIME ZONE;

CREATE TABLE connections (
    id BIGINT DEFAULT NEXTVAL('pk_sequence') NOT NULL,
    aggregated_connection_id TEXT NOT NULL,
    requester_nsa TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    deleted_at TIMESTAMP WITH TIME ZONE,
    PRIMARY KEY (id));
CREATE UNIQUE INDEX connections_aggregated_connection_id ON connections (aggregated_connection_id);

ALTER TABLE messages DROP COLUMN aggregated_connection_id;
ALTER TABLE messages ADD COLUMN connection_id BIGINT NOT NULL;
ALTER TABLE messages ADD CONSTRAINT connection_fkey FOREIGN KEY (connection_id) REFERENCES connections (id);

# --- !Downs

TRUNCATE messages;
ALTER TABLE messages DROP COLUMN connection_id;
ALTER TABLE messages ALTER COLUMN created_at TYPE TIMESTAMP WITHOUT TIME ZONE;
ALTER TABLE messages ADD COLUMN aggregated_connection_id TEXT NOT NULL;

DROP TABLE connections;
