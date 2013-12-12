# --- !Ups

DROP INDEX messages_correlation_id;

# --- !Downs

CREATE UNIQUE INDEX messages_correlation_id ON messages (correlation_id, type);
