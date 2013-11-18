# --- !Ups

CREATE UNIQUE INDEX messages_correlation_id ON messages (correlation_id, type);

# --- !Downs

DROP INDEX messages_correlation_id;
