# --- !Ups

CREATE INDEX messages_correlation_id ON messages (correlation_id);

# --- !Downs

DROP INDEX messages_correlation_id;
