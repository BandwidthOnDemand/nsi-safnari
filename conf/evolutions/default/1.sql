# --- !Ups

CREATE SEQUENCE pk_sequence;

CREATE TABLE messages (
  id BIGINT DEFAULT NEXTVAL('pk_sequence') NOT NULL,
  aggregated_connection_id TEXT NOT NULL,
  correlation_id UUID NOT NULL,
  protocol TEXT NOT NULL,
  type TEXT NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL,
  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE messages;
