# Users schema

# --- !Ups

CREATE TABLE messages (
  id BIGINT(20) NOT NULL AUTO_INCREMENT,
  aggregated_connection_id UUID NOT NULL,
  correlation_id UUID NOT NULL,
  protocol VARCHAR NOT NULL,
  content VARCHAR NOT NULL,
  created_at TIMESTAMP NOT NULL,
  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE messages;
