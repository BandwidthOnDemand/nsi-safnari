# --- !Ups

ALTER TABLE connections ADD COLUMN correlation_id_generator_version SMALLINT;

# --- !Downs

ALTER TABLE connections DROP COLUMN correlation_id_generator_version;
