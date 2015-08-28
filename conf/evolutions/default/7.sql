# --- !Ups

UPDATE messages SET content = REPLACE(content, '"}],"trace":[', '","parameter":[]}],"trace":[') WHERE type = 'ToPce';
UPDATE messages SET content = REPLACE(content, '"}]},{', '","parameter":[]}]},{') WHERE type = 'FromPce';
UPDATE messages SET content = REPLACE(content, '"}]}]}}', '","parameter":[]}]}]}}') WHERE type = 'FromPce';

# --- !Downs

UPDATE messages SET content = REPLACE(content, ',"parameter":[]', '') WHERE type = 'ToPce';
UPDATE messages SET content = REPLACE(content, '","parameter":[]}', '"}') WHERE type = 'FromPce';
