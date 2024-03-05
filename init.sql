CREATE TABLE test (
  id SERIAL PRIMARY KEY,
  time  timestamp with time zone,
  iid TEXT,
  metric TEXT,
  value TEXT
);

CREATE TABLE errors (
  id SERIAL PRIMARY KEY,
  database_time  timestamp with time zone default (now()),
  mqtt_time timestamp with time zone,
  client_id  TEXT,
  headers  TEXT,
  topic TEXT,
  payload TEXT,
  error_message TEXT
);