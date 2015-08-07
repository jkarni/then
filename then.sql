CREATE EXTENSION IF NOT EXISTS citext;

CREATE TABLE IF NOT EXISTS "user" (
    username PRIMARY KEY,
    email citext UNIQUE NOT NULL,
    password text NOT NULL,
);
