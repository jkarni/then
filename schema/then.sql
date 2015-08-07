CREATE TABLE IF NOT EXISTS "user" (
    username   varchar(80)   PRIMARY KEY,
    email      text          UNIQUE NOT NULL,
    password   text          NOT NULL
);
