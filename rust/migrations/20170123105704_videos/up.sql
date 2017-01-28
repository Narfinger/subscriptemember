CREATE TABLE subscriptions (
  id Integer PRIMARY KEY NOT NULL,
  channelid VARCHAR NOT NULL DEFAULT "",
  channelname VARCHAR NOT NULL,
  uploadplaylist VARCHAR NOT NULL,
  thumbnail VARCHAR NOT NULL,
  description TEXT NOT NULL
);

CREATE TABLE videos (
  id Integer PRIMARY KEY NOT NULL,
  vid VARCHAR NOT NULL,
  title VARCHAR NOT NULL,
  thumbnail VARCHAR NOT NULL,
  published_at VARCHAR NOT NULL,
  channelname VARCHAR NOT NULL,
  url VARCHAR NOT NULL
);

CREATE TABLE config (
  id Integer PRIMARY KEY NOT NULL,
  lastupdate VARCHAR,
)
