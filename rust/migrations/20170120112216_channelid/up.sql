CREATE TABLE subscriptions (
  id Integer PRIMARY KEY NOT NULL,
  channelid VARCHAR NOT NULL DEFAULT "",
  channelname VARCHAR NOT NULL,
  uploadplaylist VARCHAR NOT NULL,
  thumbnail VARCHAR NOT NULL,
  description TEXT NOT NULL
)
