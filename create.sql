
CREATE TABLE IF NOT EXISTS story (datetime DATETIME,user TEXT,mes TEXT);

CREATE INDEX IF NOT EXISTS story_datetime ON story (datetime ASC);

CREATE INDEX IF NOT EXISTS story_user ON story (user ASC);
