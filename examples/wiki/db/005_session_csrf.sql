BEGIN;

ALTER TABLE user_sessions
ADD COLUMN IF NOT EXISTS csrf_token TEXT;

UPDATE user_sessions
SET csrf_token = session_token
WHERE csrf_token IS NULL;

ALTER TABLE user_sessions
ALTER COLUMN csrf_token SET NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS idx_user_sessions_csrf_token
  ON user_sessions (csrf_token);

COMMIT;
