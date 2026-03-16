BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS roles (
  name TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO roles (name, description)
VALUES
  ('viewer', 'Read-only wiki access'),
  ('editor', 'Can create and edit wiki content'),
  ('admin', 'Can manage wiki administration features')
ON CONFLICT (name) DO NOTHING;

CREATE TABLE IF NOT EXISTS users (
  id BIGSERIAL PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  display_name TEXT NOT NULL,
  password_hash TEXT NOT NULL,
  role_name TEXT NOT NULL REFERENCES roles(name),
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_login_at TIMESTAMPTZ
);

DROP TRIGGER IF EXISTS trg_users_set_updated_at ON users;
CREATE TRIGGER trg_users_set_updated_at
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

CREATE TABLE IF NOT EXISTS user_sessions (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  session_token TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  expires_at TIMESTAMPTZ NOT NULL,
  revoked_at TIMESTAMPTZ
);

CREATE INDEX IF NOT EXISTS idx_user_sessions_user_id_created
  ON user_sessions (user_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_user_sessions_expires_at
  ON user_sessions (expires_at);

INSERT INTO users (username, display_name, password_hash, role_name)
VALUES (
  'admin',
  'Administrator',
  crypt('admin', gen_salt('bf')),
  'admin'
)
ON CONFLICT (username) DO NOTHING;

COMMIT;
