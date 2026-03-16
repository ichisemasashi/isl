BEGIN;

CREATE TABLE IF NOT EXISTS backup_runs (
  id BIGSERIAL PRIMARY KEY,
  triggered_by_user_id BIGINT REFERENCES users(id) ON DELETE SET NULL,
  mode TEXT NOT NULL DEFAULT 'backup',
  status TEXT NOT NULL,
  backup_dir TEXT,
  keep_count INTEGER,
  dry_run BOOLEAN NOT NULL DEFAULT false,
  sql_path TEXT,
  media_path TEXT,
  message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  completed_at TIMESTAMPTZ,
  CONSTRAINT backup_runs_mode_chk CHECK (mode IN ('backup', 'restore')),
  CONSTRAINT backup_runs_status_chk CHECK (status IN ('ok', 'error', 'dry-run'))
);

CREATE INDEX IF NOT EXISTS idx_backup_runs_created_at ON backup_runs (created_at DESC);
CREATE INDEX IF NOT EXISTS idx_backup_runs_mode_created_at ON backup_runs (mode, created_at DESC);

COMMIT;
