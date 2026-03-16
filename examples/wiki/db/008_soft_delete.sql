BEGIN;

ALTER TABLE pages
  ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS deleted_by TEXT;

ALTER TABLE media_assets
  ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS deleted_by TEXT;

CREATE INDEX IF NOT EXISTS idx_pages_deleted_at ON pages (deleted_at);
CREATE INDEX IF NOT EXISTS idx_media_assets_deleted_at ON media_assets (deleted_at);

COMMIT;
