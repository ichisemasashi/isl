BEGIN;

CREATE TABLE IF NOT EXISTS media_assets (
  id BIGSERIAL PRIMARY KEY,
  page_id BIGINT REFERENCES pages(id) ON DELETE SET NULL,
  media_type TEXT NOT NULL,
  title TEXT,
  original_filename TEXT NOT NULL,
  stored_filename TEXT NOT NULL UNIQUE,
  mime_type TEXT NOT NULL,
  storage_path TEXT NOT NULL,
  public_url TEXT NOT NULL,
  created_by TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT media_assets_type_chk CHECK (media_type IN ('image', 'video', 'audio', 'file')),
  CONSTRAINT media_assets_filename_not_blank_chk CHECK (btrim(stored_filename) <> ''),
  CONSTRAINT media_assets_public_url_not_blank_chk CHECK (btrim(public_url) <> '')
);

CREATE INDEX IF NOT EXISTS idx_media_assets_page_id_created ON media_assets (page_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_media_assets_type_created ON media_assets (media_type, created_at DESC);

COMMIT;
