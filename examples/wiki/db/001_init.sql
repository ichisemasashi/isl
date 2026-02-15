BEGIN;

-- Update helper for updated_at columns.
CREATE OR REPLACE FUNCTION set_updated_at()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  NEW.updated_at := now();
  RETURN NEW;
END;
$$;

-- Current wiki page state.
CREATE TABLE IF NOT EXISTS pages (
  id BIGSERIAL PRIMARY KEY,
  slug TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  body_md TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_edited_by TEXT,
  CONSTRAINT pages_slug_format_chk
    CHECK (slug ~ '^[a-z0-9](?:[a-z0-9-]{0,126}[a-z0-9])?$'),
  CONSTRAINT pages_title_not_blank_chk
    CHECK (btrim(title) <> '')
);

CREATE INDEX IF NOT EXISTS idx_pages_updated_at ON pages (updated_at DESC);

DROP TRIGGER IF EXISTS trg_pages_set_updated_at ON pages;
CREATE TRIGGER trg_pages_set_updated_at
BEFORE UPDATE ON pages
FOR EACH ROW
EXECUTE FUNCTION set_updated_at();

-- Immutable page revision history.
CREATE TABLE IF NOT EXISTS page_revisions (
  id BIGSERIAL PRIMARY KEY,
  page_id BIGINT NOT NULL REFERENCES pages(id) ON DELETE CASCADE,
  rev_no INTEGER NOT NULL,
  title TEXT NOT NULL,
  body_md TEXT NOT NULL,
  edited_by TEXT,
  edit_summary TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT page_revisions_rev_no_chk CHECK (rev_no > 0),
  CONSTRAINT page_revisions_title_not_blank_chk CHECK (btrim(title) <> ''),
  CONSTRAINT page_revisions_page_rev_uniq UNIQUE (page_id, rev_no)
);

CREATE INDEX IF NOT EXISTS idx_page_revisions_page_created
  ON page_revisions (page_id, created_at DESC);

-- Seed top page used by first MVP route.
INSERT INTO pages (slug, title, body_md, last_edited_by)
VALUES ('home', 'Home', '# Welcome\n\nThis is the first wiki page.', 'system')
ON CONFLICT (slug) DO NOTHING;

INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary)
SELECT p.id, 1, p.title, p.body_md, 'system', 'Initial page'
FROM pages p
WHERE p.slug = 'home'
  AND NOT EXISTS (
    SELECT 1 FROM page_revisions r WHERE r.page_id = p.id AND r.rev_no = 1
  );

COMMIT;
