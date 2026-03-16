BEGIN;

ALTER TABLE pages
  ADD COLUMN IF NOT EXISTS status TEXT NOT NULL DEFAULT 'published',
  ADD COLUMN IF NOT EXISTS search_vector tsvector;

ALTER TABLE pages
  DROP CONSTRAINT IF EXISTS pages_status_chk;

ALTER TABLE pages
  ADD CONSTRAINT pages_status_chk
  CHECK (status IN ('draft', 'published', 'private'));

CREATE TABLE IF NOT EXISTS page_tags (
  id BIGSERIAL PRIMARY KEY,
  page_id BIGINT NOT NULL REFERENCES pages(id) ON DELETE CASCADE,
  tag TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT page_tags_tag_not_blank_chk CHECK (btrim(tag) <> ''),
  CONSTRAINT page_tags_page_tag_uniq UNIQUE (page_id, tag)
);

CREATE INDEX IF NOT EXISTS idx_page_tags_tag ON page_tags (tag);
CREATE INDEX IF NOT EXISTS idx_pages_status ON pages (status);
CREATE INDEX IF NOT EXISTS idx_pages_search_vector ON pages USING GIN (search_vector);

CREATE OR REPLACE FUNCTION set_page_search_vector()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  NEW.search_vector :=
    setweight(to_tsvector('simple', coalesce(NEW.title, '')), 'A') ||
    setweight(to_tsvector('simple', coalesce(NEW.body_md, '')), 'B');
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_pages_set_search_vector ON pages;
CREATE TRIGGER trg_pages_set_search_vector
BEFORE INSERT OR UPDATE OF title, body_md
ON pages
FOR EACH ROW
EXECUTE FUNCTION set_page_search_vector();

UPDATE pages
SET search_vector =
  setweight(to_tsvector('simple', coalesce(title, '')), 'A') ||
  setweight(to_tsvector('simple', coalesce(body_md, '')), 'B')
WHERE search_vector IS NULL;

COMMIT;
