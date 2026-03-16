BEGIN;

ALTER TABLE pages
ADD COLUMN IF NOT EXISTS current_rev_no INTEGER NOT NULL DEFAULT 1;

UPDATE pages p
SET current_rev_no = sub.max_rev
FROM (
  SELECT page_id, COALESCE(MAX(rev_no), 1) AS max_rev
  FROM page_revisions
  GROUP BY page_id
) sub
WHERE p.id = sub.page_id;

UPDATE pages
SET current_rev_no = 1
WHERE current_rev_no IS NULL OR current_rev_no < 1;

ALTER TABLE pages
DROP CONSTRAINT IF EXISTS pages_current_rev_no_chk;

ALTER TABLE pages
ADD CONSTRAINT pages_current_rev_no_chk CHECK (current_rev_no > 0);

COMMIT;
