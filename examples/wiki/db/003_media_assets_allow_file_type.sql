BEGIN;

ALTER TABLE media_assets DROP CONSTRAINT IF EXISTS media_assets_type_chk;

ALTER TABLE media_assets
  ADD CONSTRAINT media_assets_type_chk
  CHECK (media_type IN ('image', 'video', 'audio', 'file'));

COMMIT;
