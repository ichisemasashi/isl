# Security Review Issues

最終更新: 2026-03-25

対象:

- `isl` (interpreter / compiler)
- `examples/wiki`
- `examples/dbms`
- `examples/webserver`
- `examples/md2html`

## P1: md2html / wiki の XSS 防止

状態: 対応済み（既定で安全側）

内容:

- `md2html` が raw HTML をそのまま通していた
- `javascript:` などの危険な URL scheme をリンク/画像で許していた
- `wiki` はその HTML を本文としてそのまま表示していた

対応:

- raw HTML は既定でエスケープ表示
- 明示的に `MD2HTML_ALLOW_RAW_HTML=1` のときのみ raw HTML を通す
- 危険な URL scheme は `#unsafe-link` に置換

残課題:

- 許可制 HTML sanitizer を導入し、`iframe` など必要タグだけ通す設計にしたい
- `wiki` 側で CSP を段階導入したい

## P1: wiki の平文パスワード保存

状態: 対応済み（互換移行あり）

内容:

- `users.password_hash` が実質平文パスワードだった
- 初期 admin パスワードが固定だった

対応:

- 新規作成/更新パスワードはハッシュ化して保存
- 旧平文レコードは初回ログイン時に自動アップグレード
- 初期 admin パスワードは `WIKI_INITIAL_ADMIN_PASSWORD` を優先
- 未指定時は one-time password を `/tmp/isl-wiki-initial-admin-password.txt` に出力
- 互換目的で `WIKI_BOOTSTRAP_ALLOW_DEFAULT_ADMIN=1` のときだけ `admin` を許可

残課題:

- 独自 KDF ではなく PBKDF2 / bcrypt / Argon2 へ切り替えたい
- パスワード変更強制フラグを初期 admin に入れたい

## P1: webserver の埋め込み wiki CGI 起動コマンドの shell quote 漏れ

状態: 対応済み

内容:

- wiki 特例分岐で `root` や入出力パスをクォートせず shell 文字列を組み立てていた

対応:

- `cd`, `ISL_ROOT`, 実行ファイルパス, stdin/stdout パスをすべて `ws-shell-quote` 化

残課題:

- 可能なら shell 経由をやめ、引数配列ベースの実行 API に寄せたい

## P1: dbms の認証境界が弱い

状態: 未対応

内容:

- `DBMS_SESSION_USER` 未指定時に `admin` へフォールバックする
- 埋め込み側の認証漏れがそのまま管理者権限につながる

推奨対応:

- 未認証時は `anonymous` またはエラーにする
- `admin` 自動昇格を廃止する
- DBMS を単体利用する入口で明示ログインを必須化する

## P2: wiki の open redirect

状態: 対応済み

内容:

- `/login?next=...` の `next` が外部 URL を許していた

対応:

- `app-base` 配下のローカルパスだけを許可
- 改行混入も拒否

## P2: dbms の独自 KDF / 監査整合性ハッシュ

状態: 未対応

内容:

- password KDF と audit hash が独自実装
- 強度・検証容易性の面で標準暗号 primitives より弱い

推奨対応:

- password: Argon2id / bcrypt / PBKDF2-HMAC-SHA256
- audit: HMAC-SHA256 以上の keyed hash
- audit key は固定値ではなく環境または秘密情報ストアから供給

## P3: isl はサンドボックスではない

状態: 未対応（仕様明記が必要）

内容:

- `load`, `with-open-file`, `eval` により任意コード/任意ファイルアクセスが可能
- 未信頼コードの安全実行環境としては使えない

推奨対応:

- README / docs に「trusted code only」を明記
- 将来必要なら restricted runtime を別プロファイルで追加

## P3: wiki のセキュリティヘッダ強化

状態: 一部対応済み

内容:

- レスポンスに防御ヘッダが不足していた

対応:

- `X-Frame-Options: DENY`
- `X-Content-Type-Options: nosniff`
- `Referrer-Policy: same-origin`

残課題:

- CSP の導入
- `Secure` cookie の HTTPS 条件付き付与
