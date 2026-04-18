# echospace 要件定義 (v0.1)

この文書は `examples/echospace` の初期要件です。  
`echospace` は、Discord ライクな操作感を持つ Web チャットアプリとして実装します。  
本フェーズでは、まず MVP を明確化し、後方の拡張余地を残した設計を前提とします。

## 1. 目的

- ISL 製 Web アプリの実践例として、複数ユーザーが参加できる常駐型チャットアプリを提供する
- `examples/webserver` と組み合わせて動作する、状態を持つアプリケーション実装のサンプルにする
- 将来的な通知、スレッド、添付ファイル、検索などへ拡張しやすい土台を作る

## 2. 想定ユーザー

- リポジトリ利用者が ISL での Web アプリ実装例を学ぶ用途
- ローカルまたは小規模環境でチャットアプリを試したい開発者
- `webserver` / `dbms` / HTML UI を組み合わせたサンプルを必要とする保守者

## 3. MVP の範囲

MVP では以下を必須とする。

- ログイン / ログアウト
- ワークスペース一覧表示
- ワークスペース配下のテキストチャンネル一覧表示
- チャンネル内メッセージ一覧表示
- テキストメッセージ投稿
- ユーザー一覧表示
- 一定間隔のポーリングによる新着反映
- メッセージ / チャンネル / ユーザーの永続化

MVP では以下を対象外とする。

- 音声通話 / ビデオ通話
- 絵文字リアクション
- DM
- スレッド返信
- ファイルアップロード
- メンション通知
- 既読管理
- モバイルアプリ

## 4. 画面要件

アプリ全体は Discord ライクな 4 カラム構成を基本とする。

- 左カラム: ワークスペース一覧
- 第2カラム: 選択中ワークスペースのチャンネル一覧
- 中央: メッセージタイムライン
- 右カラム: 参加メンバー一覧

必須画面:

- `/echospace/login`
  - ユーザー名 / パスワード入力
- `/echospace`
  - 最後に見ていた、または既定のワークスペース / チャンネルへリダイレクト
- `/echospace/workspaces/{workspace_slug}/channels/{channel_slug}`
  - メッセージ一覧
  - 投稿フォーム
  - メンバー一覧
- `/echospace/admin`
  - ワークスペース / チャンネル / ユーザーの最低限の管理

## 5. 機能要件

### 5.1 認証

- ユーザーはログインして利用する
- セッション Cookie によりログイン状態を保持する
- 未認証ユーザーが保護画面へアクセスした場合は `/echospace/login` へ遷移する

### 5.2 ワークスペース

- ワークスペースは `name` と URL 用 `slug` を持つ
- ユーザーは 1 つ以上のワークスペースに所属できる
- MVP では公開招待機能は持たず、管理画面または初期データで所属を管理する

### 5.3 チャンネル

- チャンネルは特定ワークスペースに属する
- MVP ではテキストチャンネルのみ対応する
- チャンネルは `name`, `slug`, `topic`, `display_order` を持つ
- 各ワークスペースには少なくとも 1 つのチャンネルが存在する

### 5.4 メッセージ

- ユーザーは所属ワークスペースのチャンネルにメッセージ投稿できる
- メッセージは `author`, `body`, `created_at` を持つ
- メッセージ本文はプレーンテキストとして扱い、HTML はエスケープ表示する
- 改行は表示時に維持する
- 投稿順は `created_at` 昇順を基本とする
- MVP では編集 / 削除は管理者のみ許可、または未実装としてよい

### 5.5 メンバー一覧

- 右カラムに現在のワークスペース所属ユーザー一覧を表示する
- MVP ではオンライン状態は簡易表示とし、以下のいずれかでよい
  - 常に `online` 固定
  - 直近アクセス時刻ベースの擬似状態

### 5.6 新着反映

- MVP のリアルタイム性は短周期ポーリングで実現する
- クライアントは 3〜10 秒間隔で新着取得 API を呼び出す
- 追加分のみ取得できる API を提供する
- 将来の SSE / WebSocket 導入を阻害しない API / データ構造にする

### 5.7 管理機能

- 管理者は以下を行える
  - ユーザー作成
  - ワークスペース作成
  - チャンネル作成
  - 所属関係の管理
- 管理 UI は簡素でよい

## 6. 権限要件

ロールは最低 2 種類とする。

- `member`
  - 所属ワークスペースの閲覧
  - メッセージ投稿
- `admin`
  - `member` 権限に加えて管理画面操作

権限制約:

- 非所属ワークスペースは閲覧不可
- 非所属チャンネルへの投稿不可
- 管理 API は `admin` のみ利用可

## 7. API / ルーティング要件

初期案として以下のルートを持つ。

- `GET /echospace/login`
- `POST /echospace/login`
- `POST /echospace/logout`
- `GET /echospace`
- `GET /echospace/workspaces/{workspace_slug}/channels/{channel_slug}`
- `POST /echospace/workspaces/{workspace_slug}/channels/{channel_slug}/messages`
- `GET /echospace/api/workspaces/{workspace_slug}/channels/{channel_slug}/messages?after_id=...`
- `GET /echospace/api/workspaces/{workspace_slug}/members`
- `GET /echospace/admin`

API の返却形式:

- 画面表示は HTML
- 非同期更新用 API は JSON

## 8. データ要件

永続化は `examples/dbms` の利用を第一候補とする。  
MVP で必要な主テーブルは以下。

- `users`
  - `id`, `username`, `password_hash`, `display_name`, `role`, `created_at`
- `workspaces`
  - `id`, `slug`, `name`, `created_at`
- `workspace_members`
  - `id`, `workspace_id`, `user_id`, `created_at`
- `channels`
  - `id`, `workspace_id`, `slug`, `name`, `topic`, `display_order`, `created_at`
- `messages`
  - `id`, `workspace_id`, `channel_id`, `author_user_id`, `body`, `created_at`

初期データ要件:

- 管理者 1 名を自動生成できる
- サンプルワークスペース 1 件以上を用意できる
- 初期チャンネルとして `general` 相当を 1 件以上持つ

## 9. UI / UX 要件

- デスクトップ表示を優先する
- モバイルでは 1 カラムへ縮退できれば望ましいが、MVP では必須ではない
- 配色は Discord 風のダークトーンを参考にしてよいが、完全再現は不要
- 視認性の高いメッセージリスト、投稿欄、現在地表示を持つ
- ページ全体のリロードなしで新着更新と投稿結果反映ができることが望ましい

## 10. 非機能要件

### 10.1 性能

- 単一プロセス / 小規模利用を前提とする
- 1 ワークスペース数十人、1 チャンネル数千メッセージ程度を実用範囲とする
- 初回表示は過去メッセージを直近 `50` 件程度に制限してよい

### 10.2 セキュリティ

- パスワードは平文保存しない
- HTML / script の埋め込みは無効化する
- CSRF 対策を考慮する
- セッション Cookie には `HttpOnly` を付与する
- HTTPS 環境では `Secure` を付与できる設計にする

### 10.3 可観測性

- ログイン失敗、投稿失敗、権限拒否はログに残す
- 監視用のヘルスチェックを提供する

## 11. 運用要件

- `examples/webserver` から起動できる構成を優先する
- アプリ本体は常駐プロセス内で再利用される構成が望ましい
- 設定値は環境変数または専用設定ファイルから注入できるようにする

想定する主要設定:

- `ISL_ECHOSPACE_ROOT`
- `ISL_ECHOSPACE_DB_ROOT`
- `ISL_ECHOSPACE_SESSION_SECRET`
- `ISL_ECHOSPACE_POLL_INTERVAL_SEC`

## 12. テスト要件

- ルーティング単体テスト
- 認証 / 認可テスト
- メッセージ投稿テスト
- 新着取得 API テスト
- 最低限の画面統合テスト

## 13. 将来拡張

以下は MVP 後の拡張候補とする。

- SSE または WebSocket によるリアルタイム配信
- メッセージ編集 / 削除
- スレッド
- リアクション
- ファイル添付
- 検索
- DM
- 招待リンク
- 通知
- 未読管理

## 14. 実装方針メモ

- サーバーサイドレンダリングを基本とし、必要最小限の JavaScript で操作性を補う
- UI は HTML + CSS + 素朴な `fetch` ベース更新を第一候補とする
- ルーティングは `PATH_INFO` または `webserver` 側のアプリルータと整合する形で実装する
- まずは MVP を完了し、その後にリアルタイム性や管理機能を段階的に追加する

## 15. 未確定事項

以下は実装前に別途決める。

- `dbms` をそのまま使うか、`wiki` のストレージ構成を流用するか
- 認証基盤を `wiki` と共有するか、`echospace` 専用に持つか
- 管理 UI を HTML フォーム中心にするか、簡易 JSON API ベースにするか
- メッセージ編集 / 削除を MVP に入れるか
