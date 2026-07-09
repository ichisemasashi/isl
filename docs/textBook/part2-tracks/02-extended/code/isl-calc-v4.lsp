;;; isl-calc 4周目: Web サービス版（extended プロファイル）
;;; 教科書 part2-tracks/02-extended/04-calc-as-web-service.md 本文の完成版
;;;
;;; 起動（既定の extended プロファイル。ネットワークは拡張機能）:
;;;   ./bin/isl docs/textBook/part2-tracks/02-extended/code/isl-calc-v4.lsp
;;; アクセス例（別端末で）:
;;;   curl --path-as-is 'http://127.0.0.1:18080/eval/(+%201%20(*%202%203))'
;;;   curl --path-as-is 'http://127.0.0.1:18080/eval/(/%201%200)'   -> 400
;;;
;;; 設計: 3周目の strict な評価コア（calc-parse / calc-try）はそのまま。
;;; その外側に extended の HTTP 外殻（ソケット・ファイルI/O）をかぶせる。

;;; ============================================================
;;; 評価コア（3周目 v3 と同じ。strict の範囲で書かれている）
;;; ============================================================
(defclass num-node   () ((value :initarg value :accessor node-value)))
(defclass var-node   () ((name  :initarg name  :accessor node-name)))
(defclass binop-node () ((op    :initarg op    :accessor node-op)
                         (lhs   :initarg lhs   :accessor node-lhs)
                         (rhs   :initarg rhs   :accessor node-rhs)))
(defclass let-node   () ((name  :initarg name  :accessor node-name)
                         (init  :initarg init  :accessor node-init)
                         (body  :initarg body  :accessor node-body)))

(defun calc-op->fn (op)
  (cond ((eq op '+) #'+) ((eq op '-) #'-)
        ((eq op '*) #'*) ((eq op '/) #'/)
        (t (error "unknown operator: ~A" op))))

(defun calc-parse (e)
  (cond ((numberp e) (create (class num-node) 'value e))
        ((symbolp e) (create (class var-node) 'name e))
        ((eq (car e) 'let)
         (let ((b (car (cadr e))))
           (create (class let-node)
                   'name (car b)
                   'init (calc-parse (cadr b))
                   'body (calc-parse (car (cddr e))))))
        (t (create (class binop-node)
                   'op  (car e)
                   'lhs (calc-parse (cadr e))
                   'rhs (calc-parse (car (cddr e)))))))

(defun calc-env-lookup (name env)
  (let ((pair (assoc name env)))
    (if pair (cdr pair) (error "unbound variable: ~A" name))))

(defgeneric calc-eval-node (node env))
(defmethod calc-eval-node ((n num-node) env) (node-value n))
(defmethod calc-eval-node ((n var-node) env) (calc-env-lookup (node-name n) env))
(defmethod calc-eval-node ((n binop-node) env)
  (funcall (calc-op->fn (node-op n))
           (calc-eval-node (node-lhs n) env)
           (calc-eval-node (node-rhs n) env)))
(defmethod calc-eval-node ((n let-node) env)
  (let ((v (calc-eval-node (node-init n) env)))
    (calc-eval-node (node-body n) (cons (cons (node-name n) v) env))))

(defun calc-eval (e) (calc-eval-node (calc-parse e) '()))

;; 文字列の式を評価して (ok 値) / (error 種別 …) を返す
(defun calc-try-string (str)
  (catch 'calc-done
    (with-handler
      (lambda (c)
        (throw 'calc-done
          (cond ((instancep c (class <division-by-zero>))
                 (list 'error 'division-by-zero (arithmetic-error-operands c)))
                ((instancep c (class <error>))
                 (list 'error 'other (condition-message c)))
                (t (list 'error 'unknown)))))
      (list 'ok (calc-eval (read (create-string-input-stream str)))))))

;;; ============================================================
;;; 履歴永続化（extended: ファイルI/O）
;;; ============================================================
(defglobal *calc-history* "docs/textBook/part2-tracks/02-extended/code/calc-history.txt")

(defun calc-save-entry (expr-str result)
  (with-open-file (out *calc-history*
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~A => ~S~%" expr-str result)))

;;; ============================================================
;;; HTTP 外殻（extended: ソケット）
;;; ============================================================

;; %XX を1文字に戻す最小 URL デコード（パス方式なので + はそのまま演算子）
(defun url-decode (s)
  (let ((out "") (i 0) (n (length s)))
    (while (< i n)
      (let ((ch (elt s i)))
        (cond ((char= ch #\%)
               (setq out (string-append
                          out (create-string
                               1 (integer->char (string->number (subseq s (+ i 1) (+ i 3)) 16)))))
               (setq i (+ i 3)))
              (t
               (setq out (string-append out (create-string 1 ch)))
               (setq i (+ i 1))))))
    out))

;; リクエスト行 "GET /eval/... HTTP/1.1" からパスを取り出す
(defun request-path (line)
  (let ((sp1 (string-index " " line)))
    (if sp1
        (let ((sp2 (string-index " " line (+ sp1 1))))
          (if sp2 (subseq line (+ sp1 1) sp2) '()))
        '()))) ;

;; パスが /eval/ で始まればデコードした式文字列を、そうでなければ nil を返す
(defun path->expr (path)
  (if (and (>= (length path) 6) (string= (subseq path 0 6) "/eval/"))
      (url-decode (subseq path 6 (length path)))
      '()))

;; ヘッダ行を空行まで読み飛ばす
(defun drain-headers (conn)
  (let ((line (tcp-receive-line conn '())))
    (while (and (stringp line) (> (length line) 0))
      (setq line (tcp-receive-line conn '())))))

;; HTTP レスポンスを組み立てて送る
(defun send-response (conn status body)
  (tcp-send conn
    (string-append "HTTP/1.1 " status "\r\n"
                   "Content-Type: text/plain; charset=utf-8\r\n"
                   "Content-Length: " (convert (length body) <string>) "\r\n"
                   "Connection: close\r\n\r\n"
                   body)))

;; 1接続を処理する
(defun handle-connection (conn)
  (let ((reqline (tcp-receive-line conn '())))
    (drain-headers conn)
    (if (stringp reqline)
        (let ((expr-str (path->expr (request-path reqline))))
          (if (stringp expr-str)
              (let ((r (calc-try-string expr-str)))
                (calc-save-entry expr-str r)
                (if (eq (car r) 'ok)
                    (send-response conn "200 OK"
                                   (format nil "~A => ~S~%" expr-str (cadr r)))
                    (send-response conn "400 Bad Request"
                                   (format nil "error: ~A : ~S~%" expr-str (car (cddr r))))))
              (send-response conn "404 Not Found" (format nil "use /eval/<expr>~%"))))
        nil)
    (tcp-close conn)))

;; サーバ本体：ポートを開いて accept ループ
(defun calc-serve (port)
  (let ((listener (tcp-listen port)))
    (format (standard-output) "isl-calc web service on http://127.0.0.1:~D/eval/<expr>~%" port)
    (while t
      (handle-connection (tcp-accept listener)))))

;;; --- 起動 ----------------------------------------------------------
(calc-serve 18080)
