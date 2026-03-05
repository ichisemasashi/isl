;; dbms/app/main.lsp
;; DBMS CLIエントリポイント。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/profile.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/sql_parser.lsp")

(defun dbms-read-stdin-text ()
  (with-open-file (s "/dev/stdin" :direction :input)
    (let ((line (read-line s #f))
          (acc "")
          (first-line t))
      (while (not (null line))
        (if first-line
            (progn
              (setq acc line)
              (setq first-line nil))
            (setq acc (string-append acc "\n" line)))
        (setq line (read-line s #f)))
      acc)))

(defun dbms-repeat-char (ch n)
  (let ((i 0)
        (out ""))
    (while (< i n)
      (setq out (string-append out ch))
      (setq i (+ i 1)))
    out))

(defun dbms-value->display-string (v)
  (if (eq v 'NULL)
      "NULL"
      (if (eq v t)
          "TRUE"
          (if (null v)
              "FALSE"
              (format nil "~A" v)))))

(defun dbms-max (a b)
  (if (> a b) a b))

(defun dbms-list-ref (xs idx)
  (if (= idx 0)
      (car xs)
      (dbms-list-ref (cdr xs) (- idx 1))))

(defun dbms-list-set (xs idx value)
  (if (= idx 0)
      (cons value (cdr xs))
      (cons (car xs) (dbms-list-set (cdr xs) (- idx 1) value))))

(defun dbms-string-widths-for-rows (cols rows)
  (let ((widths '())
        (i 0)
        (n (length cols)))
    (while (< i n)
      (setq widths (append widths (list (length (dbms-list-ref cols i)))))
      (setq i (+ i 1)))
    (dolist (row rows)
      (setq i 0)
      (while (< i n)
        (let* ((cell (dbms-value->display-string (dbms-list-ref row i)))
               (w (length cell))
               (old (dbms-list-ref widths i)))
          (setq widths (dbms-list-set widths i (dbms-max old w))))
        (setq i (+ i 1))))
    widths))

(defun dbms-pad-right (s width)
  (let ((pad (- width (length s))))
    (if (<= pad 0)
        s
        (string-append s (dbms-repeat-char " " pad)))))

(defun dbms-print-separator (widths)
  (let ((out "+"))
    (dolist (w widths)
      (setq out (string-append out (dbms-repeat-char "-" (+ w 2)) "+")))
    (format t "~A~%" out)))

(defun dbms-print-row (cells widths)
  (let ((out "|")
        (i 0))
    (while (< i (length cells))
      (setq out
            (string-append out
                           " "
                           (dbms-pad-right (dbms-value->display-string (dbms-list-ref cells i))
                                           (dbms-list-ref widths i))
                           " |"))
      (setq i (+ i 1)))
    (format t "~A~%" out)))

(defun dbms-cli-print-rows (payload)
  (let* ((cols (first payload))
         (rows (second payload))
         (widths (dbms-string-widths-for-rows cols rows)))
    (dbms-print-separator widths)
    (dbms-print-row cols widths)
    (dbms-print-separator widths)
    (dolist (row rows)
      (dbms-print-row row widths))
    (dbms-print-separator widths)
    (format t "~A rows.~%" (length rows))))

(defun dbms-cli-print-error (err)
  (format t "ERROR ~A: ~A~%" (second err) (third err))
  (if (null (fourth err))
      nil
      (format t "DETAIL: ~A~%" (fourth err))))

(defun dbms-cli-print-result (result)
  (if (not (dbms-result-p result))
      (format t "~A~%" result)
      (let ((kind (second result))
            (payload (third result)))
        (if (eq kind 'rows)
            (dbms-cli-print-rows payload)
            (if (eq kind 'count)
                (format t "affected rows: ~A~%" payload)
                (if (eq kind 'ok)
                    (format t "ok~%")
                    (if (eq kind 'error)
                        (dbms-cli-print-error payload)
                        (format t "~A~%" result))))))))

(defun dbms-cli-parse-and-exec (catalog sql-text)
  (let ((ast (dbms-parse-sql sql-text)))
    (if (dbms-error-p ast)
        (dbms-make-result 'error ast)
        (let ((stmts (dbms-ast-statements ast)))
          (if (or (null stmts) (not (= (length stmts) 1)))
              (dbms-make-result-error 'dbms/parse-error "single statement required" sql-text)
              (dbms-engine-dispatch catalog (first stmts)))))))

(defun main ()
  (let* ((catalog (dbms-engine-init))
         (sql-text (dbms-read-stdin-text))
         (result (if (string= sql-text "")
                     (dbms-make-result-error 'dbms/no-input "stdin is empty" '())
                     (dbms-cli-parse-and-exec catalog sql-text))))
    (dbms-cli-print-result result)))

(main)
