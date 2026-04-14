(defun md2html-test-root ()
  (let ((v (getenv "MD2HTML_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(setenv "MD2HTML_ROOT" (md2html-test-root))

(load (string-append (md2html-test-root) "/examples/md2html/lib/errors.lsp"))
(load (string-append (md2html-test-root) "/examples/md2html/lib/ast.lsp"))
(load (string-append (md2html-test-root) "/examples/md2html/lib/escape.lsp"))
(load (string-append (md2html-test-root) "/examples/md2html/lib/lexer.lsp"))
(load (string-append (md2html-test-root) "/examples/md2html/lib/parser.lsp"))
(load (string-append (md2html-test-root) "/examples/md2html/lib/renderer.lsp"))

(defun assert-true (label pred)
  (if pred
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'md2html-assert-failed label))))

(defun assert-equal (label expected actual)
  (assert-true label (equal expected actual)))

(defun run-tests ()
  (let* ((pos (md-pos 1 1 0))
         (state (flush-para-state-rev
                 '()
                 '("third" "second" "first")
                 pos))
         (paragraph (first (first state))))
    (assert-true "flush-para-state-rev creates paragraph"
                 (eq (block-kind paragraph) 'block-paragraph))
    (assert-equal "flush-para-state-rev preserves original line order"
                  "<p>first\nsecond\nthird</p>"
                  (render-block paragraph)))

  (let* ((parsed (list (make-block-hr (md-pos 2 1 10))
                       '(rest-token)))
         (state (consume-parsed-block-rev '() parsed)))
    (assert-true "consume-parsed-block-rev stores block at front"
                 (eq (block-kind (first (first state))) 'block-hr))
    (assert-equal "consume-parsed-block-rev returns remaining tokens"
                  '(rest-token)
                  (second state)))

  (let ((blocks (parse-markdown "# Title\n\nParagraph line 1\nParagraph line 2\n")))
    (assert-equal "parse-markdown heading then paragraph count" 2 (length blocks))
    (assert-true "first block heading" (eq (block-kind (first blocks)) 'block-heading))
    (assert-true "second block paragraph" (eq (block-kind (second blocks)) 'block-paragraph)))

  (format t "t1-parser-unit: ok~%"))

(catch 'md2html-assert-failed
  (run-tests))
