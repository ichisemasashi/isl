;;; core/reader.scm
;;; isl.core: ISLISP リーダ層と REPL
;;; このファイルは src/isl/core.scm から (include ...) され、
;;; モジュール isl.core の本体に展開される（単独ロード不可）。

;;; ----------------------------------------------------------------
;;; ISLISP reader layer
;;;
;;; Gauche's native reader does not understand the ISLISP/Common-Lisp
;;; function-quote syntax  #'foo  ==  (function foo).  We provide a thin
;;; wrapper reader that handles list structure, quote sugar and #', and
;;; delegates every other datum (numbers, strings, characters, vectors,
;;; #t/#f, charsets, ...) to Gauche's `read`, so the well-tested lexer
;;; for those is reused verbatim.
;;;
;;; Implementation: each source port is wrapped (once, cached as a port
;;; attribute) in a <virtual-input-port> backed by a pushback buffer.
;;; All character reads go through this virtual port so Gauche's own
;;; one-char unget composes correctly with our multi-char pushback.
;;; ----------------------------------------------------------------

(define (isl-delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\" #\; #\'))))

(define (make-isl-vport src)
  ;; Returns (vport . unread!) where unread! pushes a char so it is read next.
  (let ((buf '()))
    (let* ((getc (lambda ()
                   (if (pair? buf)
                       (let ((c (car buf))) (set! buf (cdr buf)) c)
                       (read-char src))))
           (vp (make <virtual-input-port> :getc getc))
           (un! (lambda (c) (set! buf (cons c buf)))))
      (cons vp un!))))

(define (isl-port-for src)
  ;; One persistent wrapper per source port (kept as a port attribute so
  ;; its lifetime follows the port and leftover lookahead survives across
  ;; successive single-datum reads, e.g. at the REPL).
  (if (port? src)
      (or (port-attribute-ref src 'isl-reader #f)
          (let ((p (make-isl-vport src)))
            (port-attribute-set! src 'isl-reader p)
            p))
      (make-isl-vport src)))

(define (isl-peek-char vp un!)
  (let ((c (read-char vp)))
    (if (eof-object? c) c (begin (un! c) c))))

(define (isl-skip-line vp)
  (let ((c (read-char vp)))
    (cond ((eof-object? c) #t)
          ((char=? c #\newline) #t)
          (else (isl-skip-line vp)))))

(define (isl-skip-block vp un!)
  ;; Opening #| already consumed; handle nested #| ... |#.
  (let loop ((depth 1))
    (let ((c (read-char vp)))
      (cond
       ((eof-object? c) (error "unterminated #| block comment"))
       ((char=? c #\#)
        (if (eqv? (isl-peek-char vp un!) #\|)
            (begin (read-char vp) (loop (+ depth 1)))
            (loop depth)))
       ((char=? c #\|)
        (if (eqv? (isl-peek-char vp un!) #\#)
            (begin (read-char vp) (if (= depth 1) #t (loop (- depth 1))))
            (loop depth)))
       (else (loop depth))))))

(define (isl-skip-ws vp un!)
  (let ((c (read-char vp)))
    (cond
     ((eof-object? c) #t)
     ((char-whitespace? c) (isl-skip-ws vp un!))
     ((char=? c #\;) (isl-skip-line vp) (isl-skip-ws vp un!))
     ((char=? c #\#)
      (let ((c2 (read-char vp)))
        (cond
         ((eof-object? c2) (un! #\#) #t)
         ((char=? c2 #\|) (isl-skip-block vp un!) (isl-skip-ws vp un!))
         ((char=? c2 #\;) (isl-read-1 vp un!) (isl-skip-ws vp un!))
         (else (un! c2) (un! #\#) #t))))
     (else (un! c) #t))))

(define (isl-read-list vp un! close)
  (let loop ((acc '()))
    (isl-skip-ws vp un!)
    (let ((c (read-char vp)))
      (cond
       ((eof-object? c) (error "unexpected EOF inside list"))
       ((char=? c close) (reverse acc))
       ((or (char=? c #\)) (char=? c #\])) (error "mismatched close paren" c))
       ((and (char=? c #\.)
             (isl-delimiter? (isl-peek-char vp un!)))
        ;; dotted tail:  (a b . c)
        (let ((tail (isl-read-1 vp un!)))
          (isl-skip-ws vp un!)
          (let ((cc (read-char vp)))
            (if (and (char? cc) (char=? cc close))
                (append-reverse acc tail)
                (error "malformed dotted list")))))
       (else (un! c)
             (loop (cons (isl-read-1 vp un!) acc)))))))

(define (isl-read-1 vp un!)
  (isl-skip-ws vp un!)
  (let ((c (read-char vp)))
    (cond
     ((eof-object? c) c)
     ((char=? c #\() (isl-read-list vp un! #\)))
     ((char=? c #\[) (isl-read-list vp un! #\]))
     ((or (char=? c #\)) (char=? c #\])) (error "unexpected close paren" c))
     ((char=? c #\') (list 'quote (isl-read-1 vp un!)))
     ((char=? c #\`) (list 'quasiquote (isl-read-1 vp un!)))
     ((char=? c #\,)
      (let ((c2 (read-char vp)))
        (if (and (char? c2) (char=? c2 #\@))
            (list 'unquote-splicing (isl-read-1 vp un!))
            (begin (when (char? c2) (un! c2))
                   (list 'unquote (isl-read-1 vp un!))))))
     ((char=? c #\#)
      (let ((c2 (read-char vp)))
        (cond
         ((eof-object? c2) (un! #\#) (read vp))
         ((char=? c2 #\') (list 'function (isl-read-1 vp un!)))
         (else (un! c2) (un! #\#) (read vp)))))
     (else (un! c) (read vp)))))

(define (isl-read src)
  (let ((p (isl-port-for src)))
    (isl-read-1 (car p) (cdr p))))

(define (read-all port)
  (let loop ((acc '()))
    (let ((x (isl-read port)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))

(define (repl env)
  (define (write-result x)
    (cond
     ((closure? x)
      (let ((name (closure-name x)))
        (if name
            (begin
              (display "#<function ")
              (display name)
              (display ">"))
            (display "#<closure>"))))
     ((primitive? x)
      (begin
        (display "#<primitive ")
        (display (primitive-name x))
        (display ">")))
     ((macro? x)
      (let ((name (macro-name x)))
        (if name
            (begin
              (display "#<macro ")
              (display name)
              (display ">"))
            (display "#<macro>"))))
     (else
      (write x))))
  (display "ISLISP> ")
  (flush)
  (let ((x (isl-read (current-input-port))))
    (if (eof-object? x)
        (begin
          (newline)
          'bye)
        (begin
          (guard (e (else
                     (display "Error: ")
                     (write e)
                     (newline)
                     (when *debug-mode*
                       (break-loop env (write-to-string e)))))
            (let ((result (eval-islisp (if (strict-profile?)
                                           (sanitize-for-strict x)
                                           x)
                                       env)))
              (write-result result)
              (newline)))
          (repl env)))))
