(add-load-path "./src")
(use isl.core)
(use isl.compiler.jit)

(define ienv (make-initial-env 'strict))
(define jst (make-jit-state 'strict))

(define (write-to-string* x)
  (let ((p (open-output-string)))
    (write x p)
    (get-output-string p)))

(define (trim-right-newline s)
  (let ((n (string-length s)))
    (if (and (> n 0) (char=? (string-ref s (- n 1)) #\newline))
        (substring s 0 (- n 1))
        s)))

(define (starts-with? s prefix)
  (let ((n (string-length s))
        (m (string-length prefix)))
    (and (>= n m)
         (string=? (substring s 0 m) prefix))))

(define (last-line s)
  (let loop ((i 0) (start 0) (last ""))
    (if (>= i (string-length s))
        (if (string=? last "") (substring s start i) last)
        (if (char=? (string-ref s i) #\newline)
            (loop (+ i 1) (+ i 1) (substring s start i))
            (loop (+ i 1) start last)))))

(define (eval-interpreter forms)
  (guard (e (else '(error)))
    (list 'ok (eval-islisp (cons 'progn forms) ienv))))

(define (eval-jit forms)
  (guard (e (else '(error)))
    (list 'ok (jit-eval-forms jst forms))))

(define (eval-aot forms)
  (let* ((tag (symbol->string (gensym "m11aot")))
         (src (string-append "/tmp/" tag ".lsp"))
         (bin (string-append "/tmp/" tag ".bin"))
         (out (string-append "/tmp/" tag ".out"))
         (init (if (null? (cdr forms)) '() (reverse (cdr (reverse forms)))))
         (lastf (car (reverse forms))))
    (dynamic-wind
      (lambda ()
        (call-with-output-file src
          (lambda (p)
            (for-each (lambda (f) (write f p) (newline p)) init)
            (write (list 'print lastf) p)
            (newline p))
          :if-exists :supersede))
      (lambda ()
        (let ((c1 (sys-system (string-append "./bin/islc-aot --profile strict --opt-preset safe -o "
                                             bin
                                             " "
                                             src
                                             " >/dev/null 2>&1"))))
          (if (not (= c1 0))
              '(error)
              (begin
                (sys-system (string-append bin " > " out " 2>&1"))
                (let* ((txt (call-with-input-file out
                              (lambda (p)
                                (let loop ((acc ""))
                                  (let ((line (read-line p #f)))
                                    (if (eof-object? line)
                                        acc
                                        (loop (string-append acc line "\n")))))))
                            )
                       (line (trim-right-newline (last-line txt))))
                  (if (or (starts-with? line "#<error")
                          (string=? line ""))
                      '(error)
                      (list 'ok line)))))))
      (lambda ()
        (sys-system (string-append "rm -f " src " " bin " " out))))))

(define (same? ij aot)
  (cond
   ((and (eq? (car ij) 'error) (eq? (car aot) 'error)) #t)
   ((and (eq? (car ij) 'ok) (eq? (car aot) 'ok))
    (equal? (write-to-string* (cadr ij)) (cadr aot)))
   (else #f)))

(define (check-case forms)
  (let ((iv (eval-interpreter forms))
        (jv (eval-jit forms))
        (av (eval-aot forms)))
    (unless (equal? iv jv)
      (error "interpreter/jit mismatch" forms iv jv))
    (unless (same? iv av)
      (error "interpreter/jit/aot mismatch" forms iv jv av))))

;; AOT-supported subset for three-way oracle.
(for-each
 check-case
 (list
  '((defun m11-fib (n)
      (if (< n 2) n (+ (m11-fib (- n 1)) (m11-fib (- n 2)))))
    (m11-fib 10))
  '((defun m11-err () (car 1))
    (m11-err))))

(display "jit M11 triple oracle passed\n")
