(define-module isl.main
  (use isl.core)
  (export main))

(select-module isl.main)

(define (usage)
  (display "Usage: isl [--profile strict|extended] [file ...]\n"))

(define (starts-with? s prefix)
  (let ((n (string-length s))
        (m (string-length prefix)))
    (and (>= n m)
         (string=? (substring s 0 m) prefix))))

(define (option-arg? arg)
  (and (> (string-length arg) 0)
       (char=? (string-ref arg 0) #\-)))

(define (parse-profile-value value)
  (cond
   ((string-ci=? value "strict") (list 'ok 'strict))
   ((string-ci=? value "extended") (list 'ok 'extended))
   (else
    (list 'error
          (string-append "unknown profile: "
                         value
                         " (expected strict or extended)")))))

(define (parse-profile-option value rest files)
  (let ((parsed (parse-profile-value value)))
    (if (eq? (car parsed) 'ok)
        (parse-main-args-loop rest (cadr parsed) files)
        parsed)))

(define (eval-forms forms env)
  (for-each (lambda (form)
              (eval-islisp form env))
            forms))

(define (run-file path env)
  (guard (e
          (else
           (error "Failed to load file" path e)))
    (call-with-input-file path
      (lambda (p)
        (eval-forms (read-all p) env)))))

(define (parse-main-args-loop xs profile files)
    (if (null? xs)
        (list 'ok profile (reverse files))
        (let ((arg (car xs)))
          (cond
           ((or (string=? arg "-h") (string=? arg "--help"))
            (list 'help))
           ((string=? arg "--")
            (list 'ok profile (append (reverse files) (cdr xs))))
           ((string=? arg "--profile")
            (if (null? (cdr xs))
                (list 'error "--profile requires a value: strict or extended")
                (parse-profile-option (cadr xs) (cddr xs) files)))
           ((starts-with? arg "--profile=")
            (parse-profile-option (substring arg 10 (string-length arg))
                                  (cdr xs)
                                  files))
           ((and (option-arg? arg) (not (string=? arg "-")))
            (list 'error (string-append "unknown option: " arg)))
           (else
            (parse-main-args-loop (cdr xs) profile (cons arg files)))))))

(define (parse-main-args argv)
  (parse-main-args-loop argv 'extended '()))

(define (run-files files env)
  (for-each (lambda (path)
              (run-file path env))
            files))

(define (main args)
  (let ((parsed (parse-main-args (cdr args))))
    (case (car parsed)
      ((help)
       (usage)
       0)
      ((error)
       (usage)
       (display "Error: ")
       (display (cadr parsed))
       (newline)
       2)
      ((ok)
       (let* ((profile (cadr parsed))
              (files (caddr parsed))
              (env (make-initial-env profile)))
         (if (null? files)
             (begin
               (display "ISLISP interpreter (Gauche 0.9.15)\n")
               (display "Profile: ")
               (display profile)
               (newline)
               (display "Ctrl-D to exit.\n")
               (repl env)
               0)
             (begin
               (run-files files env)
               0))))
      (else
       (usage)
       2))))
