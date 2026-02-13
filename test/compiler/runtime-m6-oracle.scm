(add-load-path "./src")
(use isl.core)
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define ienv (make-initial-env 'strict))

(define (eval-interpreter forms)
  (guard (e (else (list 'error e)))
    (list 'ok (eval-islisp (cons 'progn forms) ienv))))

(define (eval-runtime forms)
  (guard (e (else (list 'error e)))
    (list 'ok (run-forms forms 'strict))))

(define (same-result? a b)
  (cond
   ((and (eq? (car a) 'ok) (eq? (car b) 'ok))
    (equal? (cadr a) (cadr b)))
   ((and (eq? (car a) 'error) (eq? (car b) 'error))
    #t)
   (else #f)))

(define (check-case forms)
  (let ((ia (eval-interpreter forms))
        (rb (eval-runtime forms)))
    (unless (same-result? ia rb)
      (error "mismatch" forms ia rb))))

(for-each
 check-case
 (list
  '((handler-case (/ 1 0) (error (c) 'caught)))
  '((handler-case (+ 1 2) (error (c) 'caught)))
  '((handler-case (progn (car 1) 'ng) (error (c) 'ok)))
  '((handler-case (handler-case (car 1) (error (c) (throw 'm6 5)))
                  (error (c) 'outer)))
  '((catch 'm6 (handler-case (car 1) (error (c) (throw 'm6 'via-handler)))))))

(display "runtime M6 oracle passed\n")
