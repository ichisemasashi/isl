(add-load-path "./src")
(use isl.core)
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define ienv (make-initial-env 'strict))

(define (eval-interpreter form)
  (guard (e (else (list 'error e)))
    (list 'ok (eval-islisp form ienv))))

(define (eval-runtime form)
  (guard (e (else (list 'error e)))
    (list 'ok (run-forms (list form) 'strict))))

(define (same-result? a b)
  (cond
   ((and (eq? (car a) 'ok) (eq? (car b) 'ok))
    (equal? (cadr a) (cadr b)))
   ((and (eq? (car a) 'error) (eq? (car b) 'error))
    #t)
   (else #f)))

(define (check-case form)
  (let ((ia (eval-interpreter form))
        (rb (eval-runtime form)))
    (unless (same-result? ia rb)
      (error "mismatch" form ia rb))))

(for-each
 check-case
 (list
  '((lambda (x)
      ((lambda (f) (funcall f 5))
       (lambda (y) (+ x y))))
    10)
  '((lambda (a &rest r) (list a r)) 1 2 3 4)
  '((lambda (x)
      (progn
        ((lambda (x) x) 9)
        x))
    1)
  '((lambda (x y) x) 1)))

(display "runtime M2 oracle passed\n")
