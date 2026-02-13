(add-load-path "./src")
(use isl.core)
(use isl.compiler.runner)

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
  '((zerop 0))
  '((list (plusp 1) (minusp -1) (oddp 3) (evenp 4)))
  '((list (floor 7) (ceiling 7) (truncate 7) (round 7)))
  '((+ 9007199254740991 1))
  '((< "a" 1))
  '((oddp 3.5))
  '((mod 7 3))
  '((mod 7.0 3))))

(display "runtime M8 oracle passed\n")
