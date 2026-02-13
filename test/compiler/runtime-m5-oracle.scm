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
  '((block done (return-from done 42) 0))
  '((catch 'k (throw 'k 99) 0))
  '((throw 'no-target 1))
  '((block b (catch 'k (return-from b 7) 0) 1))
  '((defglobal m5-x 0)
    (tagbody
     start
     (setq m5-x (+ m5-x 1))
     (if (< m5-x 3) (go start) 'done))
    m5-x)))

(display "runtime M5 oracle passed\n")
