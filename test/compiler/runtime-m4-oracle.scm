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
  '((defglobal m4-log '())
    (defun m4-push (x) (setq m4-log (append m4-log (list x))) x)
    (+ (m4-push 1) (m4-push 2))
    m4-log)
  '((defglobal m4-x 0)
    (defun m4-a () (setq m4-x (+ m4-x 1)) m4-x)
    (defun m4-b () (setq m4-x (+ m4-x 10)) m4-x)
    (list (m4-a) (m4-b) m4-x))
  '((defglobal m4-v (vector 0 0))
    (defglobal m4-i 0)
    (defun m4-idx () (setq m4-i (+ m4-i 1)) 0)
    (setf (vector-ref m4-v (m4-idx)) 9)
    (list m4-i (vector-ref m4-v 0)))
  '((defglobal m4-log2 '())
    (defun m4-mark (x) (setq m4-log2 (append m4-log2 (list x))) x)
    (if (m4-mark #f) (m4-mark 'then) (m4-mark 'else))
    m4-log2)
  '((defglobal m4-c 0)
    (let ((x (setq m4-c (+ m4-c 1)))
          (y (setq m4-c (+ m4-c 10))))
      m4-c))))

(display "runtime M4 oracle passed\n")
