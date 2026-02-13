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
  '((defclass m7-a () ((v :initarg :v :reader m7-v)))
    (defglobal m7-o (make-instance 'm7-a :v 9))
    (m7-v m7-o))
  '((defclass m7-a2 () ((v :initarg :v :reader m7-v2)))
    (defglobal m7-o2 (make-instance 'm7-a2 :v 8))
    (defgeneric m7-g2 (x))
    (defmethod m7-g2 ((x m7-a2)) 'a)
    (m7-g2 m7-o2))
  '((defclass m7-a3 () ((v :initarg :v :reader m7-v3)))
    (defclass m7-b3 (m7-a3) ())
    (defglobal m7-ob3 (make-instance 'm7-b3 :v 3))
    (defgeneric m7-g3 (x))
    (defmethod m7-g3 ((x m7-a3)) 'a)
    (defmethod m7-g3 ((x m7-b3)) 'b)
    (m7-g3 m7-ob3))
  '((defclass m7-a4 () ((v :initarg :v)))
    (defglobal m7-o4 (make-instance 'm7-a4 :v 9))
    (slot-value m7-o4 'v))
  '((defclass m7-a5 () ())
    (defclass m7-b5 (m7-a5) ())
    (defglobal m7-ob5 (make-instance 'm7-b5))
    (defgeneric m7-h5 (x y))
    (defmethod m7-h5 ((x m7-a5) y) 'left)
    (m7-h5 m7-ob5 1))))

(display "runtime M7 oracle passed\n")
