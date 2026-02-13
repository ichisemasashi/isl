(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(assert-equal
 9
 (run-forms
  '((defclass m7-a () ((v :initarg :v :reader m7-v)))
    (defglobal m7-o (make-instance 'm7-a :v 9))
    (m7-v m7-o))
  'strict)
 "reader-generated")

(assert-equal
 'a
 (run-forms
  '((defclass m7-a2 () ((v :initarg :v :reader m7-v2)))
    (defglobal m7-o2 (make-instance 'm7-a2 :v 8))
    (defgeneric m7-g2 (x))
    (defmethod m7-g2 ((x m7-a2)) 'a)
    (m7-g2 m7-o2))
  'strict)
 "single-dispatch")

(assert-equal
 'b
 (run-forms
  '((defclass m7-a3 () ((v :initarg :v :reader m7-v3)))
    (defclass m7-b3 (m7-a3) ())
    (defglobal m7-ob3 (make-instance 'm7-b3 :v 3))
    (defgeneric m7-g3 (x))
    (defmethod m7-g3 ((x m7-a3)) 'a)
    (defmethod m7-g3 ((x m7-b3)) 'b)
    (m7-g3 m7-ob3))
  'strict)
 "most-specific")

(assert-equal
 9
 (run-forms
  '((defclass m7-a4 () ((v :initarg :v)))
    (defglobal m7-o4 (make-instance 'm7-a4 :v 9))
    (slot-value m7-o4 'v))
  'strict)
 "slot-value")

(assert-equal
 'left
 (run-forms
  '((defclass m7-a5 () ())
    (defclass m7-b5 (m7-a5) ())
    (defglobal m7-ob5 (make-instance 'm7-b5))
    (defgeneric m7-h5 (x y))
    (defmethod m7-h5 ((x m7-a5) y) 'left)
    (m7-h5 m7-ob5 1))
  'strict)
 "multi-arg-method")

(display "runtime M7 smoke passed\n")
