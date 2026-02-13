(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(assert-equal
 '(1 2)
 (run-forms
  '((defglobal m4-log '())
    (defun m4-push (x) (setq m4-log (append m4-log (list x))) x)
    (+ (m4-push 1) (m4-push 2))
    m4-log)
  'strict)
 "left-to-right-call-args")

(assert-equal
 '(1 11 11)
 (run-forms
  '((defglobal m4-x 0)
    (defun m4-a () (setq m4-x (+ m4-x 1)) m4-x)
    (defun m4-b () (setq m4-x (+ m4-x 10)) m4-x)
    (list (m4-a) (m4-b) m4-x))
  'strict)
 "global-side-effect-order")

(assert-equal
 '(1 9)
 (run-forms
  '((defglobal m4-v (vector 0 0))
    (defglobal m4-i 0)
    (defun m4-idx () (setq m4-i (+ m4-i 1)) 0)
    (setf (vector-ref m4-v (m4-idx)) 9)
    (list m4-i (vector-ref m4-v 0)))
  'strict)
 "setf-eval-once-order")

(assert-equal
 '(#f else)
 (run-forms
  '((defglobal m4-log2 '())
    (defun m4-mark (x) (setq m4-log2 (append m4-log2 (list x))) x)
    (if (m4-mark #f) (m4-mark 'then) (m4-mark 'else))
    m4-log2)
  'strict)
 "if-branch-side-effect")

(assert-equal
 11
 (run-forms
  '((defglobal m4-c 0)
    (let ((x (setq m4-c (+ m4-c 1)))
          (y (setq m4-c (+ m4-c 10))))
      m4-c))
  'strict)
 "let-init-left-to-right")

(display "runtime M4 smoke passed\n")
