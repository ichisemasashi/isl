(add-load-path "./src")
(use isl.compiler.frontend)
(use isl.compiler.codegen)

(define (assert-contains text needle label)
  (let ((n (string-length text))
        (m (string-length needle)))
    (let loop ((i 0))
      (cond
       ((> (+ i m) n)
        (error "assertion failed" label needle))
       ((string=? (substring text i (+ i m)) needle)
        #t)
       (else
        (loop (+ i 1)))))))

(define env (make-frontend-env 'strict))
(define units
  (frontend-compile-forms
   '((defun choose (x)
       (if x
           (+ 1 2)
           (+ 3 4)))
     (choose #t))
   env))

(define ll-text (ll-units->llvm-module units))

(assert-contains ll-text "define ptr @isl_fun_choose" "fun-define")
(assert-contains ll-text "br i1" "branch")
(assert-contains ll-text "phi ptr" "phi")
(assert-contains ll-text "@isl_rt_call" "call-runtime")
(assert-contains ll-text "declare ptr @isl_rt_make_int(i64)" "decl")

(display "codegen smoke passed\n")
