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
   '((defglobal x 10)
     (defun fact (n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))
     (print (fact x)))
   env))

(define aot-text (ll-units->llvm-aot-module units))

(assert-contains aot-text "define i32 @main()" "has-main")
(assert-contains aot-text "@isl_rt_create_env" "has-create-env")
(assert-contains aot-text "@isl_rt_make_compiled_fun" "has-fn-register")
(assert-contains aot-text "call void @isl_rt_install_primitives" "has-primitive-init")

(display "aot smoke passed\n")
