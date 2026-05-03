(add-load-path "./src")
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-isl-error thunk label)
  (guard (e
          ((runtime-error? e) #t)
          ((with-module isl.core (isl-condition? e)) #t)
          (else (error "expected isl-condition or runtime-error" label e)))
    (thunk)
    (error "expected error but got value" label)))

;; ---- 6-A: #f → nil sanitization ----
;; In extended mode, #f evaluates to #f (Scheme boolean).
(assert-equal #f
              (run-forms '(#f))
              "6-A: #f evaluates normally in extended mode")

;; In strict mode, defvar and dynamic work (strict mode compiles the same IR).
(assert-equal '*sv*
              (run-forms '((defvar *sv* 10)) 'strict)
              "6-A: defvar works in strict mode")

;; ---- 6-B: defvar / dynamic / dynamic-let ----

;; Basic defvar
(assert-equal '*my-var*
              (run-forms '((defvar *my-var* 42)))
              "6-B: defvar returns variable name")

;; defvar value accessible via dynamic
(assert-equal 42
              (run-forms '((defvar *dv1* 42)
                           (dynamic *dv1*)))
              "6-B: dynamic reads defvar value")

;; defvar does not rebind if already bound
(assert-equal 42
              (run-forms '((defvar *dv2* 42)
                           (defvar *dv2* 99)   ; should be ignored
                           (dynamic *dv2*)))
              "6-B: defvar idempotent - second defvar ignored")

;; dynamic-let temporarily rebinds
(assert-equal 99
              (run-forms '((defvar *dv3* 42)
                           (dynamic-let ((*dv3* 99))
                             (dynamic *dv3*))))
              "6-B: dynamic-let provides temporary rebinding")

;; After dynamic-let exits, original value is restored
(assert-equal 42
              (run-forms '((defvar *dv4* 42)
                           (dynamic-let ((*dv4* 99))
                             (dynamic *dv4*))
                           (dynamic *dv4*)))
              "6-B: dynamic value restored after dynamic-let")

;; nested dynamic-let
(assert-equal 100
              (run-forms '((defvar *dv5* 1)
                           (dynamic-let ((*dv5* 10))
                             (dynamic-let ((*dv5* 100))
                               (dynamic *dv5*)))))
              "6-B: nested dynamic-let inner wins")

;; After nested dynamic-let, correct restoration
(assert-equal 10
              (run-forms '((defvar *dv6* 1)
                           (dynamic-let ((*dv6* 10))
                             (dynamic-let ((*dv6* 100))
                               (dynamic *dv6*))
                             (dynamic *dv6*))))
              "6-B: nested dynamic-let restores intermediate value")

;; dynamic-let with multiple bindings
(assert-equal '(20 30)
              (run-forms '((defvar *da* 1)
                           (defvar *db* 2)
                           (dynamic-let ((*da* 20) (*db* 30))
                             (list (dynamic *da*) (dynamic *db*)))))
              "6-B: dynamic-let multiple bindings")

;; dynamic on unbound variable raises error
(assert-isl-error
  (lambda () (run-forms '((dynamic *unbound-dyn*))))
  "6-B: dynamic on unbound var raises error")

;; dynamic-let body returns last value
(assert-equal 99
              (run-forms '((defvar *dv7* 0)
                           (dynamic-let ((*dv7* 99))
                             (+ 1 2)
                             (* 3 4)
                             (dynamic *dv7*))))
              "6-B: dynamic-let returns last body expression")

(display "runtime Phase6 smoke passed\n")
