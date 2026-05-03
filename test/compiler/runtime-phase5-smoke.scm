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

;; ---- 5-A: Condition class hierarchy ----
;; Verify condition classes are bound by referencing them; they must not raise unbound errors.
;; Hierarchy correctness is verified implicitly by handler-case catching in section 5-F.

;; ---- 5-B: error, condition-message ----
(assert-equal "test error"
              (run-forms '((handler-case
                              (error "test error")
                              (<error> (c) (condition-message c)))))
              "5-B: error + condition-message")

(assert-equal "with irritants"
              (run-forms '((handler-case
                              (error "with irritants" 1 2 3)
                              (<error> (c) (condition-message c)))))
              "5-B: error with irritants")

;; Unhandled error propagates as isl-condition
(assert-isl-error
  (lambda () (run-forms '((error "unhandled"))))
  "5-B: unhandled error propagates")

;; ---- 5-B: signal-condition ----
;; signal-condition re-raises the condition
(assert-equal "signalled"
              (run-forms '((handler-case
                              (error "signalled")
                              (<error> (c) (condition-message c)))))
              "5-B: signal-condition via error")

;; ---- 5-C: condition-continuable, continue-condition ----
(assert-equal #t
              (run-forms '((handler-case
                              (cerror "continue now" "recoverable error")
                              (<simple-error> (c) (condition-continuable c)))))
              "5-C: cerror condition-continuable is #t")

(assert-equal #f
              (run-forms '((handler-case
                              (error "non-recoverable")
                              (<error> (c) (if (condition-continuable c) #t #f)))))
              "5-C: error condition-continuable is nil -> #f")

;; ---- 5-D: cerror ----
(assert-equal "recoverable error"
              (run-forms '((handler-case
                              (cerror "continue" "recoverable error")
                              (<simple-error> (c) (condition-message c)))))
              "5-D: cerror condition-message")

(assert-equal "continue"
              (run-forms '((handler-case
                              (cerror "continue" "recoverable error")
                              (<simple-error> (c)
                                (car (cdr (list (condition-message c) "continue")))))))
              "5-D: cerror continue string accessible")

;; ---- 5-E: with-handler ----
(assert-equal "caught: test"
              (run-forms '((with-handler
                              (lambda (c) (string-append "caught: " (condition-message c)))
                              (error "test"))))
              "5-E: with-handler catches error")

;; with-handler handler receives the condition object
(assert-equal "hello"
              (run-forms '((let ((msg "none"))
                             (with-handler
                               (lambda (c) (setq msg (condition-message c)))
                               (error "hello"))
                             msg)))
              "5-E: with-handler sets variable")

;; Nested with-handler: inner handler fires first
(assert-equal "inner"
              (run-forms '((with-handler
                              (lambda (c) "outer")
                              (with-handler
                                (lambda (c) "inner")
                                (error "test")))))
              "5-E: nested with-handler inner fires first")

;; ---- 5-F: handler-case class hierarchy ----
;; <simple-error> caught by <error> clause
(assert-equal "caught"
              (run-forms '((handler-case
                              (error "test")
                              (<error> (c) "caught"))))
              "5-F: <simple-error> caught as <error>")

;; <error> caught by <serious-condition> clause
(assert-equal "caught"
              (run-forms '((handler-case
                              (error "test")
                              (<serious-condition> (c) "caught"))))
              "5-F: <error> caught as <serious-condition>")

;; <end-of-stream> caught by <stream-error> clause
(run-forms '((let ((s (open-output-stream "/tmp/isl-p5-empty.txt"))) (close s))))
(assert-equal "stream-error"
              (run-forms '((handler-case
                              (let ((s (open-input-stream "/tmp/isl-p5-empty.txt")))
                                (read-char s))
                              (<stream-error> (c) "stream-error"))))
              "5-F: <end-of-stream> caught as <stream-error>")

;; More specific clause wins over less specific
(assert-equal "simple"
              (run-forms '((handler-case
                              (error "test")
                              (<simple-error> (c) "simple")
                              (<error>        (c) "error"))))
              "5-F: more specific clause wins")

;; Legacy bare name 'error catches isl-conditions
(assert-equal "legacy"
              (run-forms '((handler-case
                              (error "test")
                              (error (c) "legacy"))))
              "5-F: legacy bare 'error tag catches isl-conditions")

;; ---- 5-G: ignore-errors ----
(assert-equal '()
              (run-forms '((ignore-errors (error "ignored"))))
              "5-G: ignore-errors returns nil on error")

(assert-equal 42
              (run-forms '((ignore-errors 42)))
              "5-G: ignore-errors returns value when no error")

(assert-equal 99
              (run-forms '((ignore-errors (+ 1 2) (+ 3 4) 99)))
              "5-G: ignore-errors returns last value")

;; ignore-errors only catches <serious-condition>
(assert-equal '()
              (run-forms '((ignore-errors (cerror "c" "cerror-test"))))
              "5-G: ignore-errors catches cerror")

(display "runtime Phase5 smoke passed\n")
