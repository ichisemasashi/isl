(add-load-path "./src")
(use isl.main)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-true actual label)
  (unless actual
    (error "assertion failed" label actual)))

(define (parse argv)
  (with-module isl.main
    (parse-main-args argv)))

(assert-equal
 '(ok extended ("sample.lsp"))
 (parse '("sample.lsp"))
 "default-profile")

(assert-equal
 '(ok strict ("sample.lsp"))
 (parse '("--profile" "strict" "sample.lsp"))
 "profile-strict")

(assert-equal
 '(ok extended ())
 (parse '("--profile=EXTENDED"))
 "profile-extended-case-insensitive")

(assert-equal
 '(error "--profile requires a value: strict or extended")
 (parse '("--profile"))
 "profile-missing-value")

(assert-equal
 '(error "unknown profile: fast (expected strict or extended)")
 (parse '("--profile=fast"))
 "profile-invalid-value")

(assert-equal
 '(error "unknown option: --bogus")
 (parse '("--bogus"))
 "unknown-option")

(assert-equal
 '(ok extended ("--bogus" "program.lsp"))
 (parse '("--" "--bogus" "program.lsp"))
 "option-terminator")

(let ((raised #f))
  (guard (e
          (else
           (set! raised #t)
           (assert-true
            (not (eq? #f (string-scan (write-to-string e) "does-not-exist.lsp")))
            "run-file-error-includes-path")))
    (with-module isl.main
      (run-file "test/does-not-exist.lsp" #f))
    (error "expected missing file error")))

(display "main smoke passed\n")
