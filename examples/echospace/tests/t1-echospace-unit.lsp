(defun echospace-test-root ()
  (let ((v (getenv "ISL_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(setenv "ISL_ROOT" (echospace-test-root))
(setenv "ECHOSPACE_EMBED_MODE" "1")

(load (string-append (echospace-test-root) "/examples/echospace/app/echospace.lsp"))

(defun assert-true (label pred)
  (if pred
      t
      (error (string-append "assert failed: " label))))

(defun assert-equal (label expected actual)
  (assert-true label (equal expected actual)))

(defun run-tests ()
  (assert-equal "login route public"
                ""
                (echospace-route-required-role "GET" '("login")))
  (assert-equal "healthz route public"
                ""
                (echospace-route-required-role "GET" '("healthz")))
  (assert-equal "channel route requires member"
                "member"
                (echospace-route-required-role "GET" '("workspaces" "welcome" "channels" "general")))
  (assert-equal "admin route requires admin"
                "admin"
                (echospace-route-required-role "GET" '("admin")))
  (assert-equal "redirect target rejects external"
                (echospace-app-base)
                (echospace-safe-redirect-target "https://example.com/x"))
  (assert-equal "redirect target keeps local"
                "/echospace/workspaces/welcome/channels/general"
                (let ((saved (getenv "SCRIPT_NAME")))
                  (setenv "SCRIPT_NAME" "/echospace")
                  (let ((result (echospace-safe-redirect-target "/echospace/workspaces/welcome/channels/general")))
                    (setenv "SCRIPT_NAME" (if (null saved) "" saved))
                    result)))
  (assert-true "password hash validates"
               (let ((hash (echospace-password-hash "secret")))
                 (echospace-password-valid-p hash "secret")))
  (assert-true "password hash rejects wrong secret"
               (let ((hash (echospace-password-hash "secret")))
                 (not (echospace-password-valid-p hash "nope"))))
  (assert-equal "string sort ascending"
                '("Administrator" "Demo User")
                (mapcar (lambda (row) (first row))
                        (echospace-sort-by '(("Demo User") ("Administrator"))
                                           (lambda (row) (first row))
                                           nil)))
  (assert-equal "number sort descending"
                '(3 2 1)
                (echospace-sort-by '(3 1 2) (lambda (n) n) t))
  (assert-equal "int min works"
                2
                (echospace-int-min 2 5))
  (format t "t1-echospace-unit: ok~%"))

(run-tests)
