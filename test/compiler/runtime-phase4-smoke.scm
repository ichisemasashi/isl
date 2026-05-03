(add-load-path "./src")
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-runtime-error thunk label)
  (guard (e
          ((runtime-error? e) #t)
          ;; Phase 5: isl-conditions (e.g. <end-of-stream>) are also runtime errors
          ((with-module isl.core (isl-condition? e)) #t)
          (else (error "expected runtime-error" label e)))
    (thunk)
    (error "expected runtime-error but got value" label)))

;; ---- 4-B: print (stream-aware), write, terpri ----
(assert-equal 42     (run-forms '((print 42)))                            "print basic")

;; write returns the object
(assert-equal "hi"   (run-forms '((let ((s (open-output-stream "/tmp/isl-p4-write.txt")))
                                     (let ((r (write "hi" s)))
                                       (close s)
                                       r))))
              "write returns object")

;; terpri returns nil
(assert-equal '()    (run-forms '((let ((s (open-output-stream "/tmp/isl-p4-terpri.txt")))
                                     (let ((r (terpri s)))
                                       (close s)
                                       r))))
              "terpri returns nil")

;; ---- 4-A: write-char, read-char, peek-char ----
;; write-char to file and read back
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-chars.txt")))
                (write-char #\H s)
                (write-char #\i s)
                (close s))))

(assert-equal #\H    (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-chars.txt")))
                                     (let ((c (read-char s)))
                                       (close s)
                                       c))))
              "read-char basic")

;; peek-char does not consume
(assert-equal '(#\H #\H)
              (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-chars.txt")))
                              (let* ((p (peek-char s))
                                     (r (read-char s)))
                                (close s)
                                (list p r)))))
              "peek-char does not consume")

;; peek-char on empty stream returns nil (EOF)
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-empty.txt")))
                (close s))))
(assert-equal '()    (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-empty.txt")))
                                     (let ((r (peek-char s)))
                                       (close s)
                                       r))))
              "peek-char EOF returns nil")

;; read-char on empty stream raises runtime-error
(assert-runtime-error
  (lambda ()
    (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-empty.txt")))
                    (let ((r (read-char s)))
                      (close s)
                      r)))))
  "read-char EOF raises error")

;; write-char returns the character
(assert-equal #\Z    (run-forms '((let ((s (open-output-stream "/tmp/isl-p4-wc.txt")))
                                     (let ((c (write-char #\Z s)))
                                       (close s)
                                       c))))
              "write-char returns char")

;; ---- 4-C: open-input-stream, open-output-stream, close ----
;; open-output-stream creates/overwrites
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-overwrite.txt")))
                (write-char #\A s)
                (close s))))
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-overwrite.txt")))
                (write-char #\B s)
                (close s))))
(assert-equal #\B    (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-overwrite.txt")))
                                     (let ((c (read-char s)))
                                       (close s)
                                       c))))
              "open-output-stream supersedes")

;; close returns nil
(assert-equal '()    (run-forms '((let ((s (open-output-stream "/tmp/isl-p4-close.txt")))
                                     (close s))))
              "close returns nil")

;; ---- 4-D: input-stream-p, output-stream-p ----
(assert-equal #t     (run-forms '((input-stream-p *standard-input*)))    "input-stream-p standard-input")
(assert-equal #t     (run-forms '((output-stream-p *standard-output*)))  "output-stream-p standard-output")
(assert-equal #t     (run-forms '((output-stream-p *error-output*)))     "output-stream-p error-output")
(assert-equal '()    (run-forms '((input-stream-p 42)))                  "input-stream-p non-stream")
(assert-equal '()    (run-forms '((output-stream-p "foo")))              "output-stream-p non-stream")
(assert-equal #t     (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-chars.txt")))
                                     (let ((r (input-stream-p s)))
                                       (close s) r))))
              "input-stream-p file stream")

;; ---- 4-E: standard stream variables ----
(assert-equal #t     (run-forms '((input-stream-p *standard-input*)))    "standard-input bound")
(assert-equal #t     (run-forms '((output-stream-p *standard-output*)))  "standard-output bound")
(assert-equal #t     (run-forms '((output-stream-p *error-output*)))     "error-output bound")

;; ---- 4-F: read-line with 0/1/2 args ----
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-readline.txt")))
                (write-char #\h s) (write-char #\e s) (write-char #\l s)
                (write-char #\l s) (write-char #\o s)
                (close s))))

(assert-equal "hello"
              (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-readline.txt")))
                              (let ((line (read-line s)))
                                (close s)
                                line))))
              "read-line 1-arg")

;; read-line with eof-error-p = nil on EOF returns nil
(run-forms '((let ((s (open-output-stream "/tmp/isl-p4-rl-empty.txt")))
                (close s))))
(assert-equal '()
              (run-forms '((let ((s (open-input-stream "/tmp/isl-p4-rl-empty.txt")))
                              (let ((r (read-line s nil)))
                                (close s)
                                r))))
              "read-line eof-error-p nil returns nil")

(display "runtime Phase4 smoke passed\n")
