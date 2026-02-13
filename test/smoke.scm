(add-load-path "./src")
(use isl.core)

(define env (make-initial-env))

(define (check expected form)
  (let ((actual (eval-islisp form env)))
    (unless (equal? expected actual)
      (error "assertion failed" form expected actual))))

(define (check-error form)
  (guard (e
          (else #t))
    (let ((actual (eval-islisp form env)))
      (error "assertion failed (expected error)" form actual))))

(define smoke-file "test/tmp-smoke-input.lsp")
(call-with-output-file
 smoke-file
 (lambda (p)
   (write '(alpha beta gamma) p))
 :if-exists :supersede)

(define load-file "test/tmp-smoke-load.lsp")
(call-with-output-file
 load-file
 (lambda (p)
   (write '(defglobal loaded-from-file 314) p)
   (newline p)
   (write 'loaded-from-file p))
 :if-exists :supersede)

(define text-file "test/tmp-smoke-lines.txt")
(call-with-output-file
 text-file
 (lambda (p)
   (display "line-1\nline-2\n" p))
 :if-exists :supersede)

(define out-file "test/tmp-smoke-output.txt")
(define delete-file-target "test/tmp-smoke-delete.txt")
(define sqlite-file "test/tmp-smoke-db.sqlite")
(call-with-output-file
 delete-file-target
 (lambda (p)
   (display "to-delete\n" p))
 :if-exists :supersede)
(when (file-exists? sqlite-file)
  (sys-unlink sqlite-file))

(check 3 '(+ 1 2))
(check #(1 2 3) '(make-array 3 :initial-contents '(1 2 3)))
(check #(1 2 3) '(make-vector 3 :initial-contents '(1 2 3)))
(check #\x '(vector-ref (make-vector 3 :initial-element #\x) 2))
(check 1 '(aref (make-array 3 :initial-contents '(1 2 3)) 0))
(check 3 '(vector-ref (vector 1 2 3) 2))
(check 9 '(progn
            (defglobal vv (make-vector 3 0))
            (vector-set! vv 1 9)
            (vector-ref vv 1)))
(check 7 '(progn
            (defglobal vv2 (vector 1 2 3))
            (setf (vector-ref vv2 0) 7)
            (vector-ref vv2 0)))
(check 8 '(progn
            (setf (aref vv2 1) 8)
            (aref vv2 1)))
(check #t '(vectorp (vector 1 2)))
(check #f '(vectorp '(1 2)))
(check 10 '(progn
             (defglobal ht1 (make-hash-table))
             (puthash 'a 10 ht1)
             (gethash 'a ht1)))
(check '() '(gethash 'b ht1))
(check 99 '(gethash 'b ht1 99))
(check 20 '(progn
             (setf (gethash 'b ht1) 20)
             (gethash 'b ht1)))
(check #t '(hash-table-p ht1))
(check 2 '(hash-table-count ht1))
(check #t '(remhash 'a ht1))
(check 1 '(hash-table-count ht1))
(check 0 '(progn (clrhash ht1) (hash-table-count ht1)))
(check 1 '(progn
            (defglobal ht2 (make-hash-table :test 'equal))
            (puthash "key" 1 ht2)
            (gethash "key" ht2)))
(check 314 `(load ,load-file))
(check 314 'loaded-from-file)
(check '("line-1" "line-2")
       `(with-open-file (stream ,text-file :direction :input)
          (let ((line (read-line stream nil))
                (acc '()))
            (while line
              (setq acc (append acc (list line)))
              (setq line (read-line stream nil)))
            acc)))
(check "written"
       `(progn
          (with-open-file (stream ,out-file :direction :output :if-exists :overwrite)
            (write-line "This is a test" stream))
          "written"))
(check out-file `(probe-file ,out-file))
(check '() '(probe-file "test/definitely-not-found-file-xyz.txt"))
(check #t `(delete-file ,delete-file-target))
(check '() `(probe-file ,delete-file-target))
(check #f '(debug))
(check #t '(progn (debug t) (debug)))
(check #f '(progn (debug nil) (debug)))
(check "smoke1" '(progn
                   (setenv "ISL_SMOKE_ENV" "smoke1")
                   (getenv "ISL_SMOKE_ENV")))
(check "smoke1" '(progn
                   (setenv "ISL_SMOKE_ENV" "smoke2" nil)
                   (getenv "ISL_SMOKE_ENV")))
(check "smoke3" '(progn
                   (setenv "ISL_SMOKE_ENV" "smoke3" t)
                   (getenv "ISL_SMOKE_ENV")))
(check 0 '(system "sh -c 'exit 0'"))
(check 7 '(system "sh -c 'exit 7'"))
(check #t '(if (= (system "sh -c 'command -v sqlite3 >/dev/null 2>&1'") 0) #t #f))
(when (= (eval-islisp '(system "sh -c 'command -v sqlite3 >/dev/null 2>&1'") env) 0)
  (check #t `(progn
               (defglobal db1 (sqlite-open ,sqlite-file))
               (sqlite-db-p db1)))
  (check #t '(sqlite-exec db1 "create table if not exists items(id integer primary key, name text)"))
  (check #t '(sqlite-exec db1 "insert into items(name) values('alice'),('bob')"))
  (check '(("alice") ("bob")) '(sqlite-query db1 "select name from items order by id"))
  (check "2" '(sqlite-query-one db1 "select count(*) from items"))
  (check #t '(sqlite-close db1)))
(check #t '(progn
             (defglobal pgdb1 (postgres-open "postgresql://user:pass@127.0.0.1:5432/db"))
             (postgres-db-p pgdb1)))
(check #t '(postgres-close pgdb1))
(check #t '(progn
             (defglobal mydb1 (mysql-open "127.0.0.1" 3306 "user" "pass" "db"))
             (mysql-db-p mydb1)))
(check #t '(mysql-close mydb1))
(check #f '(tcp-connection-p 1))
(check #f '(tcp-listener-p 1))
(check-error '(tcp-listen 0))
(check-error '(tcp-accept 1))
(check-error '(tcp-send 1 "x"))
(check-error '(tcp-send-line 1 "x"))
(check-error '(tcp-receive-line 1 nil))
(check-error '(tcp-send-byte 1 65))
(check-error '(tcp-receive-byte 1 nil))
(check-error '(tcp-flush 1))
(check-error '(tcp-close 1))
(check-error '(tcp-listener-close 1))
(check #f '(udp-socket-p 1))
(check #t '(progn (defglobal us1 (udp-open)) (udp-socket-p us1)))
(check #t '(udp-close us1))
(check-error '(udp-bind 1 "127.0.0.1" 9999))
(check-error '(udp-connect 1 "127.0.0.1" 9999))
(check-error '(udp-send 1 "x"))
(check-error '(udp-receive 1 1))
(check-error '(udp-sendto 1 "127.0.0.1" 9999 "x"))
(check-error '(udp-receive-from 1 1))
(check-error '(udp-close 1))
(check-error '(http-get 1 "/"))
(check-error '(http-head "example.com" 1))
(check-error '(http-post "example.com" "/" 1))
(check-error '(http-get "example.com" "/" :receiver))
(define smoke-pg-url (sys-getenv "ISL_SMOKE_PG_URL"))
(when smoke-pg-url
  (when (= (eval-islisp '(system "sh -c 'command -v psql >/dev/null 2>&1'") env) 0)
    (check "1" `(progn
                  (defglobal pgdb2 (postgres-open ,smoke-pg-url))
                  (postgres-query-one pgdb2 "select 1")
                  (postgres-close pgdb2)
                  "1"))))
(define smoke-mysql-host (sys-getenv "ISL_SMOKE_MYSQL_HOST"))
(define smoke-mysql-port (sys-getenv "ISL_SMOKE_MYSQL_PORT"))
(define smoke-mysql-user (sys-getenv "ISL_SMOKE_MYSQL_USER"))
(define smoke-mysql-pass (sys-getenv "ISL_SMOKE_MYSQL_PASSWORD"))
(define smoke-mysql-db (sys-getenv "ISL_SMOKE_MYSQL_DB"))
(when (and smoke-mysql-host smoke-mysql-port smoke-mysql-user smoke-mysql-pass smoke-mysql-db)
  (when (= (eval-islisp '(system "sh -c 'command -v mysql >/dev/null 2>&1 || command -v mariadb >/dev/null 2>&1'") env) 0)
    (check "1" `(progn
                  (defglobal mydb2
                    (mysql-open ,smoke-mysql-host ,(string->number smoke-mysql-port)
                                ,smoke-mysql-user ,smoke-mysql-pass ,smoke-mysql-db))
                  (mysql-query-one mydb2 "select 1")
                  (mysql-close mydb2)
                  "1"))))
(check #t '(> (get-universal-time) 2208988800))
(check #t '(> (internal-time-units-per-second) 0))
(check #t '(<= (get-internal-real-time) (get-internal-real-time)))
(define ffi-libc
  (cond
   ((file-exists? "/usr/lib/libSystem.B.dylib") "/usr/lib/libSystem.B.dylib")
   ((file-exists? "/lib/x86_64-linux-gnu/libc.so.6") "/lib/x86_64-linux-gnu/libc.so.6")
   ((file-exists? "/lib64/libc.so.6") "/lib64/libc.so.6")
   ((file-exists? "/usr/lib64/libc.so.6") "/usr/lib64/libc.so.6")
   (else #f)))
(when ffi-libc
  (check 42 `(ffi-call ,ffi-libc "abs" :int '(:int) '(-42)))
  (check 7 `(progn
              (defpackage :ffi-smoke (:use :common-lisp :cffi))
              (in-package :ffi-smoke)
              (ffi:load-foreign-library ,ffi-libc)
              (ffi:define-foreign-function "abs" (ffi-abs x)
                :returning int
                :arguments (:int))
              (ffi-abs -7))))
(check #t '(<= (get-internal-run-time) (get-internal-run-time)))
(check #t '(>= (get-internal-run-time) 0))
(check "This is a test"
       `(with-open-file (stream ,out-file :direction :input)
          (read-line stream nil)))
(check 'missing-ok
       '(with-open-file (s "test/with-open-missing-in.lsp"
                           :direction :input
                           :if-does-not-exist nil)
          (if (null s) 'missing-ok 'ng)))
(check-error
 '(with-open-file (s "test/with-open-missing-in.lsp"
                     :direction :input
                     :if-does-not-exist :error)
    (read s)))
(check "kept"
       '(progn
          (with-open-file (s "test/with-open-existing.txt"
                             :direction :output
                             :if-exists :overwrite)
            (write-line "kept" s))
          (with-open-file (s "test/with-open-existing.txt"
                             :direction :output
                             :if-exists nil)
            (if (null s) 'skipped (write-line "overwritten" s)))
          (with-open-file (s "test/with-open-existing.txt"
                             :direction :input)
            (read-line s nil))))
(check 'no-create
       '(progn
          (delete-file "test/with-open-no-create.txt")
          (with-open-file (s "test/with-open-no-create.txt"
                             :direction :output
                             :if-does-not-exist nil)
            (if (null s) 'no-create 'created))))
(check "255" '(format nil "~D" 255))
(check "11" '(format nil "~B" 3))
(check "377" '(format nil "~O" 255))
(check "ff" '(format nil "~X" 255))
(check "A" '(format nil "~C" #\A))
(check "stream-format"
       `(progn
          (with-open-file (s ,out-file :direction :output :if-exists :overwrite)
            (format s "~A" "stream-format"))
          (with-open-file (s ,out-file :direction :input)
            (read-line s nil))))
(check 2 '(mod 17 5))
(check 2 '(floor (/ 7 3)))
(check 3 '(ceiling (/ 7 3)))
(check -2 '(truncate (/ -7 3)))
(check 3 '(round (/ 11 4)))
(check #t '(zerop 0))
(check #t '(plusp 3))
(check #t '(minusp -1))
(check #t '(evenp 8))
(check #f '(evenp 7))
(check #t '(oddp 7))
(check #f '(oddp 8))
(check 120 '(progn
              (defun fact (n)
                (if (= n 0)
                    1
                    (* n (fact (- n 1)))))
              (fact 5)))
(check '(1 2 3) '(list 1 2 3))
(check 1 '(first '(1 2 3 4 5 6 7 8 9 10)))
(check 2 '(second '(1 2 3 4 5 6 7 8 9 10)))
(check 3 '(third '(1 2 3 4 5 6 7 8 9 10)))
(check 4 '(fourth '(1 2 3 4 5 6 7 8 9 10)))
(check 5 '(fifth '(1 2 3 4 5 6 7 8 9 10)))
(check 6 '(sixth '(1 2 3 4 5 6 7 8 9 10)))
(check 7 '(seventh '(1 2 3 4 5 6 7 8 9 10)))
(check 8 '(eighth '(1 2 3 4 5 6 7 8 9 10)))
(check 9 '(ninth '(1 2 3 4 5 6 7 8 9 10)))
(check 10 '(tenth '(1 2 3 4 5 6 7 8 9 10)))
(check #t '(symbolp 'abc))
(check 'ISLISP-USER::point '(defclass point () (x y)))
(check 3 '(progn
            (defglobal p0 (make-instance 'point))
            (setf (slot-value p0 'x) 3)
            (slot-value p0 'x)))
(check 7 '(progn
            (setf (slot-value p0 'y) 7)
            (slot-value p0 'y)))
(check 11 '(progn
             (defglobal p1 (make-instance point))
             (setf (slot-value p1 'x) 11)
             (slot-value p1 'x)))
(check #t '(instancep p0))
(check #f '(instancep 42))
(check #t '(eq (class-of p0) point))
(check #t '(eq (class-of point) point))
(check 'ISLISP-USER::describe-shape '(defgeneric describe-shape (obj)))
(check 'ISLISP-USER::describe-shape
       '(defmethod describe-shape ((obj point))
          (format nil "POINT(~A,~A)"
                  (slot-value obj 'x)
                  (slot-value obj 'y))))
(check "POINT(3,7)" '(describe-shape p0))
(check 'ISLISP-USER::entity '(defclass entity () (id)))
(check 'ISLISP-USER::person '(defclass person (entity) (name)))
(check 'ISLISP-USER::who '(defgeneric who (obj)))
(check 'ISLISP-USER::who
       '(defmethod who ((obj entity))
          "entity"))
(check 'ISLISP-USER::who
       '(defmethod who ((obj person))
          "person"))
(check "person" '(progn
                   (defglobal p2 (make-instance 'person))
                   (who p2)))
(check 'ISLISP-USER::tag '(defgeneric tag (x)))
(check 'ISLISP-USER::tag
       '(defmethod tag (x)
          "default"))
(check "default" '(tag 123))
(check 'ISLISP-USER::mammal '(defclass mammal (entity) ()))
(check 'ISLISP-USER::dog '(defclass dog (mammal) ()))
(check 'ISLISP-USER::classify '(defgeneric classify (obj)))
(check 'ISLISP-USER::classify
       '(defmethod classify ((obj entity))
          "entity"))
(check 'ISLISP-USER::classify
       '(defmethod classify ((obj mammal))
          "mammal"))
(check 'ISLISP-USER::classify
       '(defmethod classify (obj)
          "default"))
(check "mammal" '(progn
                   (defglobal d1 (make-instance 'dog))
                   (classify d1)))
(check "default" '(classify "no-instance"))
(check 'ISLISP-USER::point3d '(defclass point3d (point) (z)))
(check 40 '(progn
             (defglobal p3 (make-instance 'point3d))
             (setf (slot-value p3 'x) 10)
             (setf (slot-value p3 'z) 30)
             (+ (slot-value p3 'x) (slot-value p3 'z))))
(check #f '(instancep point3d))
(check 'ISLISP-USER::only-inst '(defgeneric only-inst (obj)))
(check 'ISLISP-USER::only-inst
       '(defmethod only-inst ((obj entity))
          "ok"))
(check-error '(only-inst 1))
(check 'ISLISP-USER::lefta '(defclass lefta (entity) ()))
(check 'ISLISP-USER::righta '(defclass righta (entity) ()))
(check 'ISLISP-USER::botha '(defclass botha (lefta righta) ()))
(check 'ISLISP-USER::amb '(defgeneric amb (obj)))
(check 'ISLISP-USER::amb
       '(defmethod amb ((obj lefta))
          "left"))
(check 'ISLISP-USER::amb
       '(defmethod amb ((obj righta))
          "right"))
(check-error '(progn
                (defglobal b1 (make-instance 'botha))
                (amb b1)))
(check 'ISLISP-USER::person2
       '(defclass person2 ()
          ((name :accessor person2-name :initform "Anonymous")
           (age :accessor person2-age :initform 0))))
(check "Anonymous" '(progn
                      (defglobal pp0 (make-instance 'person2))
                      (person2-name pp0)))
(check 0 '(person2-age pp0))
(check "Alice" '(progn
                  (defglobal pp1 (make-instance 'person2 :name "Alice" :age 30))
                  (person2-name pp1)))
(check 30 '(person2-age pp1))
(check "Bob" '(progn
                (setf (person2-name pp1) "Bob")
                (person2-name pp1)))
(check 'ISLISP-USER::person3
       '(defclass person3 ()
          ((name :initarg :person-name :accessor person3-name :initform "Anonymous")
           (age :initarg :person-age :accessor person3-age :initform 0))))
(check "Carol" '(progn
                  (defglobal pp3 (make-instance 'person3 :person-name "Carol" :person-age 25))
                  (person3-name pp3)))
(check 25 '(person3-age pp3))
(check 'ISLISP-USER::person4
       '(defclass person4 ()
          ((name :initform "none" :reader person4-name :writer set-person4-name)
           (age :initform 0 :reader person4-age :writer set-person4-age))))
(check "none" '(progn
                 (defglobal pp4 (make-instance 'person4))
                 (person4-name pp4)))
(check 41 '(progn
             (set-person4-age pp4 41)
             (person4-age pp4)))
(check "Dave" '(progn
                 (set-person4-name pp4 "Dave")
                 (person4-name pp4)))
(check 'ok '(progn (defglobal g 10)
                   (setq g 20)
                   'ok))
(check 7 '(progn (defglobal v 7) v))
(check 1 '(progn (defvar dv 1) (defvar dv 99) dv))
(check 5 '(progn
            (defpackage :my-package
              (:use :common-lisp)
              (:export greed))
            (in-package :my-package)
            (defun greed (x) (+ x 2))
            (greed 3)))
(check 49 '(progn
             (defpackage :kw-package
               (:use :islisp)
               (:export :kwsq))
             (in-package :kw-package)
             (defun kwsq (x) (* x x))
             (in-package :islisp-user)
             (kw-package:kwsq 7)))
(check 8 '(progn
            (in-package :islisp-user)
            (my-package:greed 6)))
(check 'ISLISP-USER '(progn
                    (defpackage :pkg-base
                      (:use :islisp)
                      (:intern hidden)
                      (:export base-fn base-val))
                    (in-package :pkg-base)
                    (defun base-fn () 123)
                    (defglobal base-val 77)
                    (defglobal hidden 88)
                    (in-package :islisp-user)
                    (current-package)))
(check 77 '(progn
             (defpackage :pkg-consumer
               (:nicknames :pc)
               (:use :islisp)
               (:import-from :pkg-base base-val)
               (:shadow base-fn)
               (:intern local-val)
               (:export base-fn local-val base-val))
             (in-package :pkg-consumer)
             (defun base-fn () 999)
             (defglobal local-val 55)
             base-val))
(check 999 '(progn
              (in-package :islisp-user)
              (pc:base-fn)))
(check 55 '(progn
             (in-package :islisp-user)
             pc:local-val))
(check 77 '(progn
             (in-package :islisp-user)
             pc:base-val))
(check 77 '(progn
             (defpackage :pkg-collide-a
               (:use :islisp)
               (:export val))
             (in-package :pkg-collide-a)
             (defglobal val 77)
             (defpackage :pkg-collide-b
               (:use :islisp)
               (:export val))
             (in-package :pkg-collide-b)
             (defglobal val 900)
             (defpackage :pkg-collide-user
               (:use :islisp :pkg-collide-a :pkg-collide-b)
               (:shadowing-import-from :pkg-collide-a val))
             (in-package :pkg-collide-user)
             val))
(check 'demo '(progn
                (defpackage demo (:use islisp) (:export sq v))
                (in-package demo)
                (defun sq (x) (* x x))
                (defglobal v 11)
                (in-package islisp-user)
                'demo))
(check 25 '(demo:sq 5))
(check 11 'demo:v)
(check #t '(use-package 'demo))
(check 36 '(sq 6))
(check #t '(provide :my-module))
(check #t '(require :my-module))
(check #t `(require :tmp-required ,load-file))
(check #t '(require :tmp-required))
(check 3 '(progn 1 2 3))
(check 5 '(let ((x 2) (y 3)) (+ x y)))
(check 3 '(let* ((x 1) (y (+ x 2))) y))
(check 30 '(block calc
             (defglobal b1 10)
             (+ b1 20)))
(check '() '(block empty))
(check 99 '(block out
             (return-from out 99)
             0))
(check 99 '(catch 'done
             (throw 'done 99)
             0))
(check 3 '(handler-case
            (+ 1 2)
            (error (e) 0)))
(check 42 '(handler-case
             not-defined-variable
             (error (e) 42)))
(check #t '(handler-case
             not-defined-variable
             (error (e) (not (null e)))))
(check-error '(handler-case
                not-defined-variable
                (warning (e) 0)))
(check 20 '(catch 'done
             (+ 1 (catch 'inner
                    (throw 'inner 19)
                    0))))
(check 42 '(progn
             (defun thrower (tag value)
               (throw tag value))
             (catch 'ok
               (thrower 'ok 42))))
(check 7 '(block out
            (+ 1 (block inner
                   (return-from inner 6)
                   0))))
(check 42 '(progn (defglobal s 0) (setf s 42) s))
(check 11 '(progn (defglobal incx 10) (incf incx)))
(check 15 '(progn (defglobal incx2 10) (incf incx2 5)))
(check '(4 . 2) '(progn (defglobal p3 (cons 1 2)) (incf (car p3) 3) p3))
(check '(1 . 7) '(progn (defglobal p4 (cons 1 2)) (incf (cdr p4) 5) p4))
(check '(9 . 2) '(progn (defglobal p (cons 1 2)) (setf (car p) 9) p))
(check '(9 . 8) '(progn (defglobal p2 (cons 9 2)) (setf (cdr p2) 8) p2))
(check 10 '(progn
             (defglobal wi 0)
             (defglobal wsum 0)
             (while (< wi 5)
               (setq wsum (+ wsum wi))
               (setq wi (+ wi 1)))
             wsum))
(check 15 '(progn
             (defglobal lsum 0)
             (loop for i from 1 to 5
                   do (setq lsum (+ lsum i)))
             lsum))
(check 6 '(progn
            (defglobal lsum2 0)
            (loop for i from 1 to 5
                  until (> i 3)
                  do (setq lsum2 (+ lsum2 i)))
            lsum2))
(check 120 '(do ((n 5 (- n 1))
                 (acc 1 (* acc n)))
                ((= n 0) acc)))
(check 6 '(progn
            (defglobal dlsum 0)
            (dolist (x '(1 2 3) dlsum)
              (setq dlsum (+ dlsum x)))))
(check 10 '(progn
             (defglobal dtsum 0)
             (dotimes (i 5 dtsum)
               (setq dtsum (+ dtsum i)))))
(check 3 '(progn
            (defglobal tg 0)
            (tagbody
              top
                (setq tg (+ tg 1))
                (if (< tg 3) (go top) nil))
            tg))
(check 2 '(if nil 1 2))
(check 'ok '(cond
             ((= 1 2) 'ng)
             ((= 2 2) 'ok)
             (t 'ng2)))
(check 'fallback '(cond
                   (nil 'ng)
                   (t 'fallback)))
(check 'two '(case 2
               ((1) 'one)
               ((2 3) 'two)
               (otherwise 'other)))
(check 'other '(case 9
                 ((1 2 3) 'small)
                 (otherwise 'other)))
(check #t '(not nil))
(check #f '(not t))
(check "Hello, ONE!" '(format nil "Hello, ~A!" "ONE"))
(check "(1 2)" '(format nil "~S" '(1 2)))
(check "A\nB~C" '(format nil "A~%B~~C"))
(check '() '(format t "visible: ~A~%" "ok"))
(check '(alpha beta gamma) `(with-open-file (s ,smoke-file) (read s)))
(check 'file-not-found
       '(catch 'file-error
          (with-open-file (s "test/this-file-does-not-exist-xyz.lsp")
            (if (null s)
                (throw 'file-error 'file-not-found)
                (read s)))))
(check #t '(string= "abc" "abc"))
(check #f '(string= "abc" "abd"))
(check #t '(string/= "abc" "abd"))
(check #f '(string/= "abc" "abc"))
(check #t '(string< "abc" "abd"))
(check #t '(string<= "abc" "abc"))
(check #t '(string> "abd" "abc"))
(check #t '(string>= "abc" "abc"))
(check #t '(stringp "abc"))
(check #f '(stringp 1))
(check "xxx" '(create-string 3 #\x))
(check "abcdef" '(string-concat "ab" "cd" "ef"))
(check "abcdef" '(string-append "ab" "cd" "ef"))
(check "bc" '(substring "abcd" 1 3))
(check "cd" '(substring "abcd" 2))
(check 1 '(char-index #\b "abcd"))
(check '() '(char-index #\z "abcd"))
(check 2 '(char-index #\c "abcd" 2))
(check 1 '(string-index "bc" "abcd"))
(check '() '(string-index "zz" "abcd"))
(check 2 '(string-index "" "abcd" 2))
(check 4 '(length "abcd"))
(check 3 '(length '(1 2 3)))
(check '(1 4 9) '(mapcar (lambda (x) (* x x)) '(1 2 3)))
(check 10 '(reduce + '(1 2 3 4)))
(check 20 '(reduce + '(1 2 3 4) 10))
(check '(3 4 5) '(remove-if (lambda (x) (< x 3)) '(1 2 3 4 5)))
(check '(2 4) '(remove-if-not evenp '(1 2 3 4 5)))
(check 4 '(find 4 '(1 3 4 5)))
(check '() '(find 2 '(1 3 5)))
(check 3 '(find 3 '(1 2 3 4) :test =))
(check '(3 . b) '(find 3 '((1 . a) (3 . b) (5 . c)) :key car))
(check #\a '(find #\a "abcde"))
(check 3 '(find 3 #(1 2 3 4)))
(check 50000 '(progn
                (defun count-down (n acc)
                  (if (= n 0)
                      acc
                      (count-down (- n 1) (+ acc 1))))
                (count-down 50000 0)))
(check 120 '(progn
               (defun factorial-t (n &optional (acc 1))
                 (if (<= n 1)
                     acc
                     (factorial-t (- n 1) (* n acc))))
               (factorial-t 5)))
(check 600 '(factorial-t 5 5))
(check 'none '(progn
                (defun opt1 (a &optional b)
                  (if b b 'none))
                (opt1 1)))
(check 9 '(opt1 1 9))
(check "Negative value" '(progn
                           (defun double (x)
                             (if (< x 0)
                                 (return-from double "Negative value")
                                 (* 2 x)))
                           (double -3)))
(check 8 '(double 4))
(check 5 '(progn
            (defun traced-add1 (x) (+ x 1))
            (trace traced-add1)
            (traced-add1 4)))
(check 6 '(progn
            (defun traced-add2 (x) (+ x 2))
            (trace traced-add2)
            (untrace traced-add2)
            (traced-add2 4)))
(check 7 '(progn
            (defmacro traced-macro (x) (list '+ x 2))
            (trace traced-macro)
            (traced-macro 5)))
(check '(1 2 3) '(progn
                   (defun collect (a &rest r)
                     (cons a r))
                   (collect 1 2 3)))
(check 42 '(progn
             (defmacro inc2 (x)
               (list '+ x 2))
             (inc2 40)))
(check 7 '(progn
            (defglobal mz 0)
            (defmacro set-to-seven (place)
              (list 'setq place 7))
            (set-to-seven mz)
            mz))
(check 2 '(progn
            (defglobal mw 0)
            (defmacro my-when (condition &rest body)
              `(if ,condition
                   (progn ,@body)))
            (my-when (> 5 3)
              (setq mw (+ mw 1))
              (setq mw (+ mw 1)))
            mw))
(check 2 '(progn
            (defglobal mw2 0)
            (defmacro my-when2 (condition &body body)
              `(if ,condition
                   (progn ,@body)))
            (my-when2 (> 5 3)
              (setq mw2 (+ mw2 1))
              (setq mw2 (+ mw2 1)))
            mw2))
(check '(+ 1 2) '(progn
                   (defmacro mplus (a b)
                     (list '+ a b))
                   (macroexpand-1 '(mplus 1 2))))
(check '(+ 5 1) '(progn
                   (defmacro mplus (a b)
                     (list '+ a b))
                   (defmacro mwrap (x)
                     (list 'mplus x 1))
                   (macroexpand '(mwrap 5))))
(check #t '(progn
             (defglobal g1 (gensym))
             (defglobal g2 (gensym))
             (if (symbolp g1)
                 (if (symbolp g2)
                     (not (eq g1 g2))
                     nil)
                 nil)))
(check #t '(progn
             (defglobal gp1 (gensym "TMP"))
             (defglobal gp2 (gensym "TMP"))
             (if (symbolp gp1)
                 (if (symbolp gp2)
                     (not (eq gp1 gp2))
                     nil)
                 nil)))

(display "smoke tests passed\n")
