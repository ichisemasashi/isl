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

;; ---- 7-A: call-next-method in primary methods ----

(assert-equal "Dog: woof"
  (run-forms
    '((defclass <animal> () ())
      (defgeneric speak (a))
      (defmethod speak ((a <animal>)) "woof")
      (defclass <dog> (<animal>) ())
      (defmethod speak ((d <dog>))
        (string-append "Dog: " (call-next-method)))
      (speak (make-instance (class <dog>)))))
  "7-A: call-next-method returns parent method result")

;; call-next-method with explicit args
(assert-equal 30
  (run-forms
    '((defclass <base> () ())
      (defgeneric add-it (x n))
      (defmethod add-it ((x <base>) n) n)
      (defclass <derived> (<base>) ())
      (defmethod add-it ((x <derived>) n) (call-next-method x (* n 3)))
      (add-it (make-instance (class <derived>)) 10)))
  "7-A: call-next-method with explicit args")

;; next-method-p returns true when there is a next method
(assert-equal #t
  (run-forms
    '((defclass <p> () ())
      (defclass <c> (<p>) ())
      (defgeneric has-next (x))
      (defmethod has-next ((x <p>)) "parent")
      (defmethod has-next ((x <c>))
        (if (next-method-p) #t '()))
      (has-next (make-instance (class <c>)))))
  "7-A: next-method-p returns #t when next method exists")

;; next-method-p returns nil when no next method
(assert-equal '()
  (run-forms
    '((defclass <solo> () ())
      (defgeneric solo-fn (x))
      (defmethod solo-fn ((x <solo>))
        (if (next-method-p) #t '()))
      (solo-fn (make-instance (class <solo>)))))
  "7-A: next-method-p returns nil when no next method")

;; call-next-method raises error when no next method
(assert-isl-error
  (lambda ()
    (run-forms
      '((defclass <alone> () ())
        (defgeneric lonely (x))
        (defmethod lonely ((x <alone>)) (call-next-method))
        (lonely (make-instance (class <alone>))))))
  "7-A: call-next-method raises error when no next method")

;; ---- 7-B: :before, :after, :around auxiliary methods ----

;; :before runs before primary
(assert-equal '(after primary before)
  (run-forms
    '((defvar *log* (quote ()))
      (defclass <evt> () ())
      (defgeneric trigger (x))
      (defmethod trigger ((x <evt>)) (setq *log* (cons (quote primary) *log*)) 'done)
      (defmethod trigger :before ((x <evt>)) (setq *log* (cons (quote before) *log*)))
      (defmethod trigger :after  ((x <evt>)) (setq *log* (cons (quote after)  *log*)))
      (trigger (make-instance (class <evt>)))
      *log*))
  "7-B: :before and :after run in correct order")

;; :before/:after don't change return value
(assert-equal "primary-result"
  (run-forms
    '((defclass <r> () ())
      (defgeneric get-result (x))
      (defmethod get-result ((x <r>)) "primary-result")
      (defmethod get-result :before ((x <r>)) "before-ignored")
      (defmethod get-result :after  ((x <r>)) "after-ignored")
      (get-result (make-instance (class <r>)))))
  "7-B: :before/:after do not affect return value")

;; :around wraps everything
(assert-equal "around[primary]"
  (run-forms
    '((defclass <wrapped> () ())
      (defgeneric process (x))
      (defmethod process ((x <wrapped>)) "primary")
      (defmethod process :around ((x <wrapped>))
        (string-append "around[" (call-next-method) "]"))
      (process (make-instance (class <wrapped>)))))
  "7-B: :around wraps primary method")

;; :around can skip primary by not calling next-method
(assert-equal "bypassed"
  (run-forms
    '((defclass <bypassable> () ())
      (defgeneric bypass-me (x))
      (defmethod bypass-me ((x <bypassable>)) "primary-never-called")
      (defmethod bypass-me :around ((x <bypassable>)) "bypassed")
      (bypass-me (make-instance (class <bypassable>)))))
  "7-B: :around can skip primary")

;; :around with :before and :after
(assert-equal '(around-after after primary before around-before)
  (run-forms
    '((defvar *full-log* (quote ()))
      (defclass <full> () ())
      (defgeneric full-test (x))
      (defmethod full-test ((x <full>))
        (setq *full-log* (cons (quote primary) *full-log*)) 'ok)
      (defmethod full-test :before ((x <full>))
        (setq *full-log* (cons (quote before) *full-log*)))
      (defmethod full-test :after ((x <full>))
        (setq *full-log* (cons (quote after) *full-log*)))
      (defmethod full-test :around ((x <full>))
        (setq *full-log* (cons (quote around-before) *full-log*))
        (let ((r (call-next-method)))
          (setq *full-log* (cons (quote around-after) *full-log*))
          r))
      (full-test (make-instance (class <full>)))
      *full-log*))
  "7-B: :around + :before + :after complete sequence")

;; ---- 7-C: subclassp ----

(assert-equal #t
  (run-forms
    '((defclass <animal> () ())
      (defclass <dog> (<animal>) ())
      (subclassp (class <dog>) (class <animal>))))
  "7-C: subclassp returns #t for subclass")

(assert-equal '()
  (run-forms
    '((defclass <cat> () ())
      (defclass <fish> () ())
      (subclassp (class <cat>) (class <fish>))))
  "7-C: subclassp returns nil for unrelated classes")

(assert-equal #t
  (run-forms
    '((defclass <a> () ())
      (subclassp (class <a>) (class <a>))))
  "7-C: subclassp returns #t for same class")

;; transitive subclassp
(assert-equal #t
  (run-forms
    '((defclass <a> () ())
      (defclass <b> (<a>) ())
      (defclass <c> (<b>) ())
      (subclassp (class <c>) (class <a>))))
  "7-C: subclassp is transitive")

;; (class <name>) special form
(assert-equal #t
  (run-forms
    '((defclass <myclass> () ())
      (let ((c (class <myclass>)))
        (if c #t '()))))
  "7-C: (class <name>) returns class object")

;; ---- 7-D: <standard-object> and <built-in-class> ----

;; Classes without explicit superclass inherit from <standard-object>
(assert-equal #t
  (run-forms
    '((defclass <userclass> () ())
      (subclassp (class <userclass>) (class <standard-object>))))
  "7-D: user class is subclass of <standard-object>")

;; <standard-object> itself is available
(assert-equal #t
  (run-forms
    '((let ((so (class <standard-object>)))
        (if so #t '()))))
  "7-D: <standard-object> is defined")

;; <built-in-class> is available
(assert-equal #t
  (run-forms
    '((let ((bc (class <built-in-class>)))
        (if bc #t '()))))
  "7-D: <built-in-class> is defined")

;; subclass of <standard-object> works transitively
(assert-equal #t
  (run-forms
    '((defclass <a> () ())
      (defclass <b> (<a>) ())
      (subclassp (class <b>) (class <standard-object>))))
  "7-D: subclass of user class is also subclass of <standard-object>")

;; ---- 7-E: initialize-object ----

;; Default initialize-object returns object unchanged
(assert-equal 99
  (run-forms
    '((defclass <init-test> () ((val :initarg :val :initform 99)))
      (defvar *obj* (make-instance (class <init-test>)))
      (slot-value *obj* (quote val))))
  "7-E: default initialize-object preserves initform")

;; initialize-object can be overridden with a custom method (just checks it gets called)
(assert-equal #t
  (run-forms
    '((defvar *init-called* nil)
      (defclass <tracked> () ((x :initarg :x :initform 0)))
      (defmethod initialize-object ((o <tracked>) initargs)
        (setq *init-called* #t)
        (call-next-method))
      (make-instance (class <tracked>) :x 5)
      *init-called*))
  "7-E: custom initialize-object called on make-instance")

;; ---- 7-F: slot-boundp and slot-makunbound ----

;; slot-boundp returns #t for bound slot
(assert-equal #t
  (run-forms
    '((defclass <withslot> () ((x :initarg :x :initform 42)))
      (defvar *ws* (make-instance (class <withslot>)))
      (slot-boundp *ws* (quote x))))
  "7-F: slot-boundp returns #t for slot with initform")

;; slot-makunbound removes slot binding
(assert-equal '()
  (run-forms
    '((defclass <withslot> () ((x :initarg :x :initform 42)))
      (defvar *ws* (make-instance (class <withslot>)))
      (slot-makunbound *ws* (quote x))
      (slot-boundp *ws* (quote x))))
  "7-F: slot-makunbound makes slot unbound")

;; slot-makunbound returns the instance
(assert-equal #t
  (run-forms
    '((defclass <sm-test> () ((y :initform 10)))
      (defvar *sm* (make-instance (class <sm-test>)))
      (let ((result (slot-makunbound *sm* (quote y))))
        (if result #t '()))))
  "7-F: slot-makunbound returns instance")

;; slot-boundp returns nil when slot is not bound
(assert-equal '()
  (run-forms
    '((defclass <unbnd> () ((z :initarg :z :initform 5)))
      (defvar *ub* (make-instance (class <unbnd>)))
      (slot-makunbound *ub* (quote z))
      (slot-boundp *ub* (quote z))))
  "7-F: slot-boundp returns nil after makunbound")

(display "runtime Phase7 smoke passed")
(newline)
