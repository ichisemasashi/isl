;; math-package.lisp
(defpackage :math
  (:use :islisp) ; ISLISP 標準パッケージを利用
  (:export square cube))

(in-package :math)

(defun square (x) (* x x))
(defun cube (x) (* x x x))
