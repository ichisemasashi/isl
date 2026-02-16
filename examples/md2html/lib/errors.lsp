(defun md-pos (line col offset)
  (list line col offset))

(defun md-pos-line (pos) (first pos))
(defun md-pos-col (pos) (second pos))
(defun md-pos-offset (pos) (third pos))

(defun md-error (code message pos detail)
  (list 'md-error code message pos detail))

(defun md-error-code (e) (second e))
(defun md-error-message (e) (third e))
(defun md-error-pos (e) (fourth e))
(defun md-error-detail (e) (fifth e))

(defun md-fail (code message pos detail)
  (error "MD2HTML" code message pos detail))
