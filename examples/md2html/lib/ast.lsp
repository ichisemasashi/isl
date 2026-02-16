;; Block AST
;;   (block-heading pos level inlines)
;;   (block-paragraph pos inlines)

(defun make-block-heading (pos level inlines)
  (list 'block-heading pos level inlines))

(defun make-block-paragraph (pos inlines)
  (list 'block-paragraph pos inlines))

(defun block-kind (b) (first b))
(defun block-pos (b) (second b))
(defun block-heading-level (b) (third b))
(defun block-inlines (b)
  (if (eq (block-kind b) 'block-heading)
      (fourth b)
      (third b)))

;; Inline AST
;;   (inline-text pos text)
;;   (inline-code pos text)
;;   (inline-emph pos children)
;;   (inline-strong pos children)
;;   (inline-link pos children url)

(defun make-inline-text (pos text)
  (list 'inline-text pos text))

(defun make-inline-code (pos text)
  (list 'inline-code pos text))

(defun make-inline-emph (pos children)
  (list 'inline-emph pos children))

(defun make-inline-strong (pos children)
  (list 'inline-strong pos children))

(defun make-inline-link (pos children url)
  (list 'inline-link pos children url))

(defun inline-kind (n) (first n))
(defun inline-pos (n) (second n))
(defun inline-text-value (n) (third n))
(defun inline-children (n) (third n))
(defun inline-link-url (n) (fourth n))
