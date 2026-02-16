;; Block AST
;;   (block-heading pos level inlines)
;;   (block-paragraph pos inlines)
;;   (block-blockquote pos blocks)
;;   (block-hr pos)
;;   (block-list pos list-kind list-type start items)
;;   (block-deflist pos items)
;;   (block-code pos text id classes attrs)
;;   (block-line-block pos lines)

(defun make-block-heading (pos level inlines)
  (list 'block-heading pos level inlines))

(defun make-block-paragraph (pos inlines)
  (list 'block-paragraph pos inlines))

(defun make-block-blockquote (pos blocks)
  (list 'block-blockquote pos blocks))

(defun make-block-hr (pos)
  (list 'block-hr pos))

(defun make-block-list (pos list-kind list-type start items)
  (list 'block-list pos list-kind list-type start items))

(defun make-block-deflist (pos items)
  (list 'block-deflist pos items))

(defun make-block-code (pos text id classes attrs)
  (list 'block-code pos text id classes attrs))

(defun make-block-line-block (pos lines)
  (list 'block-line-block pos lines))

(defun block-kind (b) (first b))
(defun block-pos (b) (second b))
(defun block-heading-level (b) (third b))
(defun block-inlines (b)
  (if (eq (block-kind b) 'block-heading)
      (fourth b)
      (third b)))
(defun block-children (b) (third b))
(defun block-list-kind (b) (third b))
(defun block-list-type (b) (fourth b))
(defun block-list-start (b) (fifth b))
(defun block-list-items (b) (sixth b))
(defun block-deflist-items (b) (third b))
(defun block-code-text (b) (third b))
(defun block-code-id (b) (fourth b))
(defun block-code-classes (b) (fifth b))
(defun block-code-attrs (b) (sixth b))
(defun block-line-block-lines (b) (third b))

;; List item:
;;   (list-item pos blocks task-state)
;; task-state: 'none | 'unchecked | 'checked
(defun make-list-item (pos blocks task-state)
  (list 'list-item pos blocks task-state))
(defun list-item-pos (i) (second i))
(defun list-item-blocks (i) (third i))
(defun list-item-task-state (i) (fourth i))

;; Definition list item:
;;   (def-item pos term-inlines defs)
;; defs: list of block lists
(defun make-def-item (pos term-inlines defs)
  (list 'def-item pos term-inlines defs))
(defun def-item-pos (i) (second i))
(defun def-item-term (i) (third i))
(defun def-item-defs (i) (fourth i))

;; Inline AST
;;   (inline-text pos text)
;;   (inline-code pos text)
;;   (inline-emph pos children)
;;   (inline-strong pos children)
;;   (inline-link pos children url)
;;   (inline-image pos alt-children url)

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

(defun make-inline-image (pos children url)
  (list 'inline-image pos children url))

(defun inline-kind (n) (first n))
(defun inline-pos (n) (second n))
(defun inline-text-value (n) (third n))
(defun inline-children (n) (third n))
(defun inline-link-url (n) (fourth n))
(defun inline-image-url (n) (fourth n))
