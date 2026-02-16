;; Block AST
;;   (block-heading pos level inlines id classes attrs)
;;   (block-paragraph pos inlines)
;;   (block-blockquote pos blocks)
;;   (block-hr pos)
;;   (block-list pos list-kind list-type start items)
;;   (block-deflist pos items)
;;   (block-code pos text id classes attrs)
;;   (block-line-block pos lines)
;;   (block-table pos headers aligns rows caption id classes attrs)
;;   (block-div pos blocks id classes attrs)

(defun make-block-heading (pos level inlines id classes attrs)
  (list 'block-heading pos level inlines id classes attrs))

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

(defun make-block-table (pos headers aligns rows caption id classes attrs)
  (list 'block-table pos headers aligns rows caption id classes attrs))

(defun make-block-div (pos blocks id classes attrs)
  (list 'block-div pos blocks id classes attrs))

(defun block-kind (b) (first b))
(defun block-pos (b) (second b))
(defun block-heading-level (b) (third b))
(defun block-inlines (b)
  (if (eq (block-kind b) 'block-heading)
      (fourth b)
      (third b)))
(defun block-heading-id (b)
  (if (and (eq (block-kind b) 'block-heading) (> (length b) 4))
      (fifth b)
      '()))
(defun block-heading-classes (b)
  (if (and (eq (block-kind b) 'block-heading) (> (length b) 5))
      (sixth b)
      '()))
(defun block-heading-attrs (b)
  (if (and (eq (block-kind b) 'block-heading) (> (length b) 6))
      (seventh b)
      '()))
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
(defun block-table-headers (b) (third b))
(defun block-table-aligns (b) (fourth b))
(defun block-table-rows (b) (fifth b))
(defun block-table-caption (b) (sixth b))
(defun block-table-id (b) (seventh b))
(defun block-table-classes (b) (eighth b))
(defun block-table-attrs (b)
  (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr b))))))))))
(defun block-div-id (b) (fourth b))
(defun block-div-classes (b) (fifth b))
(defun block-div-attrs (b) (sixth b))

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
;;   (inline-code pos text id classes attrs)
;;   (inline-emph pos children)
;;   (inline-strong pos children)
;;   (inline-link pos children url id classes attrs)
;;   (inline-image pos alt-children url)
;;   (inline-span pos children id classes attrs)
;;   (inline-del pos children)
;;   (inline-sub pos children)
;;   (inline-sup pos children)
;;   (inline-mark pos children)
;;   (inline-abbr pos text title)

(defun make-inline-text (pos text)
  (list 'inline-text pos text))

(defun make-inline-code (pos text id classes attrs)
  (list 'inline-code pos text id classes attrs))

(defun make-inline-emph (pos children)
  (list 'inline-emph pos children))

(defun make-inline-strong (pos children)
  (list 'inline-strong pos children))

(defun make-inline-link (pos children url id classes attrs)
  (list 'inline-link pos children url id classes attrs))

(defun make-inline-image (pos children url)
  (list 'inline-image pos children url))

(defun make-inline-span (pos children id classes attrs)
  (list 'inline-span pos children id classes attrs))

(defun make-inline-del (pos children)
  (list 'inline-del pos children))

(defun make-inline-sub (pos children)
  (list 'inline-sub pos children))

(defun make-inline-sup (pos children)
  (list 'inline-sup pos children))

(defun make-inline-mark (pos children)
  (list 'inline-mark pos children))

(defun make-inline-abbr (pos text title)
  (list 'inline-abbr pos text title))

(defun inline-kind (n) (first n))
(defun inline-pos (n) (second n))
(defun inline-text-value (n) (third n))
(defun inline-children (n) (third n))
(defun inline-link-url (n) (fourth n))
(defun inline-image-url (n) (fourth n))
(defun inline-code-id (n) (if (> (length n) 3) (fourth n) '()))
(defun inline-code-classes (n) (if (> (length n) 4) (fifth n) '()))
(defun inline-code-attrs (n) (if (> (length n) 5) (sixth n) '()))
(defun inline-link-id (n) (if (> (length n) 4) (fifth n) '()))
(defun inline-link-classes (n) (if (> (length n) 5) (sixth n) '()))
(defun inline-link-attrs (n) (if (> (length n) 6) (seventh n) '()))
(defun inline-span-id (n) (fourth n))
(defun inline-span-classes (n) (fifth n))
(defun inline-span-attrs (n) (sixth n))
(defun inline-abbr-text (n) (third n))
(defun inline-abbr-title (n) (fourth n))
