(defun +agenda-projects-get-children (parent query)
  "Return list of child elements of `PARENT' that match the org-ql
query `QUERY'."
  ;; REVIEW This regexp-based approach is wildly inefficient.
  (org-ql-select (org-agenda-files)
    `(and ,query
          (parent (heading-regexp ,(org-element-property :raw-value parent))))
    :action #'element-with-markers))

(defun +agenda-projects-process-entry (element depth)
  (insert (format "%s%s\n" (make-string (* 2 depth) 32) (org-ql-view--format-element element)))
  (dolist (child (+agenda-projects-get-children element '(todo)))
    (+agenda-projects-process-entry child (1+ depth))))

(defun +agenda-projects-block (_)
  ;; (org-agenda-prepare-window (generate-new-buffer "*agenda") nil)
  (org-agenda-prepare)
  (let ((elements (org-ql-select
                    (org-agenda-files)
                    '(and (todo)
                          (children (todo))
                          (not (parent (todo))))
                    :action #'element-with-markers)))
    (dolist (element elements)
      (+agenda-projects-process-entry element 0)
      (insert "\n")))
  (org-agenda-finalize))

(cl-pushnew '("e" "Experimental"
             ((+agenda-projects-block nil)))
            org-agenda-custom-commands)

(general-define-key :keymaps 'override "<f1>" (lambda () (interactive (org-agenda nil "e"))))
