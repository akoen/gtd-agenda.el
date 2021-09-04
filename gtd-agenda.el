;; Inspired by:
;; https://github.com/pestctrl/emacs-config/blob/84c557982a860e86d6f67976a82ea776a7bd2c7a/config-org-new.org#my-own-agenda-renderer

(straight-use-package 'org-ql)
(require 'org-ql-view)

(setq tmp-org-agenda-display-buffer-alist '(("\\*Org Agenda\\*"
                                             (+popup-buffer)
                                             (actions)
                                             (side . bottom)
                                             (size 0.25)
                                             (window-width . 40)
                                             (window-height . 0.16)
                                             (slot)
                                             (vslot)
                                             (window-parameters
                                              (ttl . 5)
                                              (quit . t)
                                              (select . t)
                                              (modeline)
                                              (autosave)
                                              (transient . t)
                                              (no-other-window . t)))))

(cl-defstruct +agenda-headline todo? keyword text deadline)

(defun +agenda-projects-add-next-face (string)
  (let ((face 'org-priority))
    (org-add-props string nil 'face face 'font-lock-fontified t)))

;; (defun +agenda-projects-list-child-headings (parent query)
;;   "Return list of child elements of `PARENT' that match the org-ql
;; query `QUERY'."
;;   ;; REVIEW This regexp-based approach is wildly inefficient.
;;   (org-ql-select (org-agenda-files)
;;     `(and ,query
;;           (parent (heading-regexp ,(org-element-property :raw-value parent))))
;;     :action #'element-with-markers))

(defun +agenda-projects-list-child-headings (parent)
  "Return each subheading of PARENT, formatted as an org-element with markers."
  ;; TODO Should not depend on (org-ql--add-markers).
  ;; NOTE: See https://github.com/alphapapa/org-ql/issues/41 for a potential
  ;; implementation of this function in org-ql.
  (save-excursion
    (let ((marker (org-element-property :org-marker parent)))
      (set-buffer (marker-buffer marker))
      (goto-char marker))
    (save-restriction
      (org-narrow-to-subtree)
      (outline-show-all)
      (when (org-goto-first-child)
        (setq tmp-entries (cl-loop collect (org-ql--add-markers
                                            (org-element-headline-parser
                                             (line-end-position)))
                                   while (outline-get-next-sibling)))))))

(defun +agenda-projects-get-heading-status (heading)
  (if-let ((children (+agenda-projects-list-child-headings heading)))
      (let* ((child-statuses (mapcar (lambda (child)
                                       (+agenda-projects-get-heading-status child))
                                     children)))
        (cond
         ((member 'deadline child-statuses) 'deadline)
         ((member 'waiting child-statuses) 'waiting)
         ((member 'next child-statuses) 'not-stuck)
         (t 'stuck)))
    ;; (org-element-put-property heading :status status))

    (let* ((todo-type (org-element-property :todo-type heading))
           (keyword (org-element-property :todo-keyword heading))
           (scheduled (org-element-property :scheduled heading))
           (deadline (org-element-property :deadline heading)))
      (cond
       ((eq todo-type 'done) 'not-todo)
       (deadline 'deadline)
       (scheduled 'scheduled)
       ((string-equal keyword "NEXT") 'next)
       ((string-equal keyword "WAITING") 'waiting)
       ((string-equal keyword "TODO") 'stuck)))))
;; (org-element-put-property heading :status status))))

(defun +agenda-format-heading (heading depth)
  "Formats HEADING as an agenda entry at DEPTH."
  (catch 'not-todo
    (let* ((text (org-element-property :raw-value heading))
           (keyword (org-element-property :todo-keyword heading))
           (status (+agenda-projects-get-heading-status heading))
           (face (pcase status
                   ('deadline 'all-the-icons-red)
                   ('waiting 'all-the-icons-blue)
                   ('stuck 'all-the-icons-purple)
                   ('not-todo (throw 'not-todo nil))
                   (default 'default))))
      (format "%s%s %s\n"
              (make-string (* 2 depth) 32)
              keyword
              (org-add-props text nil 'face face 'font-lock-fontified t)))))

(defun +agenda-projects-process-entry (element depth)
  (let ((formatted-headline (+agenda-format-heading element depth)))
    (if formatted-headline (insert formatted-headline)))
  (dolist (child (+agenda-projects-list-child-headings element))
    (+agenda-projects-process-entry child (1+ depth))))


(defun +agenda-projects-block (_)
  "Format a custom GTD agenda."
  ;; TODO Remove different display-buffer-alist upon completion.
  ;; (let ((display-buffer-alist tmp-org-agenda-display-buffer-alist))
  ;;   (org-agenda-prepare))
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

(general-define-key :keymaps 'override "<f1>" (lambda ()
                                                (interactive)
                                                ;; (+agenda-projects-block nil)
                                                (let ((org-agenda-custom-commands
                                                       '(("g" "Get Things Done (GTD)"
                                                          ((+agenda-projects-block nil))))))
                                                  (org-agenda nil "g"))))
