;;; gtd-agenda.el --- custom org-agenda, inspired by GTD -*- ;; lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Alex Camarena <akoen@mailbox.org>
;; Maintainer: Alex Camarena <akoen@mailbox.org>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/akoen/gtd-agenda.el

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; https://github.com/pestctrl/emacs-config/blob/84c557982a860e86d6f67976a82ea776a7bd2c7a/config-org-new.org#my-own-agenda-renderer

;;; Code:

(straight-use-package 'org-ql)
(require 'org-ql-view)

(defface +agenda-deadline-face
  '((t :inherit org-upcoming-deadline
       :weight bold))
  "Face used for GTD deadlines."
  :group 'gtd-agenda)

(defface +agenda-stuck-face
  '((t :inherit org-upcoming-deadline
       :foreground "#800080"
       :weight bold))
  "Face used for GTD stuck projects."
  :group 'gtd-agenda)

(defface +agenda-waiting-face
  '((t :inherit org-upcoming-deadline
       :foreground "#0096FF"
       :weight bold))
  "Face used for GTD waiting projects."
  :group 'gtd-agenda)

(defun +agenda-projects-add-next-face (string)
  (let ((face 'org-priority))
    (org-add-props string nil 'face face 'font-lock-fontified t)))

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
  "Return the status of gtd task HEADING."
  (let* ((status (org-element-property :status heading))
         (children (+agenda-projects-list-child-headings heading))
         (child-statuses (mapcar (lambda (child) (+agenda-projects-get-heading-status child)) children))
         (children-all-done (cl-every (lambda (status) (eq 'done status)) child-statuses))
         (children-all-inactive (cl-every (lambda (status) (member status '(inactive stuck done))) child-statuses)))
    (cond
     ;; Status already calculated.
     (status status)
     ;; No sub-tasks: status determined by properties.
     ((or (not children) children-all-done)
      (let* ((todo-type (org-element-property :todo-type heading))
             (keyword (org-element-property :todo-keyword heading))
             (scheduled (org-element-property :scheduled heading))
             (deadline (org-element-property :deadline heading)))
        (cond
         ((eq todo-type 'done)             'done)
         (deadline                         'deadline)
         (scheduled                        'scheduled)
         ((string-equal keyword "NEXT")    'next)
         ((string-equal keyword "WAITING") 'waiting)
         ((string-equal keyword "TODO")    'inactive)
         (t                                'default))))
     ;; Sub-tasks: status determined by children
     (t (cond
         ((member 'deadline child-statuses) 'deadline)
         ((member 'next child-statuses)     'not-stuck)
         ((member 'waiting child-statuses)  'waiting)
         (children-all-inactive             'stuck)
         (t                                 'default))))))

(defun +agenda-add-status-face (heading)
  (let* ((text (org-element-property :raw-value heading))
         (keyword (org-element-property :todo-keyword heading))
         (status (+agenda-projects-get-heading-status heading))
         (face (pcase status
                 ('deadline '+agenda-deadline-face)
                 ('scheduled 'org-default)
                 ('waiting '+agenda-waiting-face)
                 ('next 'org-agenda-dimmed-todo-face)
                 ('stuck '+agenda-stuck-face)
                 ('done 'org-done)
                 ('default 'default)))
         (title (--> (org-element-property :raw-value element)
                     (org-add-props it nil 'face face))))
    (org-element-put-property element :title title)))


(defun +agenda-format-heading (element depth)
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not element)
      ""
    (let* ((properties (cadr element))
           ;; Remove the :parent property, which so bloats the size of
           ;; the properties list that it makes it essentially
           ;; impossible to debug, because Emacs takes approximately
           ;; forever to show it in the minibuffer or with
           ;; `describe-text-properties'.  FIXME: Shouldn't be necessary
           ;; anymore since we're not parsing the whole buffer.

           ;; Also, remove ":" from key symbols.  FIXME: It would be
           ;; better to avoid this somehow.  At least, we should use a
           ;; function to convert plists to alists, if possible.
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           ;; TODO: --add-faces is used to add the :relative-due-date property, but that fact is
           ;; hidden by doing it through --add-faces (which calls --add-scheduled-face and
           ;; --add-deadline-face), and doing it in this form that gets the title hides it even more.
           ;; Adding the relative due date property should probably be done explicitly and separately
           ;; (which would also make it easier to do it independently of faces, etc).
           (title (--> (+agenda-add-status-face element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (status (+agenda-projects-get-heading-status element))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                           (org-ql-view--add-todo-face it)))
           (tag-list (if org-use-tag-inheritance
                         ;; MAYBE: Use our own variable instead of `org-use-tag-inheritance'.
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               (org-with-wide-buffer
                                (goto-char marker)
                                (cl-loop for type in (org-ql--tags-at marker)
                                         unless (or (eq 'org-ql-nil type)
                                                    (not type))
                                         append type)))
                           ;; No marker found
                           ;; TODO: Use `display-warning' with `org-ql' as the type.
                           (warn "No marker found for item: %s" title)
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                              (char-to-string)
                              (format "[#%s]")
                              (org-ql-view--add-priority-face)))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (if (eq status 'done)
          ""
        (--> string
             (concat (make-string (* 2 depth) 32) it)
             (org-add-props it properties
               'org-agenda-type 'search
               'todo-state todo-keyword
               'tags tag-list
               'org-habit-p habit-property))))))

(defun +agenda-projects-process-entry (element depth)
  (let ((status (+agenda-projects-get-heading-status element))
        (formatted-headline (+agenda-format-heading element depth)))
    (unless (eq status 'done)
      (insert formatted-headline "\n")))
  (dolist (child (+agenda-projects-list-child-headings element))
    (+agenda-projects-process-entry child (1+ depth))))


(defun +agenda-projects-block (_)
  "Format a custom GTD agenda."
  (let ((inhibit-read-only t)
        (elements (org-ql-select
                    (org-agenda-files)
                    '(and (todo)
                          (children (todo))
                          (not (parent (todo))))
                    :action #'element-with-markers)))
    (goto-char (point-max))

    ;; Overriding header
    (insert (org-add-props "\nPROJECTS\n" nil 'face 'org-agenda-structure) "\n")

    ;; Contents
    (dolist (element elements)
      (+agenda-projects-process-entry element 1)
      (insert "\n"))))


;; Testing configuration
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
(general-define-key :keymaps 'override "<f1>" (lambda ()
                                                (interactive)
                                                ;; (+agenda-projects-block nil)
                                                (let ((org-agenda-files '("~/Programming/gtd-agenda/test-agenda-file.org"))
                                                      (org-agenda-custom-commands
                                                       '(("g" "Get Things Done (GTD)"
                                                          ((+agenda-projects-block nil))))))
                                                  (org-agenda nil "g"))))

(general-define-key :keymaps 'override "<f2>" (lambda ()
                                                (interactive)
                                                (let ((org-agenda-files '("~/Programming/gtd-agenda/test-agenda-file.org"))
                                                      (display-buffer-alist tmp-org-agenda-display-buffer-alist))
                                                  (org-agenda-prepare)
                                                  (+agenda-projects-block nil))))

(provide 'gtd-agenda)
;;; gtd-agenda.el ends here
