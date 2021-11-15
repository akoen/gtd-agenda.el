;;; gtd-agenda.el --- custom org-agenda, inspired by GTD -*- ;; lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Alex Koen <akoen@mailbox.org>
;; Maintainer: Alex Koen <akoen@mailbox.org>
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

;; Inspired by the following projects:
;; https://github.com/weirdNox/dotfiles/blob/26c5c2739aff28af5ed4d6f243c7ec0e9b581821/config/.emacs.d/config.org#agenda
;; https://github.com/pestctrl/emacs-config/blob/84c557982a860e86d6f67976a82ea776a7bd2c7a/config-org-new.org#my-own-agenda-renderer

;;; Code:

;; TODO Shouldn't have 'default status - should handle all cases

(straight-use-package 'org-ql)
(require 'org-ql-view)

(defvar +agenda-show-all nil
  "Shoe project tasks with inactive keywords.")

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

(defun +agenda-children (parent)
  "Return each subheading of PARENT, formatted as an org-element with markers."
  ;; TODO Should not depend on (org-ql--add-markers).
  ;; NOTE: See https://github.com/alphapapa/org-ql/issues/41 for a potential
  ;; implementation of this function in org-ql.
  (if-let ((children (org-element-property :children parent)))
      children
    (progn
      (save-excursion
        (let ((marker (org-element-property :org-marker parent)))
          (set-buffer (marker-buffer marker))
          (goto-char marker))
        (save-restriction
          (org-narrow-to-subtree)
          (outline-show-all)
          (when (org-goto-first-child)
            (let ((children
                   (cl-loop collect (org-ql--add-markers
                                     (org-element-headline-parser
                                      (line-end-position)))
                            while (outline-get-next-sibling))))
              ;; (org-element-put-property :children children)
              children
              )))))))

(defun +agenda-root-task-status (heading)
  "Compute the status of root-level task HEADING (no children)."
  (cl-assert (not (+agenda-children heading)))
  (cl-macrolet ((task-prop (prop)
                  `(org-element-property ,prop heading)))
    (cond
     ((eq (task-prop :todo-type) 'done)                  'done)
     ((task-prop :deadline)                              'deadline)
     ((task-prop :scheduled)                             'scheduled)
     ((string-equal (task-prop :todo-keyword) "NEXT")    'next)
     ((string-equal (task-prop :todo-keyword) "WAITING") 'waiting)
     ((string-equal (task-prop :todo-keyword) "TODO")    'inactive)
     (t                                                  'default))))

(defun +agenda-projects-get-heading-status (heading)
  "Return the status of GTD task HEADING."
  (if-let ((status (org-element-property :status heading)))
      status
    (let*
        ((children (+agenda-children heading))
         (status
          (if (not children)
              (+agenda-root-task-status heading)
            (let*
                ((keyword (org-element-property :todo-keyword heading))
                 (child-statuses (mapcar (lambda (child) (+agenda-projects-get-heading-status child)) children))
                 (children-all-done (cl-every (lambda (status) (eq 'done status)) child-statuses))
                 (children-all-inactive (cl-every (lambda (status) (member status '(inactive stuck done))) child-statuses)))
              (cond
               ((member 'deadline child-statuses) 'deadline)
               ((member 'next child-statuses)     'not-stuck)
               ((member 'waiting child-statuses)  'waiting)
               ((and
                 children-all-done
                 (string-equal keyword "DONE"))   'done)
               (children-all-inactive             'stuck)
               (t                                 'default))))))
      (org-element-put-property heading :status status)
      status)))

;; Visual formatting

(defun +agenda-add-status-face (heading)
  (let* ((text (org-element-property :raw-value heading))
         (keyword (org-element-property :todo-keyword heading))
         (status (+agenda-projects-get-heading-status heading))
         (face (pcase status
                 ('deadline '+agenda-deadline-face)
                 ('scheduled 'org-scheduled)
                 ('waiting '+agenda-waiting-face)
                 ('inactive 'org-agenda-dimmed-todo-face)
                 ('stuck '+agenda-stuck-face)
                 ('done 'org-done)
                 ;; ('default 'default)
                 ))
         (title (--> (org-element-property :raw-value element)
                     (org-add-props it nil 'face face))))
    (org-element-put-property element :title title)))


(defun +agenda-format-heading (element depth prefix)
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
           (title (--> (org-ql-view--add-faces element)
                       (+agenda-add-status-face it)
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
      (--> string
           (concat prefix it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(defun +agenda-task-prefix (parent-last)
  "Format prefix arrows for task. PARENT-LAST is a list of booleans
'(b0 b1 ... bn) where b1 indicates whether the nth ancestor of the
task is the last of its siblings."
  (let ((lastp (car parent-last))
        (parent-continuations (reverse (cdr parent-last))))
    (if (zerop depth)
        ""
      (concat
       "   "
       (mapconcat (lambda (lastp) (if lastp "   " "│  ")) (cdr parent-continuations) nil)
       ;; (make-string depth 32)
       (if lastp "╰⮞" "├⮞")))))

;; Sorting

(defun +agenda-sort-pred (h1 h2)
  "Returns t if task H1 should appear before H2."
  (let ((first-task
         (or
          (+agenda-compare-priority h1 h2)
          (+agenda-compare-status h1 h2))))
    (if (eq first-task -1)
        nil
      t)))

(defun +agenda-compare-priority (h1 h2)
  (cl-macrolet ((priority (item)
                  `(org-element-property :priority ,item)))
    (let ((h1-priority (priority h1))
          (h2-priority (priority h2)))
      (cond ((and h1-priority h2-priority)
             (if (<= h1-priority h2-priority) 1 -1))
            (h1-priority 1)
            (h2-priority -1)))))

(defvar +agenda-status-priority
  '(stuck scheduled deadline next not-stuck waiting default inactive done))

(defun +agenda-compare-status (t1 t2)
  "Return t if T1 has a more pressing status than T2.

See also `+agenda-status-priority'."
  (let ((t1-status (+agenda-projects-get-heading-status t1))
        (t2-status (+agenda-projects-get-heading-status t2)))
    (if (<= (cl-position t1-status +agenda-status-priority)
            (cl-position t2-status +agenda-status-priority))
        1
      -1)))


;; Main entry

(defun +agenda-projects-block (_)
  "Format a custom GTD agenda."
  (let ((inhibit-read-only t)
        (elements (org-ql-select
                    (org-agenda-files)
                    '(and (todo)
                          (children (or (todo) (done)))
                          (not (parent (todo))))
                    :action #'element-with-markers)))
    (goto-char (point-max))

    ;; Overriding header
    (insert (org-add-props "\nPROJECTS\n" nil 'face 'org-agenda-structure) "\n")

    ;; Contents
    (+agenda-insert-tasks elements 0 nil)
    (insert "\n")))

(defun +agenda-insert-tasks (tasks depth parent-last)
  "Insert TASKS and, recursively, their children, into the agenda
buffer at DEPTH."
  (let ((active-tasks
         (-> (if +agenda-show-all
                 tasks
               (seq-filter (lambda (task)
                             (not (member (+agenda-projects-get-heading-status task)
                                          '(done inactive))))
                           tasks))
             (sort #'+agenda-sort-pred))))
    (dolist (task active-tasks)
      (let* ((status (+agenda-projects-get-heading-status task))
             (children (+agenda-children task))
             (parent-last (append (->>
                                   (last active-tasks)
                                   (car)
                                   (eq task)
                                   (list))
                                  parent-last))
             (prefix (+agenda-task-prefix parent-last))
             (formatted-headline (+agenda-format-heading task depth prefix)))
        (unless (and (member status '(done inactive)) (not +agenda-show-all))
          (when (eq depth 0) (insert "\n")) ;; Space out top-level projects
          (insert formatted-headline "\n")
          (+agenda-insert-tasks children (1+ depth) parent-last))))))


(provide 'gtd-agenda)
;;; gtd-agenda.el ends here
