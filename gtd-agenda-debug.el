
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
                                                (let ((org-agenda-files (directory-files "./" t "test-agenda-file\.org"))
                                                      (org-agenda-custom-commands
                                                       '(("g" "Get Things Done (GTD)"
                                                          ((+agenda-projects-block nil))))))
                                                  (org-agenda nil "g"))))

(general-define-key :keymaps 'override "<f2>" (lambda ()
                                                (interactive)
                                                (let ((org-agenda-files (directory-files "./" t "test-agenda-file\.org"))
                                                      (display-buffer-alist tmp-org-agenda-display-buffer-alist))
                                                  (org-agenda-prepare)
                                                  (+agenda-projects-block nil))))

;; Tracing
(trace-function '+agenda-projects-process-entry)
(trace-function '+agenda-projects-block)
(trace-function '+agenda-format-heading)
(set-popup-rule! "\\*trace-output\\*" :side 'right :width 0.3)

(provide 'gtd-agenda-debug)
