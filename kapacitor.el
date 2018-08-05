;;; kapacitor.el --- Main file for kapacitor-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand <manojm.321@gmail.com>
;; URL: http://github.com/Manoj321/kapacitor-el
;; Keywords: kapacitor, emacs, magit, tools
;; Package-Requires: ((emacs "25.1") (magit "2.13.0") (magit-popup "2.12.4"))
;; Version: 0.0.1

;;; Commentary:
;; A magit like interface for kapacitor

;;; Code:
;;;; Dependencies

(require 'json)
(require 'magit)
(require 'magit-popup)
(require 'subr-x)

;;; Options
;;;; Customizations

(defcustom kapacitor-url "http://localhost:9092"
  "The kapacitor server url."
  :group 'kapacitor
  :type 'string)

(defconst kapacitor-buffer-name "*kapacitor*")

(defgroup kapacitor nil
  "Emacs porcelain for Kapacitor."
  :group 'tools
  :prefix "kapacitor-")

;;;; Variables
;;;;; Keymap

;;;###autoload
(defvar kapacitor-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "p")   #'magit-section-backward)
    (define-key keymap (kbd "n")   #'magit-section-forward)
    (define-key keymap (kbd "M-p") #'magit-section-backward-sibling)
    (define-key keymap (kbd "M-n") #'magit-section-forward-sibling)
    (define-key keymap (kbd "C-i") #'magit-section-toggle)
    (define-key keymap (kbd "^")   #'magit-section-up)
    (define-key keymap [tab]       #'magit-section-toggle)
    (define-key keymap (kbd "q")   #'quit-window)
    (define-key keymap (kbd "g")   #'kapacitor-overview-refresh)
    (define-key keymap (kbd "RET") #'kapacitor-show-task-info)

    ;; popups
    (define-key keymap (kbd "?") #'kapacitor-overview-popup)
    (define-key keymap (kbd "s") #'kapacitor-show-task-popup)
    (define-key keymap (kbd "S") #'kapacitor-show-stats-popup)

    keymap)
  "Keymap for `kapacitor-mode'." )

;;;; Functions
;;;;; Kapacitor api

(defun kapacitor-process-kill-quietly(proc)
  "Kill kapacitor sentinel process PROC quitely."
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defun kapacitor-curl-ep(ep on-success)
  "Curl the endpoint EP and call ON-SUCCESS if the exit code is 0."
  (let* ((buf (generate-new-buffer " kapacitor"))
         (err-buf (generate-new-buffer " kapacitor-err"))
         (command (list "curl" (concat kapacitor-url ep))))
         (make-process
                :name "kapacitor"
                :buffer buf
                :stderr err-buf
                :command command
                :noquery t
                :connection-type 'pipe'
                :sentinel
                (lambda(proc _)
                  (unwind-protect
                      (let* ((exit-code (process-exit-status proc)))
                        (cond
                         ((zerop exit-code)
                          (funcall on-success buf))
                         (t
                          (message (format "Failed with exit code %d" exit-code)))))
                    (kapacitor-process-kill-quietly proc))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (ignore-errors (kill-buffer err-buf))))))

(defun kapacitor-get-tasks(cb)
  "Fetch all tasks and call CB with resulting json string."
  (kapacitor-curl-ep "/kapacitor/v1/tasks?fields=executing&fields=status&fields=type"
                     (lambda(buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-get-task-info(cb taskid)
  "Fetch task info for TASKID and call CB with resulting json."
  (kapacitor-curl-ep (concat "/kapacitor/v1/tasks/" taskid)
                     (lambda(buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-get-debug-vars(cb)
  "Fetch debug vars and call CB with resulting json string."
  (kapacitor-curl-ep "/kapacitor/v1/debug/vars"
                     (lambda(buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

;;;;; kapacitor-mode functions

(defun kapacitor-overview-refresh()
  "Refresh kapacitor overview."
  (kapacitor-overview))

(defun kapacitor--format-task-line(task)
  "Format a given TASK to be diplayed in ‘kapacitor-overview’."
  (let ((id (cdr-safe (assoc 'id task)))
        (type (cdr-safe (assoc 'type task)))
        (status (cdr-safe (assoc 'status task)))
        (executing (cdr-safe (assoc 'executing task)))
        (task-face 'magit-dimmed))
    (pcase executing
      ('t (setq executing "true"))
      (:json-false (setq executing "false" task-face 'warning)))
    (propertize
     (format "%-60s %-8s %-9s %-4s\n"
             id
             (propertize type 'face 'magit-dimmed)
             (propertize status 'face 'magit-dimmed)
             (propertize executing 'face task-face))
     'kapacitor-nav id)))

(defun kapacitor-populate-tasks(response)
  "Populate kapacitor tasks RESPONSE."
  (let* ((buf (get-buffer-create kapacitor-buffer-name)))
    (with-current-buffer buf
      (let* ((tasks (cdr-safe (assoc 'tasks response)))
             (task-lines (mapcar #'kapacitor--format-task-line tasks))
             (inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-11s : %s\n" "Server" kapacitor-url))
        (insert (format "%-11s : %d\n" "Total Tasks" (seq-length tasks)))
        (insert (propertize (format "%-60s %-8s %-9s %-4s\n"
                                    "ID" "Type" "Status" "Executing")
                            'face 'magit-section-heading))
          (dolist (task-line task-lines)
            (insert task-line))

        (kapacitor-mode)
        (pop-to-buffer buf)
        (goto-char (point-min))))))

(defun kapacitor-show-task-info()
  "Run kapacitor show task command on task at point."
  (interactive)
  (let* ((taskid (get-text-property (point) 'kapacitor-nav)))
    (if taskid
        (kapacitor-get-task-info 'kapacitor-populate-task-info taskid ))))

(defun kapacitor-maybe-fontify-tickscript(script)
  "If available, return syntax highlighted SCRIPT with ‘tickscript-mode’."
  (if (featurep 'tickscript-mode)
      (with-temp-buffer
        (insert script)
        (delay-mode-hooks (tickscript-mode))
        (font-lock-default-function 'tickscript-mode)
        (font-lock-default-fontify-region (point-min) (point-max) nil)
        (buffer-string))
    script))

(defun kapacitor-populate-task-info(response)
  "Populate given task info RESPONSE into a buffer."
  (let* ((task-buf (get-buffer-create "*kapacitor task info*")))
    (with-current-buffer task-buf
      (let* ((inhibit-read-only t)
             (id (cdr-safe (assoc 'id response)))
             (taskerror (cdr-safe (assoc 'error response)))
             (type (cdr-safe (assoc 'type response)))
             (status (cdr-safe (assoc 'status response)))
             (executing (cdr-safe (assoc 'executing response)))
             (created (cdr-safe (assoc 'created response)))
             (modified (cdr-safe (assoc 'modified response)))
             (last-enabled (cdr-safe (assoc 'last-enabled response)))
             (dbrps (aref (cdr-safe (assoc 'dbrps response)) 0))
             (script (cdr-safe (assoc 'script response)))
             (dot (cdr-safe (assoc 'dot response)))
             (db (cdr-safe (assoc 'db dbrps)))
             (rp (cdr-safe (assoc 'rp dbrps))))
        (pcase executing
          ('t (setq executing "true"))
          (:json-false (setq executing "false")))
        (erase-buffer)
        (insert (format "ID          : %s\n" id))
        (insert (format "ERROR       : %s\n" taskerror))
        (insert (format "Type        : %s\n" type))
        (insert (format "Status      : %s\n" status))
        (insert (format "Executing   : %s\n" executing))
        (insert (format "Created     : %s (%s)\n"
                        (format-time-string "%FT%T %Z" (date-to-time created))
                        (format-time-string "%FT%T %Z"
                                            (date-to-time created) "UTC")))
        (insert (format "Modified    : %s (%s)\n"
                        (format-time-string "%FT%T %Z" (date-to-time modified))
                        (format-time-string "%FT%T %Z"
                                            (date-to-time modified) "UTC")))
        (insert (format "LastEnabled : %s (%s)\n"
                        (format-time-string "%FT%T %Z" (date-to-time last-enabled))
                        (format-time-string "%FT%T %Z"
                                            (date-to-time last-enabled) "UTC")))
        (insert (format "DBRP        : \"%s\".\"%s\"\n" db rp))
        (insert "TICKscript:\n")
        (insert (kapacitor-maybe-fontify-tickscript script))
        (insert "DOT:\n")
        (insert dot)
        (goto-char (point-min))
        (view-mode)
        (pop-to-buffer task-buf)))))

(defun kapacitor-show-stats-general()
  "Show kapacitor general stats in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*kapacitor stats general*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize "Loading..." 'face 'magit-dimmed))
      (pop-to-buffer buf))
    (kapacitor-get-debug-vars 'kapacitor-populate-stats-general)))

(defun kapacitor-populate-stats-general(response)
  "Populate kapacitor general stats RESPONSE."
  (let* ((buf (get-buffer-create "*kapacitor stats general*"))
         (cluster-id (cdr-safe (assoc 'cluster_id response)))
         (server-id (cdr-safe (assoc 'server_id response)))
         (host (cdr-safe (assoc 'host response)))
         (tasks (cdr-safe (assoc 'num_tasks response)))
         (enabled-tasks (cdr-safe (assoc 'num_enabled_tasks response)))
         (subscriptions (cdr-safe (assoc 'num_subscriptions response)))
         (version (cdr-safe (assoc 'version response)))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "ClusterID: %s\n" cluster-id))
      (insert (format "SeverID: %s\n" server-id))
      (insert (format "Host: %s\n" host))
      (insert (format "Tasks: %s\n" tasks))
      (insert (format "Enabled Tasks: %s\n" enabled-tasks))
      (insert (format "Subscriptions: %s\n" subscriptions))
      (insert (format "Version: %s\n" version))
      (goto-char (point-min))
      (view-mode)
      (pop-to-buffer buf))))

;;;;; magit popups

(magit-define-popup kapacitor-show-task-popup
  "Popup console for show command."
  :group 'kapacitor
  :actions
  '((?s "Show" kapacitor-show-task-info)))

(magit-define-popup kapacitor-show-stats-popup
  "Popup console for stats command."
  :group 'kapacitor
  :actions
  '((?g "general" kapacitor-show-stats-general)
    (?i "ingress" kapacitor-show-stats-ingress)))

(magit-define-popup kapacitor-overview-popup
  "Popup console for showing an overview of available popup commands."
  :group 'kapacitor
  :actions
  '("Popup commands"
    (?s "Show" kapacitor-show-task-popup)
    (?S "Stats" kapacitor-show-stats-popup)))

;;;;; Commands

;;;###autoload
(defun kapacitor-overview()
  "Display kapacitor overview in a buffer."
    (interactive)
    (kapacitor-get-tasks 'kapacitor-populate-tasks))

;;;###autoload
(define-derived-mode kapacitor-mode special-mode "Kapacitor"
  "Base mode for Kapacitor modes.

\\{kubernetes-mode-map}"
  :group 'kapacitor
  (buffer-disable-undo))

(provide 'kapacitor)

;;; kapacitor.el ends here
