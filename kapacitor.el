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

(defvar kapacitor-url "http://localhost:9092"
  "Current configured kapacitor server url.")

(defvar kapacitor-url-list (list kapacitor-url)
  "List of kapacitor server urls.")

(defconst kapacitor-buffer-name "*kapacitor*")

(defgroup kapacitor nil
  "A magit like interface for Kapacitor."
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
    (define-key keymap (kbd "RET") #'kapacitor-show-task)
    (define-key keymap (kbd "c")   #'kapacitor-set-url)
    (define-key keymap (kbd "d")   #'kapacitor-disable-task)
    (define-key keymap (kbd "e")   #'kapacitor-enable-task)
    (define-key keymap (kbd "D")   #'kapacitor-delete-task)

    ;; popups
    (define-key keymap (kbd "?") #'kapacitor-overview-popup)
    (define-key keymap (kbd "S") #'kapacitor-show-stats-popup)

    keymap)
  "Keymap for `kapacitor-mode'." )

;;;; Functions
;;;;; Kapacitor api

(defun kapacitor-process-kill-quietly (proc)
  "Kill kapacitor sentinel process PROC quitely."
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defun kapacitor-curl-ep (ep method on-success &optional on-error data)
  "Curl the endpoint EP with METHOD and call ON-SUCCESS if the exit code is 0.

call ON-ERROR on any other exit code.  Both callbacks will receive
the result of the call as an argument.  DATA will be sent in request body as json."
  (let* ((buf (generate-new-buffer " kapacitor"))
         (err-buf (generate-new-buffer " kapacitor-err"))
         (command (list "curl"
                        "-H" "Content-Type: application/json"
                        "-d" (json-encode data)
                        "-X" method
                        (concat kapacitor-url ep))))
    (make-process
     :name "kapacitor"
     :buffer buf
     :stderr err-buf
     :command command
     :noquery t
     :connection-type 'pipe'
     :sentinel
     (lambda (proc _)
       (unwind-protect
           (let* ((exit-code (process-exit-status proc)))
             (cond
              ((zerop exit-code)
               (funcall on-success buf))
              (t
               (if on-error
                   (funcall on-error err-buf)
                 (message (format "%s Failed with exit code %d" command exit-code))))))
         (kapacitor-process-kill-quietly proc))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (ignore-errors (kill-buffer err-buf))))))

(defun kapacitor-get-tasks (cb &optional cb-err)
  "Fetch all tasks and call CB with resulting json string.

On error call CB-ERR with err buffer."
  (kapacitor-curl-ep "/kapacitor/v1/tasks?fields=executing&fields=status&fields=type"
                     "GET"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))
                     cb-err))

(defun kapacitor-get-task-info (cb taskid)
  "Fetch task info for TASKID and call CB with resulting json."
  (kapacitor-curl-ep (concat "/kapacitor/v1/tasks/" taskid)
                     "GET"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-get-debug-vars (cb)
  "Fetch debug vars and call CB with resulting json string."
  (kapacitor-curl-ep "/kapacitor/v1/debug/vars"
                     "GET"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-patch-task (cb taskid body)
  "Patch a TASKID with given BODY and call CB."
  (kapacitor-curl-ep (concat "/kapacitor/v1/tasks/" taskid)
                     "PATCH"
                     (lambda (_)
                       (funcall cb))
                     nil
                     body))

(defun kapacitor--delete-task (cb taskid)
  "Delete a given TASKID and call CB."
  (kapacitor-curl-ep (concat "/kapacitor/v1/tasks/" taskid)
                     "DELETE"
                     (lambda (_)
                       (funcall cb))))

;;;;; kapacitor-mode functions

(defun kapacitor-overview-refresh ()
  "Refresh kapacitor overview."
  (interactive)
  (kapacitor-overview))

(defun kapacitor--format-task-line (task)
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
     (format "\n%-60s %-8s %-9s %-4s"
             id
             (propertize type 'face 'magit-dimmed)
             (propertize status 'face 'magit-dimmed)
             (propertize executing 'face task-face))
     'kapacitor-nav id)))

(defun kapacitor-populate-tasks (response)
  "Populate kapacitor tasks RESPONSE."
  (let* ((buf (get-buffer-create kapacitor-buffer-name)))
    (with-current-buffer buf
      (let* ((tasks (cdr-safe (assoc 'tasks response)))
             (task-lines (mapcar #'kapacitor--format-task-line tasks))
             (inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-11s : %s\n" "Server" kapacitor-url))
        (insert (format "%-11s : %d\n" "Total Tasks" (seq-length tasks)))
        (insert (propertize (format "%-60s %-8s %-9s %-4s"
                                    "ID" "Type" "Status" "Executing")
                            'face 'magit-section-heading))
          (dolist (task-line task-lines)
            (insert task-line))

        (kapacitor-mode)
        (pop-to-buffer buf)
        (goto-char (point-min))))))

(defun kapacitor-show-task ()
  "Run kapacitor show task command on task at point."
  (interactive)
  (let* ((taskid (get-text-property (point) 'kapacitor-nav)))
    (if taskid
        (kapacitor-get-task-info 'kapacitor-populate-task-info taskid )
      (message "No task under point"))))

(defun kapacitor-maybe-fontify-tickscript (script)
  "If available, return syntax highlighted SCRIPT with ‘tickscript-mode’."
  (if (featurep 'tickscript-mode)
      (with-temp-buffer
        (insert script)
        (delay-mode-hooks (tickscript-mode))
        (font-lock-default-function 'tickscript-mode)
        (font-lock-default-fontify-region (point-min) (point-max) nil)
        (buffer-string))
    script))

(defun kapacitor-populate-task-info (response)
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

(defun kapacitor-show-stats-general ()
  "Show kapacitor general stats in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*kapacitor stats general*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize "Loading..." 'face 'magit-dimmed))
      (pop-to-buffer buf))
    (kapacitor-get-debug-vars 'kapacitor-populate-stats-general)))

(defun kapacitor-populate-stats-general (response)
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

(defun kapacitor-set-url (&rest _)
  "Set `kapacitor-url`."
  (interactive)
  (let* ((server-url (completing-read "Server URL: " kapacitor-url-list)))
    (if (member server-url kapacitor-url-list)
        (setq kapacitor-url server-url)
      (push server-url kapacitor-url-list)))
  (kapacitor-overview t))

(defun kapacitor-disable-task ()
  "Disable kapacitor task under point."
  (interactive)
  (let* ((taskid (get-text-property (point) 'kapacitor-nav)))
    (if taskid
          (kapacitor-patch-task 'kapacitor-overview-refresh taskid '(("status" . "disabled")))
      (message "No task under point"))))

(defun kapacitor-enable-task ()
  "Enable kapacitor task under point."
  (interactive)
  (let* ((taskid (get-text-property (point) 'kapacitor-nav)))
    (if taskid
        (kapacitor-patch-task 'kapacitor-overview-refresh taskid '(("status" . "enabled")))
      (message "No task under point"))))

(defun kapacitor-delete-task ()
  "Delete kapacitor task under point."
  (interactive)
  (let* ((taskid (get-text-property (point) 'kapacitor-nav)))
    (if taskid
        (kapacitor--delete-task 'kapacitor-overview-refresh taskid)
      (message "No task under point"))))

;;;;; magit popups

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
  '("Environment"
    (?c "Change server" kapacitor-set-url)
    "Popup commands"
    (?S "Stats" kapacitor-show-stats-popup)
    "Task Commands"
    (?\r  "Show"    kapacitor-show-task)
    (?d   "Disable" kapacitor-disable-task)
    (?e   "Enable"  kapacitor-enable-task)
    (?D   "Delete"  kapacitor-delete-task)))


;;;;; Commands

;;;###autoload
(defun kapacitor-overview (&optional quiet)
  "Display kapacitor overview in a buffer.

If QUIET is set then additional questions are not asked in case
the server is not reachable."

  (interactive)
  (if quiet
      (kapacitor-get-tasks 'kapacitor-populate-tasks)
    (kapacitor-get-tasks 'kapacitor-populate-tasks 'kapacitor-set-url)))

;;;###autoload
(define-derived-mode kapacitor-mode special-mode "Kapacitor"
  "Base mode for Kapacitor modes.

\\{kapacitor-mode-map}"
  :group 'kapacitor
  (buffer-disable-undo))

(provide 'kapacitor)

;;; kapacitor.el ends here
