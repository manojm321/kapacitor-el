;;; kapacitor-api.el --- Functions to query kapacitor endpoints.
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'kapacitor-vars)
(require 'subr-x)

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

(defun kapacitor-curl-ep (ep on-success)
  "Curl the endpoint EP and call ON-SUCCESS if the exit code is 0."
  (let* ((buf (generate-new-buffer " kapacitor"))
         (err-buf (generate-new-buffer " kapacitor-err"))
         (command (list "curl" (concat kapacitor-url ep)))
         (proc (make-process
                :name "kapacitor"
                :buffer buf
                :stderr err-buf
                :command command
                :noquery t
                :connection-type 'pipe'
                :sentinel
                (lambda (proc status)
                  (unwind-protect
                      (let* ((exit-code (process-exit-status proc)))
                        (cond
                         ((zerop exit-code)
                          (funcall on-success buf))
                         (t
                          (message (format "Failed with exit code %d" exit-code)))))
                    (kapacitor-process-kill-quietly proc))))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (ignore-errors (kill-buffer err-buf))))))

(defun kapacitor-get-tasks (cb)
  "Fetch all tasks and call CB with resulting json string."
  (kapacitor-curl-ep "/kapacitor/v1/tasks?fields=executing&fields=status&fields=type"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-get-task-info (cb taskid)
  "Fetch task info for TASKID and call CB with resulting json."
  (kapacitor-curl-ep (concat "/kapacitor/v1/tasks/" taskid)
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun kapacitor-get-debug-vars (cb)
  "Fetch debug vars and call CB with resulting json string."
  (kapacitor-curl-ep "/kapacitor/v1/debug/vars"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(provide 'kapacitor-api)
;;; kapacitor-api ends here
