;;; kapacitor-vars.el --- Various kapacitor vars.
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom kapacitor-url "http://localhost:9092"
  "The kapacitor server url."
  :group 'kapacitor
  :type 'string)

(defconst kapacitor-buffer-name "*kapacitor*")

(provide 'kapacitor-vars)
;;; kapacitor-vars ends here



