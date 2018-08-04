;;; kapacitor-vars.el --- Various kapacitor vars.    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand <manojm.321@gmail.com>
;; URL: http://github.com/Manoj321/kapacitor-el
;; Keywords: kapacitor, emacs, magit

;;; Commentary:

;;; Code:
(defcustom kapacitor-url "http://localhost:9092"
  "The kapacitor server url."
  :group 'kapacitor
  :type 'string)

(defconst kapacitor-buffer-name "*kapacitor*")

(provide 'kapacitor-vars)

;;; kapacitor-vars.el ends here
