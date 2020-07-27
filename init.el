;;; init.el -- Init emacs -*- lexical-binding:t ; -*-
;;; Commentary:


;;; Code:

(setq gc-cons-threshold 50000000)

(add-hook 'after-init-hook (defun mnie/reset-gc-cons-threshold-h ()
			    "Reset gc-cons-threshold."
			    (setq gc-cons-threshold 800000)
			    (garbage-collect)))

;; Bootstrap Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-base-dir (expand-file-name ".local/" user-emacs-directory)
      straight-use-package-by-default t)

;; Setup Use-package
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-always-ensure nil)

;; Load configuration
(let ((file-name-handler-alist nil))
(load-file "~/.emacs.d/config.el"))

;;; init.el ends here
