(setq user-full-name "Mathias Nielsen")

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package benchmark-init)

(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
      ns-option-modifier 'super
      ns-right-option-modifier 'none))

(use-package recentf
  :straight (:type built-in)
  :init
  :config
  (recentf-mode 1))

(use-package simple
  :straight (:type built-in)
  :init
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name ".local/auto-save/" user-emacs-directory)))))

(let ((font (font-spec :family "Source Code Pro" :size 12)))
  (setq default-frame-alist `((top . 120)
			      (left . 400)
			      (width . 128)
			      (height . 50)
			      (font . "Fira Code-14"))))

(use-package tool-bar
  :straight (:type built-in)
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :straight (:type built-in)
  :config
  (scroll-bar-mode -1))

;; Doom Themes
  (use-package doom-themes
    :defer t
    :init
    (add-hook 'after-init-hook (lambda () (load-theme
    'doom-palenight t nil))))

(use-package modus-operandi-theme
  :init
  (setq ))

(use-package doom-modeline)

(use-package smartparens
  :defer t
  :hook (prog-mode . turn-on-smartparens-mode)
  (text-mode . turn-on-smartparens-mode)
  (minibuffer-setup . turn-on-smartparens-mode)
  :config
  (with-eval-after-load 'text-mode
    (require 'smartparens-text))
  ;; disable ' char in lisp mode
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  (show-smartparens-global-mode 1))

(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  (text-mode . company-mode)
  :init
  (setq company-show-numbers t
	company-idle-delay 0.0)
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))

(use-package yasnippet
  :defer t
  :init
  (setq yas-snippet-dirs (expand-file-name ".snippets/" user-emacs-directory)))

(use-package helpful
  :defer t
  :init
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable)
  (with-eval-after-load 'evil
    (setq evil-lookup-func #'helpful-at-point)))

(use-package ivy
  :defer t
  :init
  ;; Set initial input to nil
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
    :after ivy counsel
    :init
    (setq ivy-rich-display-transformers-list '(ivy-switch-buffer
					       (:columns
						((ivy-switch-buffer-transformer (:width 30))    ; add face by the original transformer
						 (ivy-rich-switch-buffer-size (:width 7))  ; return buffer size
						 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))  ; return buffer indicator
						 (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))            ; return major mode info
						 (ivy-rich-switch-buffer-project (:width 15 :face success))               ; return project name `projectile'
						 (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
						:predicate
						(lambda (cand) (get-buffer cand)))
					       counsel-find-file
					       (:columns
						((ivy-read-file-transformer)
						 (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
					       counsel-M-x
					       (:columns
						((counsel-M-x-transformer (:width 40))
						 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
					       counsel-describe-function
					       (:columns
						((counsel-describe-function-transformer (:width 40))
						 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return docstring of the function
					       counsel-describe-variable
					       (:columns
						((counsel-describe-variable-transformer (:width 40))
						 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return docstring of the variable
					       counsel-recentf
					       (:columns
						((ivy-rich-candidate (:width 0.8))
						 (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))  ; return last modified time of the file
					       package-install
					       (:columns
						((ivy-rich-candidate (:width 30))
						 (ivy-rich-package-version (:width 16 :face font-lock-comment-face))  ; return package version
						 (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))  ; return archive summary
						 (ivy-rich-package-install-summary (:face font-lock-doc-face))))))  ; return package description
	  :config
	  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x b" . counsel-switch-buffer)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h b" . counsel-descbinds)
  :config
  (setq ivy-initial-inputs-alist nil))

;;;###autoload
(defun mnie/escape ()
  "Do what I mean when I press ESCAPE."
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 (minibuffer-keyboard-quit))
	((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'mnie/escape)

(use-package general)


(defmacro set-leader-keys! (&rest keys)
  "Set add keys to leader-key"
 `(general-define-key
:prefix "SPC"
 :non-normal-prefix "M-SPC"
 :states '(normal motion insert replace emacs)
 :keymaps 'override
 ,@keys))

(defmacro set-localleader-keys! (map &rest keys)
  "Set add keys to leader-key"
 `(general-define-key
 :prefix "SPC m"
 :non-normal-prefix "M-SPC m"
 :states '(normal motion insert replace emacs)
 :keymaps ,map
 ,@keys))

(set-leader-keys!
 "w" '(:ignore t :wk "window")
 "wd" 'delete-window
 "wh" 'evil-window-left)

(general-define-key
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 :states '(normal motion insert replace emacs)
 :keymaps 'override
 "b" '(:ignore t :wk "buffer")
  "bb" 'counsel-switch-buffer
  "bd" 'kill-current-buffer)

(let ((bindings '(("f" "" "files") ("fs" "save-buffer" "") ("ff" "find-file" ""))))
(let (keys
        key
        function
        wk)
    (dolist (binding bindings keys)
      (setq key (car binding))
      (if (eq (cadr binding) "")
          (setq function '(:ignore t))
        (setq function (intern (cadr binding))))
      (setq wk (nth 2 binding))
      (unless (or (eq wk nil)
               (eq wk ""))
        (setq function (append function `(:wk ,wk)))))
      (add-to-list 'keys key t)
      (add-to-list 'keys function t))
    )
)

(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-want-integration t
	evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
:after evil
:config
(evil-escape-mode 1))

(use-package evil-snipe)

(use-package which-key
  :config
  (which-key-mode 1))

(let ((keys '(("C-x g g" "magit-status"))))
(mapc (lambda (k) (evil-define-key nil 'global (kbd (nth 0 k)) (intern (nth 1 k)))) keys)
)

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status))

(use-package evil-magit
  :after magit)

(use-package elisp-mode
  :straight (:type built-in))

(use-package text-mode
  :straight (:type built-in))
