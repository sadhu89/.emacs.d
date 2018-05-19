(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; (auto-fill-mode 1)
;; (setq comment-auto-fill-only-comments 1)
(setq-default fill-column 100)

(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst sadhu-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; (desktop-save-mode 1)
(winner-mode 1)

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(add-hook 'prog-mode-hook 'linum-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;(toggle-frame-maximized)
;(setq ns-use-native-fullscreen nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; OSX keybindings
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)
(global-set-key (kbd "C-M-f") 'toggle-frame-maximized)
(global-set-key (kbd "s-f") 'counsel-grep-or-swiper)
;; (global-set-key (kbd "s-t") 'counsel-projectile-find-file)
(global-set-key (kbd "s-o") 'counsel-find-file)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-,") 'crux-find-user-init-file)
(global-set-key (kbd "s-P") 'counsel-M-x)
(global-set-key (kbd "s-p") 'counsel-projectile-find-file)
(global-set-key (kbd "C-s-<down>") 'move-text-down)
(global-set-key (kbd "C-s-<up>") 'move-text-up)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-b") 'ivy-switch-buffer)
(global-set-key (kbd "s-`") 'other-frame)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "s-3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "s-l") 'goto-line)
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
(setq insert-directory-program (executable-find "gls")
      ;; https://oremacs.com/2015/01/13/dired-options/
      dired-listing-switches "-laGh1v --group-directories-first")

;; extend the help commands
(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t )

(defun save-buffers-kill-emacs-only-in-console ()
  "In a GUI environment, do nothing; otherwise `save-buffers-kill-emacs`."
  (interactive)
  (if (display-graphic-p)
      (message "save-buffers-kill-emacs disabled for graphical displays.")
    (save-buffers-kill-emacs)))

(defun suspend-frame-only-in-console ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Unable key bindings that close emacs
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs-only-in-console)
(global-set-key (kbd "C-z") 'suspend-frame-only-in-console)
(global-set-key (kbd "C-x C-z") 'suspend-frame-only-in-console)

;;(setq special-display-buffer-names '("*rspec-compilation*" "*guard*"))

(setq initial-major-mode 'ruby-mode)
(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(setq initial-scratch-message nil)

;; "prefer" side-by-side window splits over stacked ones
;; https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package counsel
  :ensure smex
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  ;(global-set-key (kbd "C-r") 'swiper)
  ;(global-set-key "\C-s" 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package wgrep
  :ensure t)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

;; (use-package spacemacs-common
;;     :ensure spacemacs-theme
;;     :config (load-theme 'spacemacs-dark t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-dark t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties t))

;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t))

;; (use-package monokai-theme
;;   :ensure t
;;   :config
;;   (load-theme 'monokai t))

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))


;(setq default-frame-alist '((font . "Source Code Pro-11")))
(add-hook 'prog-mode-hook 'mac-auto-operator-composition-mode)
(setq default-frame-alist '((font . "Fira Code-11")))

;; highlight the current line
(global-hl-line-mode +1)

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s->" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-M-<tab>" . magit-section-cycle)))

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme))

(use-package github-browse-file
  :ensure t)

(use-package projectile
  :ensure t
  :bind ("C-s-p" . projectile-command-map)
  :diminish (projectile-mode . "Pjtl");; diminish projectile mode to
                                      ;; work around
                                      ;; https://github.com/bbatsov/projectile/issues/1183
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired))

(defun counsel-projectile-find-file-occur ()
    (cd (projectile-project-root))
    (counsel-cmd-to-dired
     (counsel--expand-ls
      (format
       "find . | grep -i -E '%s' | xargs ls"
       (counsel-unquote-regex-parens ivy--old-re)))))

(use-package counsel-projectile
  :ensure t
  :bind* ("s-F" . counsel-projectile-rg)
  :init
  (counsel-projectile-mode)
  :config
  (ivy-set-occur 'counsel-projectile-find-file 'counsel-projectile-find-file-occur)
  (ivy-set-occur 'counsel-projectile 'counsel-projectile-find-file-occur))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind
  (("C-s-f" . sp-forward-sexp)
   ("C-s-b" . sp-backward-sexp))
  :init
  (progn
    (require 'smartparens-config)
    (sp-use-paredit-bindings)
    (smartparens-global-mode 1))
  :config
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode))

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (put 'paredit-forward-delete 'delete-selection 'supersede)
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;   ;; enable in the *scratch* buffer
;;   (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
;;   (add-hook 'ielm-mode-hook #'paredit-mode)
;;   (add-hook 'lisp-mode-hook #'paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" sadhu-savefile-dir))
  (save-place-mode 1))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" sadhu-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" sadhu-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package ace-window
  :ensure t
  :bind* ("C-<tab>" . ace-window)
  :config
  (setq aw-scope 'frame))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("M-s-f" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :mode (("Appraisals\\'" . enh-ruby-mode)
;;          ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
;;          ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
;;   :interpreter "ruby"
;;   :init
;;   (progn
;;     (setq enh-ruby-deep-indent-paren nil
;;           enh-ruby-hanging-paren-deep-indent-level 2)))

(use-package enh-ruby-mode
  :ensure t
  :mode (("spec\\.rb\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :init
  (progn
    (setq enh-ruby-deep-indent-paren nil
          enh-ruby-hanging-paren-deep-indent-level 2)))

;; (use-package ruby-tools
;;   :ensure t)

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  :config
  (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (setq company-global-modes '(not inf-ruby-mode)))

(use-package ruby-mode
  :bind
  ("C-'" . ruby-toggle-string-quotes)
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  ;; (setq ruby-align-chained-calls nil)
  ;; (setq ruby-align-to-stmt-keywords nil)
  ;; (setq ruby-deep-indent-paren nil)
  ;; (setq ruby-deep-indent-paren-style nil)
  ;; (setq ruby-use-smie nil)
  ;; (setq ruby-align-chained-calls nil
  ;;     ruby-align-to-stmt-keywords nil
  ;;     ruby-deep-indent-paren nil
  ;;     ruby-deep-indent-paren-style nil
  ;;     ruby-use-smie nil)
  )

(use-package ruby-hash-syntax
  :ensure t)

(use-package bundler
  :ensure t)

(use-package seeing-is-believing
  :ensure t
  :bind (:map ruby-mode-map
              ("s-e e" . seeing-is-believing-run-as-xmpfilter)
              ("s-e a" . seeing-is-believing-run)
              ("s-e c" . seeing-is-believing-clear)
              ("s-e m" . seeing-is-believing-mark-current-line-for-xmpfilter)))

(use-package rubocop
  :ensure t
  :init (setq rubocop-keymap-prefix (kbd "s-R"))
  :config
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(use-package rspec-mode
  :ensure t
  :init (setq rspec-key-command-prefix (kbd "s-r"))
  :bind*
  (("s-r r" . rspec-rerun))
  :bind
  (("s-t" . rspec-toggle-spec-and-target)
   ("s-4 t" . rspec-find-spec-or-target-other-window))
  :config
  (setq compilation-scroll-output t)
  (setq rspec-primary-source-dirs '("app")))

(use-package minitest
  :ensure t
  :config
  (progn
    (setq minitest-use-bundler nil))
  (add-hook 'ruby-mode-hook 'minitest-mode))

(use-package rbenv
  :ensure t
  :init (setq rbenv-show-active-ruby-in-modeline nil)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)))

(use-package clojure-mode
  :ensure t
  :config
  ;; (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (setq cider-allow-jack-in-without-project t)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; (use-package js2-mode
;;   :ensure t
;;   :init
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;;   :config
;;   (setq js2-basic-offset 2))

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing nil)
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(use-package rjsx-mode
  :ensure t
  :defer t
  :init
  :config
  (setq js2-basic-offset 2)
  :mode "\\.js\\'")

(use-package haml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package company
  :ensure t
  :bind (("TAB" . company-indent-or-complete-common))
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-ido-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("s-D" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("s-," . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ;; ("s-r" . crux-recentf-ido-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ;;("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)
         ("C-c b" . crux-switch-to-previous-buffer)))

;; (use-package transpose-frame
;;   :ensure t
;;   :bind ("s-t" . transpose-frame))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))
  ;; :bind
  ;; (:map global-map
  ;;       ("s-t t"  . treemacs-toggle)
  ;;       ("M-0"    . treemacs-select-window)
  ;;       ("C-c 1"  . treemacs-delete-other-windows)
  ;;       ("s-t c"  . treemacs)
  ;;       ("s-t f"  . treemacs-find-file)))
(use-package treemacs-projectile
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ([f8]  . treemacs-projectile-toggle)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("s-d" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))
  ;; :bind (:map region-bindings-mode-map
  ;;        ("a"   . mc/mark-all-like-this)
  ;;        ("p"   . mc/mark-previous-like-this)
  ;;        ("M-p" . mc/skip-to-previous-like-this)
  ;;        ("n"   . mc/mark-next-like-this)
  ;;        ("M-n" . mc/skip-to-next-like-this)
  ;;        ("P"   . mc/unmark-previous-like-this)
  ;;        ("N"   . mc/unmark-next-like-this)
  ;;        ("["   . mc/cycle-backward)
  ;;        ("]"   . mc/cycle-forward)
  ;;        ("m"   . mc/mark-more-like-this-extended)
  ;;        ("h"   . mc-hide-unmatched-lines-mode)
  ;;        ("\\"  . mc/vertical-align-with-space)
  ;;        ("#"   . mc/insert-numbers) ; use num prefix to set the starting number
  ;;        ("^"   . mc/edit-beginnings-of-lines)
  ;;        ("$"   . mc/edit-ends-of-lines)))

(use-package cliphist
  :ensure t
  :config
  (setq cliphist-use-ivy t)
  :bind* ("M-y" . cliphist-paste-item))

(use-package multi-term
  :ensure t
  :bind ("C-c t" . multi-term))

(use-package dumb-jump
  :ensure t
  :init (global-unset-key (kbd "s-g"))
  :bind (("s-g o" . dumb-jump-go-other-window)
         ("s-g j" . dumb-jump-go)
         ("s-g i" . dumb-jump-go-prompt)
         ("s-g x" . dumb-jump-go-prefer-external)
         ("s-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package free-keys
  :ensure t
  :init (setq free-keys-modifiers '("" "C" "M" "C-M" "s")))

;;Sonic Pi
;; (use-package sonic-pi
;;   :ensure t
;;   :config
;;   (setq sonic-pi-path "/Users/sadhu/Developer/sonic-pi/")
;;   (setq sonic-pi-server-bin             "app/server/ruby/bin/sonic-pi-server.rb")
;;   (setq sonic-pi-compile-extensions-bin "app/server/ruby/bin/compile-extensions.rb"))

;; (use-package dash
;;   :ensure t)

;; (use-package osc
;;   :ensure t)

;; (use-package highlight
;;   :ensure t)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; https://github.com/lunaryorn/swsnr.de/blob/master/_posts/2015-04-29-the-power-of-display-buffer-alist.md
;; Configure `display-buffer' behaviour for some special buffers.
(setq
 display-buffer-alist
 `(
   ;; Put REPLs and error lists into the bottom side window
   (,(rx bos
         (or "*Help"                         ; Help buffers
             "*Warnings*"                    ; Emacs warnings
             "*Compile-Log*"                 ; Emacs byte compiler log
             "*compilation"                  ; Compilation buffers
             "*Rubocop"
             "*rspec-compilation"            ; Rspec compilation buffers
             "*Flycheck errors*"             ; Flycheck error list
             "*shell"                        ; Shell window
             "*sbt"                          ; SBT REPL and compilation buffer
             "*ensime-update*"               ; Server update from Ensime
             "*SQL"                          ; SQL REPL
             "*Cargo"                        ; Cargo process buffers
             (and (1+ nonl) " output*")      ; AUCTeX command output
             ))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . bottom)
    (reusable-frames . visible)
    (window-height   . 0.33))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package lunaryorn-window           ; Personal window utilities
  :load-path "lisp/"
  :defer t
  :bind (("C-c q" . lunaryorn-quit-all-side-windows)))
         ;; ("C-c w d" . lunaryorn-toggle-current-window-dedication)
         ;; ("C-c w b" . lunaryorn-switch-to-minibuffer-window)))

;; (defun ruby-mode-set-frozen-string-literal-true ()
;;   (when (eq major-mode 'ruby-mode)
;;     (save-excursion
;;       (widen)
;;       (goto-char (point-min))
;;       (unless (looking-at "^# frozen_string_literal: true")
;;         (insert "# frozen_string_literal: true\n\n")))))

;; (add-hook 'ruby-mode-hook (lambda()
;;                             (add-hook 'before-save-hook 'ruby-mode-set-frozen-string-literal-true)))

(use-package autoinsert
  :config
  (setq auto-insert-query nil)
  (setq auto-insert-alist
        (cons '("\\.rb\\'" nil "# frozen_string_literal: true\n") auto-insert-alist))
  (add-hook 'ruby-mode-hook 'auto-insert)
  (add-hook 'enh-ruby-mode-hook 'auto-insert))
