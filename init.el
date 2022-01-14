(require 'package)
(setq byte-compile-warnings '(cl-functions))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

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
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst sadhu-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;;(desktop-save-mode 1)
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
(column-number-mode -1)
(size-indication-mode t)

(add-hook 'prog-mode-hook 'linum-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; maximize the initial frame automatically
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

;; Wrap lines at 80 characters
(setq-default fill-column 80)

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

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(defun delete-window-or-frame (&optional window frame force)
  (interactive)
  (if (= 1 (length (window-list frame)))
      (delete-frame-or-kill-emacs)
    (delete-window window)))

(defun delete-frame-or-kill-emacs()
  "Delete the selected frame.  If the last one, kill Emacs."
  (interactive)
  (condition-case nil (delete-frame) (error (kill-emacs))))

(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun duplicate-current-line-or-region(arg)
  (interactive "p")
  (crux-duplicate-current-line-or-region arg)
  (previous-line))

   (defun goto-match-paren (arg)
     "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1))
           (t
            (backward-char 1)
            (cond ((looking-at "\\s\)")
                   (forward-char 1) (backward-list 1))
                  (t
                   (while (not (looking-at "\\s("))
                     (backward-char 1)
                     (cond ((looking-at "\\s\)")
                            (message "->> )")
                            (forward-char 1)
                            (backward-list 1)
                            (backward-char 1)))
                     ))))))

(setq-default cursor-type '(bar . 1))
(add-hook 'text-mode-hook #'blink-cursor-mode)
(setq blink-cursor-interval .5)

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

;; OSX keybindings

(bind-keys*
 ;; General
 ("M-P" . counsel-M-x)
 ("M-N" . make-frame) ;; New window/instance
 ("M-," . crux-find-user-init-file) ;; User Settings

 ;; Basic Editing
 ("M-x" . xah-cut-line-or-region) ;; Cut line(empty selection)
 ("M-c" . easy-kill) ;; Copy line(empty selection)
 ("M-v" . yank)
 ("s-<down>" . move-text-down) ;; Move line down
 ("s-<up>" . move-text-up) ;; Move line up
 ("s-S-<down>" . crux-duplicate-current-line-or-region) ;; Copy line down
 ("s-S-<up>" . duplicate-current-line-or-region) ;; Copy line up
 ("M-a" . mark-whole-buffer)
 ("M-/" . comment-line) ;; Toggle line comment

 ("M-z" . undo-tree-undo)
 ("M-Z" . undo-tree-redo)

 ("s-S" . write-file)
 ("M-<up>" . beginning-of-buffer)
 ("M-<down>" . end-of-buffer)
 ("M-b" . ivy-switch-buffer)
 ("M-`" . other-frame)
 ("M-1" . delete-other-windows)
 ("s-M-<return>" . toggle-frame-maximized)
 ("M-k M-w" . delete-other-windows)
 ("M-2" . (lambda () (interactive)(split-window-vertically) (other-window 1)))
 ("M-3" . (lambda () (interactive)(split-window-horizontally) (other-window 1)))
 ("M-\\" . (lambda () (interactive)(split-window-horizontally) (other-window 1)))
 ("M-k M-\\" . (lambda () (interactive)(split-window-vertically) (other-window 1)))

 ("M-=" . text-scale-increase)
 ("M--" . text-scale-decrease)
 ("M-0" . text-scale-adjust)

 ("M-f" . swiper)
 ("M-w" . delete-window)
 ("M-s" . save-buffer)
 ("M-o" . counsel-find-file)

 ("M-t" . projectile-toggle-between-implementation-and-test)
 ("M-4 t" . projectile-find-implementation-or-test-other-window)
 ("M-<return>" . crux-smart-open-line) ;; Insert line below
 ("M-S-<return>" . crux-smart-open-line-above) ;; Insert line above
 ("M-K" . delete-current-line)

 ;; Emacs
 ("s-f" . forward-word)
 ("s-b" . backward-word)
 ("s-DEL" . backward-kill-word)
 ("s-d" . kill-word)
 ("<escape>" . keyboard-escape-quit)

 ;;lsp
 ("<f12>" . xref-find-definitions)
 ("M-4 <f12>" . xref-find-definitions-other-window)
 ("M-<down-mouse-1>" . lsp-find-definition-mouse)
 ("M-<mouse-1>" . ignore)
 ("S-<f12>" . xref-find-references)
 ("M-." . lsp-execute-code-action)
)

(bind-keys
 ;; General
 ("M-p" . counsel-projectile-find-file) ;; Quick open, Go to File
 ("M-g b" . counsel-bookmark)
 )




(define-key global-map [remap delete-window] 'delete-window-or-frame) ;; Close window/instance

;; (global-unset-key (kbd "s-k"))
;; (global-set-key (kbd "s-k s-s") 'counsel-descbinds)


;; (global-set-key (kbd "s-K") 'delete-current-line) ;; Delete line

;; (global-unset-key (kbd "s-|"))
;; (global-set-key (kbd "s-|") 'goto-match-paren)
;; (global-set-key (kbd "s-]") 'stupid-indent-line) ;; Indent line
;; (global-set-key (kbd "s-[") 'stupid-outdent-line) ;; Outdent line
;; (global-set-key (kbd "s-<left>") 'crux-move-beginning-of-line) ;; Go to beginning of line
;; (global-set-key (kbd "s-<right>") 'move-end-of-line) ;; Go to end of line
;; (global-set-key (kbd "M-s-“") 'yafolding-hide-element) ;; Fold all subregions
;; (global-set-key (kbd "M-s-‘") 'yafolding-show-element) ;; Unfold subregions
;; (global-set-key (kbd "s-k s-0") 'yafolding-hide-all) ;; Fold all regions
;; (global-set-key (kbd "s-k s-j") 'yafolding-show-all) ;; Unfold all regions
;; (global-set-key (kbd "s-k s-j") 'yafolding-show-all) ;; Unfold all regions




;; (global-set-key (kbd "<f12>") 'xref-find-definitions)
;; (global-set-key (kbd "s-4 <f12>") 'xref-find-definitions-other-window)
;; ;; (global-set-key (kbd "s-<down-mouse-1>") 'lsp-find-definition-mouse)
;; ;; (global-set-key (kbd "s-<mouse-1>") 'ignore)
;; (global-set-key (kbd "S-<f12>") 'xref-find-references)
;; (global-set-key (kbd "s-.") 'lsp-execute-code-action)
;; (global-set-key (kbd "M-<f12>") 'eglot-find-typeDefinition)


;; ;; (global-set-key (kbd "s-v") 'vterm-yank)

;; ;; Others
;; (global-set-key (kbd "S-<SPC>") 'set-mark-command) ;; Insert cursor below


;; (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)



;; (global-set-key (kbd "s-W") 'delete-frame)


;; (global-set-key (kbd "C-M-f") 'toggle-frame-maximized)

;; ;; (global-set-key (kbd "s-t") 'counsel-projectile-find-file)




;; (global-unset-key (kbd "s-g"))
;; (global-set-key (kbd "s-g s-l") 'goto-line)
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; ;; (global-set-key (kbd "<escape>") 'keyboard-quit)

;; (define-key 'help-command (kbd "C-i") #'info-display-manual)

;; (global-set-key (kbd "s-T") 'dired-sidebar-toggle-sidebar)

;; (global-set-key (kbd "s-n") 'ivy-next-history-element)
;; (global-set-key (kbd "s-p") 'ivy-previous-history-element)


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

;; (setq initial-major-mode 'ruby-mode)
;;(setq initial-major-mode 'enh-ruby-mode)
(setq initial-major-mode 'text-mode)
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

;; (setq use-package-always-defer t
;;       use-package-always-ensure t)

;; From https://github.com/hlissner/doom-emacs/blob/develop/modules/completion/ivy/autoload/ivy.el

(defvar +ivy-edit-functions nil
  "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")

(defun +ivy/woccur ()
  "Invoke a wgrep buffer on the current ivy results, if supported."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let ((caller (ivy-state-caller ivy-last)))
    (if-let* ((occur-fn (plist-get +ivy-edit-functions caller)))
        (ivy-exit-with-action
         (lambda (_) (funcall occur-fn)))
      (if-let* ((occur-fn (plist-get ivy--occurs-list caller)))
          (let ((buffer (generate-new-buffer
                         (format "*ivy-occur%s \"%s\"*"
                                 (if caller (concat " " (prin1-to-string caller)) "")
                                 ivy-text))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (funcall occur-fn))
              (setf (ivy-state-text ivy-last) ivy-text)
              (setq ivy-occur-last ivy-last)
              (setq-local ivy--directory ivy--directory))
            (ivy-exit-with-action
             `(lambda (_)
                (pop-to-buffer ,buffer)
                (ivy-wgrep-change-to-wgrep-mode))))
        (user-error "%S doesn't support wgrep" caller)))))

;; (use-package whole-line-or-region
;;   :ensure t
;;   :config
;;   (whole-line-or-region-mode))

(use-package stupid-indent-mode
  :ensure t)

(use-package yafolding
  :ensure t)

(use-package counsel
  :ensure smex
  :config
  (ivy-mode 1)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (setq ivy-use-virtual-buffers t)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (setq ivy-use-selectable-prompt t)
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  ;(global-set-key (kbd "C-r") 'swiper)
  ;(global-set-key "\C-s" 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)
  (define-key ivy-minibuffer-map (kbd "C-c C-e") '+ivy/woccur)
  (define-key ivy-minibuffer-map (kbd "s-j") 'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "s-n") 'ivy-next-line-and-call)
  (define-key ivy-minibuffer-map (kbd "s-p") 'ivy-previous-line-and-call)
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
  (define-key ivy-occur-grep-mode-map (kbd "e") 'ivy-wgrep-change-to-wgrep-mode))

(use-package wgrep
  :ensure t)

(use-package general
  :ensure t)

(setq default-frame-alist '((font . "Source Code Pro-13")))
;(add-hook 'prog-mode-hook 'mac-auto-operator-composition-mode)
;(setq default-frame-alist '((font . "Fira Code-13")))

;; highlight the current line
(global-hl-line-mode +1)

(use-package avy
  :ensure t
  :bind (("C-j" . avy-goto-word-or-subword-1)
         ("C-J" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("M-m" . magit-status)
   ("C-M-<tab>" . magit-section-cycle))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package forge
  :ensure t
  :config
  (push '("git.realestate.com.au" "git.realestate.com.au/api/v3"
        "git.realestate.com.au" forge-github-repository)
        forge-alist))

(use-package git-timemachine
  :ensure t
  :bind (("M-g" . git-timemachine)))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line          nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;;     (set-face-attribute 'mode-line          nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-light t))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package github-browse-file
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Developer/rea"))
  :config
  (define-key projectile-mode-map (kbd "C-M-p") 'projectile-command-map)
  (projectile-global-mode +1)
  (setq projectile-switch-project-action 'projectile-dired))

;; (defun counsel-projectile-find-file-occur ()
;;     (cd (projectile-project-root))
;;     (counsel-cmd-to-dired
;;      (counsel--expand-ls
;;       (format
;;        "find . | grep -i -E '%s' | xargs ls"
;;        (counsel--elisp-to-pcre ivy--old-re)))))

(defun counsel-projectile-find-file-occur ()
    (cd (projectile-project-root))
    (counsel-cmd-to-dired
     (format
      "find . | grep -i -E '%s' | xargs -d '\n' ls"
      (counsel--elisp-to-pcre ivy--old-re))))

(use-package counsel-projectile
  :ensure t
  :bind* ("M-F" . counsel-projectile-rg)
  :init
  (counsel-projectile-mode))
  ;; :config
  ;; (ivy-set-occur 'counsel-projectile-find-file 'counsel-projectile-find-file-occur)
  ;; (ivy-set-occur 'counsel-projectile 'counsel-projectile-find-file-occur))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind
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
  :bind (("M-s-f" . anzu-query-replace)
         ("M-s-h" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :init (setq rubocop-keymap-prefix (kbd "M-R"))
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
  (([(super control up)] . move-text-up)
   ([(super control down)] . move-text-down)))

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

(use-package elixir-mode
  :ensure t)

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

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :mode (("spec\\.rb\\'" . enh-ruby-mode))
;;   :interpreter "ruby"
;;   :init
;;   (progn
;;     (setq enh-ruby-deep-indent-paren nil
;;           enh-ruby-hanging-paren-deep-indent-level 2)))


(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  :config
  (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (setq company-global-modes '(not inf-ruby-mode)))

(setq-default flycheck-disabled-checkers '(ruby-reek))

(use-package ruby-mode
  :bind
  ("C-'" . ruby-toggle-string-quotes)
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  (setq ruby-align-to-stmt-keywords t)
  ;; (setq ruby-align-chained-calls nil)
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
              ("M-e e" . seeing-is-believing-run-as-xmpfilter)
              ("M-e a" . seeing-is-believing-run)
              ("M-e c" . seeing-is-believing-clear)
              ("M-e m" . seeing-is-believing-mark-current-line-for-xmpfilter)))

(use-package rubocop
  :ensure t
  :init (setq rubocop-keymap-prefix (kbd "M-R"))
  :config
  ;; (defun rubocop-autocorrect-current-file-silent ()
  ;;   (save-window-excursion (rubocop-autocorrect-current-file)))
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode)
  ;; (add-hook 'enh-ruby-mode-hook (lambda()
  ;;                                 (add-hook 'before-save-hook 'rubocop-autocorrect-current-file-silent nil t)))
  )

(use-package rspec-mode
  :ensure t
  :init (setq rspec-key-command-prefix (kbd "M-R"))
  :bind*
  (("M-r" . rspec-rerun))
  :bind
  (
   ;; ("s-t" . rspec-toggle-spec-and-target)
   ;; ("s-4 t" . rspec-find-spec-or-target-other-window)
   ("M-r" . rspec-rerun))
  :config
  (setq compilation-scroll-output t)
  (setq rspec-use-docker-when-possible t)
  (setq rspec-docker-container "ci")
  (setq rspec-primary-source-dirs '("app" "lib")))
  ;; (setq rspec-primary-source-dirs '("app")))

;; (use-package minitest
;;   :ensure t
;;   :bind*
;;   (("s-R v" . minitest-verify)
;;    ("s-r" . minitest-rerun)
;;    ("s-R s" . minitest-verify-single)
;;    ("s-R a" . minitest-verify-all))
;;   :config
;;   (progn
;;     (setq minitest-use-bundler t)
;;     (setq minitest-default-command '("ruby" "-W0" "-Ilib:test:spec")))
;;   (add-hook 'ruby-mode-hook 'minitest-mode))

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

(setq js-indent-level 2)
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

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks))


(defun disable-abbrev-mode()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package sml-mode
  :ensure t
  :config
  (abbrev-mode nil))

;; (use-package typescript-mode
;;   :ensure t
;;   :mode "\\.tsx?$"
;;   :hook
;;   (typescript-mode . lsp)
;;   :custom
;;   (typescript-indent-level 2))

(use-package haml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :bind
  (("s-l" . markdown-live-preview-mode)
   )
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package yaml-mode
  :ensure t)

(use-package company
  :ensure t
  :bind (("TAB" . company-indent-or-complete-common))
  :config
  (global-company-mode)
  ;; (setq lsp-completion-provider :none)
  (setq company-backends '((:separate company-capf company-dabbrev-code)))
  ;; (add-to-list 'company-backends 'company-elm)
  ;; (add-to-list 'company-backends 'company-lsp)
  ;; See https://github.com/company-mode/company-mode/issues/60
  ;; https://emacs.stackexchange.com/questions/26082/company-mode-in-groovy-mode-how-to-get-completions-other-than-lowercase
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; (use-package super-save
;;   :ensure t
;;   :config
;;   (super-save-mode +1))

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

;; (use-package undo-fu
;;   :bind (("s-z" . undo-fu-only-undo)
;;          ("s-Z" . undo-fu-only-redo))
;;   :ensure t)

(use-package crux
  :ensure t
  ;; :bind*
  ;; (("C-t" . terminal))
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . rename-file-and-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c S" . crux-find-shell-init-file)
         ;; ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ;;("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-K" . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)
         ("C-c b" . crux-switch-to-previous-buffer)))


(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))


 ;; ;; Multi-cursor and Selection
;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click) ;; Insert cursor
;; (global-set-key (kbd "M-s-<up>") 'mc/mark-previous-like-this) ;; Insert cursor above
;; (global-set-key (kbd "M-s-<down>") 'mc/mark-next-like-this) ;; Insert cursor below
;; (global-set-key (kbd "M-I") 'mc/edit-ends-of-lines) ;; Insert cursor below

(use-package multiple-cursors
  :ensure t
  :init (global-unset-key (kbd "s-k"))
  :bind* (("C-S-c C-S-c" . mc/edit-lines)
          ("M-d" . mc/mark-next-like-this)
          ("M-k M-d" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-g" . mc/mark-all-like-this)
         ("M-C-<mouse-1>" . mc/add-cursor-on-click)))

(use-package dumb-jump
  :ensure t
  :init (global-unset-key (kbd "M-g"))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g g" . goto-line)
         ;; ("<f12>" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package ag
  :ensure t)

;; (use-package smart-jump
;;   :ensure t
;;   :init (global-unset-key (kbd "s-g"))
;;   :config
;;   (smart-jump-setup-default-registers)
;;   :bind (("s-g o" . smart-jump-go)))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package free-keys
  :ensure t
  :init (setq free-keys-modifiers '("" "C" "M" "C-M" "s")))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defun multiple-monitors()
  (interactive)
  (setq display-buffer-alist nil)
  (setq special-display-buffer-names '("*rspec-compilation*" "*guard*")))

(defun single-monitor()
  (interactive)
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
               "*rspec-compilation*"           ; Rspec compilation buffers
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
  )

(setq multiple-monitors nil)
(if multiple-monitors
    (multiple-monitors)
    (single-monitor))

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
  (setq auto-insert-alist
        (cons '("Gemfile" nil "source 'https://rubygems.org'\n\n") auto-insert-alist))
  (add-hook 'ruby-mode-hook 'auto-insert)
  (add-hook 'enh-ruby-mode-hook 'auto-insert))

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  :config
  (setq elm-format-on-save t))

(use-package haskell-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; (use-package yasnippet-snippets
;;   :ensure t)
(use-package direx
  :ensure t
  :bind (("C-x C-J" . 'direx:jump-to-directory)
         ("C-x C-j" . 'direx-project:jump-to-project-root))
  :init (setq direx:closed-icon " ▶ "
	      direx:open-icon " ▼ "))

;; (eval-after-load "term"
;;   '(define-key term-raw-map (kbd "s-v") 'term-paste))

;; (defun oleh-term-exec-hook ()
;;   (let* ((buff (current-buffer))
;;          (proc (get-buffer-process buff)))
;;     (set-process-sentinel
;;      proc
;;      `(lambda (process event)
;;         (if (string= event "finished\n")
;;             (kill-buffer ,buff))))))

;; (add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; (defun terminal ()
;;   "Switch to terminal. Launch if nonexistent."
;;   (interactive)
;;   (if (get-buffer "*ansi-term*")
;;       (switch-to-buffer "*ansi-term*")
;;     (ansi-term "/bin/bash"))
;;   (get-buffer-process "*ansi-term*"))

;; (defalias 'tt 'terminal)

;; (defun named-term (name)
;;   (interactive "sName: ")
;;   (ansi-term "/bin/bash" name))

;; (global-set-key (kbd "C-t") 'terminal)

;; http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
(setq insert-directory-program (executable-find "gls")
      ;; https://oremacs.com/2015/01/13/dired-options/
      dired-listing-switches "-laGh1v --group-directories-first")

(use-package default-text-scale
  :ensure t
  :bind (("M-=" . default-text-scale-increase)
         ("M--" . default-text-scale-decrease)))

(use-package git-messenger
  :ensure t
  :bind ("M-h" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t))

(use-package vterm
  :ensure t
  :init
  (add-to-list 'load-path "~/Developer/emacs-libvterm"))

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode t))

(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))


;; Metals

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)


(defun lsp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (scala-mode . lsp-install-save-hooks)
         (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-completion-provider :none)
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        (setq gc-cons-threshold 100000000) ;; 100mb
        (setq read-process-output-max (* 1024 1024)) ;; 1mb
        (setq lsp-idle-delay 0.500)
        (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))


;; (use-package dockerfile-mode
;;   :ensure t)

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t)

;; (use-package lsp-java
;;   :ensure t
;;   :config (add-hook 'java-mode-hook 'lsp))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-diagnostic-max-lines 20))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  :ensure t)

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :config
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; eglot
;; Enable scala-mode and sbt-mode
;; (use-package scala-mode
;;   :interpreter
;;     ("scala" . scala-mode))

;; (defun sbt:run-sbt (&optional kill-existing-p pop-p)
;;   "Start or restarts (if kill-existing-p is non-NIL) sbt in a
;; buffer called *sbt*projectdir."
;;   (let* ((project-root (or (sbt:find-root)
;; 			   (error "Could not find project root, type `C-h f sbt:find-root` for help.")))
;;          (buffer-name (sbt:buffer-name))
;;          (inhibit-read-only 1))
;;     ;; (when (null project-root)
;;     ;;   (error "Could not find project root, type `C-h f sbt:find-root` for help."))

;;     ;; (when (not (or (executable-find sbt:program-name)
;;     ;;                (file-executable-p (concat project-root sbt:program-name))))
;;     ;;   (error "Could not find %s in %s or on PATH. Please customize the sbt:program-name variable." sbt:program-name project-root))

;;     ;; kill existing sbt
;;     (when (and kill-existing-p (get-buffer buffer-name))
;;       (sbt:clear buffer-name)
;;       (kill-buffer buffer-name))

;;     ;; start new sbt
;;     (with-current-buffer (get-buffer-create buffer-name)
;;       (when pop-p (pop-to-buffer-same-window (current-buffer)))
;;       (unless (comint-check-proc (current-buffer))
;;         (unless (derived-mode-p 'sbt-mode) (sbt-mode))
;;         (cd project-root)
;;         (buffer-disable-undo)
;;         (message "Starting sbt in buffer %s " buffer-name)

;;         ;; insert a string to buffer so that process mark comes after
;;         ;; compilation-messages-start mark.
;;         (insert (concat "Running " sbt:program-name "\n"))
;;         (goto-char (point-min))
;;         (ignore-errors (compilation-forget-errors))
;;         (comint-exec (current-buffer) buffer-name sbt:program-name nil sbt:program-options))
;;       (current-buffer))))

;; (use-package sbt-mode
;;   :ensure t
;;   :bind ("s-r" . sbt-hydra)
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; (setq sbt:program-name "auto/dev-environment sbt")
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))

;; ;; Enable sbt mode for executing sbt commands
;; (use-package sbt-mode
;;   :ensure t
;;   :bind ("s-r" . sbt-hydra)
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false"))
;; )

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(scala-mode . ("metals-emacs")))
;;   (add-hook 'before-save-hook
;;             (lambda () (when (eglot-managed-p)
;;                          (eglot-format-buffer))))
;;   ;; (optional) Automatically start metals for Scala files.
;;   :hook (scala-mode . eglot-ensure))
;; (use-package eglot
;;   :ensure t
;;   :pin melpa-stable
;;   ;; (optional) Automatically start metals for Scala files.
;;   :hook (scala-mode . eglot-ensure))

(defun doom-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defun +vterm/here (name arg)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root."
  (interactive "P")
  (when (eq major-mode 'vterm-mode)
    (user-error "Already in a vterm buffer"))
  (require 'vterm)
  ;; This hack forces vterm to redraw, fixing strange artefacting in the tty.
  (save-window-excursion
    (pop-to-buffer "*scratch*"))
  (let ((default-directory
          (if arg
              default-directory
            (or (doom-project-root) default-directory))))
    (named-term name)))

(defun here-named-term(name)
  (interactive "sName: ")
  (+vterm/here name nil))

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*vterm*")
      (switch-to-buffer "*vterm*")
    (+vterm/here "vterm" nil))
  (get-buffer-process "*vterm*"))

(defun named-term (name)
  (interactive "sName: ")
  (let ((buffer (generate-new-buffer (concat "*" name "*"))))
    (with-current-buffer buffer
      (vterm-mode))
    (switch-to-buffer buffer)))


(defun toggle-terminal ()
  "Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*vterm*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*vterm*")
        (switch-to-buffer "*vterm*")
      (progn
        (terminal)))))

(global-set-key (kbd "C-t") 'terminal)
(global-set-key (kbd "C-`") 'toggle-terminal)
(global-set-key (kbd "C-S-t") 'here-named-term)


;; Remove after this is merged https://github.com/akermu/emacs-libvterm/pull/70
(defun vterm-send-return ()
  "Sends C-m to the libvterm."
  (interactive)
  (process-send-string vterm--process "\C-m"))

(define-key vterm-mode-map [return]                    #'vterm-send-return)
(put 'magit-diff-edit-hunk-commit 'disabled nil)


(use-package hydra
  :ensure t)

;; smerge source: https://ladicle.com/post/config/#smerge
(use-package smerge-mode
  :diminish
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

(use-package restclient
  :ensure t
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

;; (use-package company-restclient
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

(use-package slim-mode
  :ensure t)

(use-package carbon-now-sh
  :ensure t)

(use-package ripgrep
  :ensure t)

;; Set up a mode for JSON based templates

(define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))

(define-key git-commit-mode-map (kbd "C-c C-p") 'git-commit-co-authored)


;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-move-forward-on-expand        nil
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-user-mode-line-format         nil
;;           treemacs-user-header-line-format       nil
;;           treemacs-width                         35
;;           treemacs-workspace-switch-cleanup      nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)

(defun j-dired-find-file ()
    "Like `find-file' but with `default-directory' set to the
one specified by listing header."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))

(use-package dired-sidebar
  :ensure t
  :bind (:map dired-mode-map
              ("s-o" . j-dired-find-file))
  :commands (dired-sidebar-toggle-sidebar))

(use-package all-the-icons
  :ensure t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-dired
  :ensure t)


;; (setq split-width-threshold nil)
;; (setq split-height-threshold nil)


(use-package helpful
  :ensure t)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)

(use-package eshell-z
  :ensure t)


;; (defun my-forge-post-submit-callback-kill-url (value _headers _status _req)
;;   (when t
;;     (when-let ((url (alist-get 'html_url value)))
;;       (kill-new url))))

;; (add-hook 'forge-post-submit-callback-hook 'my-forge-post-submit-callback-kill-url)



(defvar log-process nil)
(make-variable-buffer-local 'log-process)

(defun metals-log()
  (interactive)
  (let ((root (projectile-project-root)))
    (if root
        (let ((log-file (concat root ".metals/metals.log"))
              (buffer (get-buffer-create "*metals-log*")))
          (setq log-process (start-process "metals-log" buffer "tail" "-n" "100" "-f" log-file))
          (switch-to-buffer buffer))
      (error "Project root not found."))))

;; https://stackoverflow.com/questions/12634850/how-to-rename-a-file-by-editing-its-current-name
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(setq magit-clone-default-directory "~/Developer/rea/")

;; (defun minitest-verify-all ()
;;   "Run all tests."
;;   (interactive)
;;   (minitest--run-command
;;     (mapconcat 'shell-quote-argument
;;                (-flatten
;;                 (--remove (eq nil it)
;;                  (list "RUBYOPT=-W0" (minitest-bundler-command) "rake" ))) " ")))


(unbind-key "C-M-p" smartparens-mode-map)
(unbind-key "M-p" markdown-mode-map)


(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
	 "[" (getenv "AWS_ROLE") "@" (system-name) " "
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~" (eshell/basename (eshell/pwd)))
	 "]"
	 (if (= (user-uid) 0) "# " "$ "))))


(use-package hippie-exp
  :bind (("S-<tab>"   . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package nand2tetris
  :ensure t
  :mode (("\\.hdl\\'" . nand2tetris-mode)))
