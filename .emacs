;;
;; vincekd's .emacs
;;

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;(defvar em-home "~/AppData/Roaming/.emacs.d")
(defvar em-dir "~/.emacs.d/")
(defvar is-windows (string-equal system-type "windows-nt"))
(defvar my-font "Consolas")


(setq user-emacs-directory em-dir)
(setq default-directory "~/")

(add-to-list 'load-path (concat em-dir "lisp/"))

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(set-face-attribute 'default nil :family my-font)

;;
;; get melpa packages in package.el
;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))
;; (add-to-list 'package-archives
;;   '("melpa" . "http: //melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)

(defvar my-packages
  ;;unused packages:
  ;;   grizzl gitconfig-mode gitignore-mode
  ;;   imenu+ sass-mode helm highlight-indentation
  ;;   indent-guide volatile-highlights persp-mode
  ;;   company company-shell company-web company-flx company-dict
  '(js2-mode less-css-mode workgroups2 diminish ace-jump-mode
             yaml-mode whitespace-cleanup-mode popup ag
             groovy-mode smartparens syntax-subword rainbow-mode
             python-mode scss-mode nlinum projectile auto-complete
             flx-ido idomenu ido-vertical-mode json-mode yaml-mode
             gradle-mode ace-window auto-package-update flycheck
             web-mode magit expand-region shackle golden-ratio
             golden-ratio-scroll-screen highlight-indent-guides
             ;;themes
             zenburn-theme color-theme-sanityinc-tomorrow gruvbox-theme)
  "A list of packages to ensure are installed at launch.")

;; conditional package adds
(if is-windows
    (nconc my-packages '(cygwin-mount))
  ;;no linux-specific packages
  )

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;
;; various settings
;;
;; cygwin bash
(when is-windows
  (load "setup-cygwin")
  (require 'setup-cygwin)
  (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe"))


(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)

;; auto update packages every 14 days
(require 'auto-package-update)
(auto-package-update-maybe)
(setq auto-package-update-interval 14)

(setq custom-file (concat em-dir "custom.el"))
;; (load custom-file :noerror)

;; put backup files in single directory
(defvar backup-file-dir (concat em-dir "backup/"))

(setq
 backup-directory-alist `(("." . ,backup-file-dir))
 make-backup-files t    ; make backups
 backup-by-copying t    ; Don't delink hardlinks
 version-control t      ; Use version numbers on backups
 delete-old-versions t  ; Automatically delete excess backups
 kept-new-versions 10    ; how many of the newest versions to keep
 kept-old-versions 0    ; and how many of the old
 delete-by-moving-to-trash t ; obvs
 ;;make-backup-file-name-function 'my-backup-file-name ;custom backup func
 )

;;disable right clicks and such
(dolist (k '([mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;; color theme
;;(load-theme 'zenburn t)
;;(setq custom-safe-themes t)
(load-theme 'gruvbox t)

;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

;; disable tabs, use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq css-indent-offset 2)
(setq web-indent-offset 2)
(set-default 'tab-always-indent 'complete)

;; scroll comp output
(setq compilation-scroll-output 1)
(setq compilation-window-height 10)

;;no wrap lines
(set-default 'truncate-lines t)

;; replace highlighted text when possible
(delete-selection-mode 1)

;; font-lock mode enables syntax highlighting
(global-font-lock-mode 1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; scroll line by line
(setq scroll-step 1)

;; disable gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Parenthesis matching
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode +1)

(setq require-final-newline t)


(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;
;; Functions
;;
;; set Ctrl-a to jump between beginning of line and beginning of code
(defun my--smart-beginning-of-line ()
  "Move point to `beginning-of-line'. If repeat command it cycle
position between `back-to-indentation' and `beginning-of-line'."
  (interactive "^")
  (if (eq last-command 'my--smart-beginning-of-line)
      (if (= (line-beginning-position) (point))
          (back-to-indentation)
          (beginning-of-line))
    (back-to-indentation)))

(defun my--smart-end-of-line ()
  "Move point to `end-of-line'. If repeat command it cycle
position between last non-whitespace and `end-of-line'."
  (interactive "^")
  (if (and (eq last-command 'my--smart-end-of-line)
           (= (line-end-position) (point)))
      (skip-syntax-backward " " (line-beginning-position))
    (end-of-line)))

(defun no-action ()
  "Dummy function that does nothing"
  (interactive "^"))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))


;; defines key overrides
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'my--smart-beginning-of-line)
    (define-key map (kbd "C-e") 'my--smart-end-of-line)
    (define-key map (kbd "C-t") 'no-action) ;get rid of char swapping
    (define-key map (kbd "M-j") (lambda () (interactive) (join-line -1)))
    (define-key map (kbd "M-/") 'hippie-expand)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    (define-key map (kbd "C-s") 'isearch-forward-regexp)
    (define-key map (kbd "C-r") 'isearch-backward-regexp)
    (define-key map (kbd "C-M-s") 'isearch-forward)
    (define-key map (kbd "C-M-r") 'isearch-backward)
    (define-key map (kbd "C-c C-v") 'hs-toggle-hiding)
    (define-key map (kbd "C-S-O") 'recentf-ido-find-file)
    (define-key map (kbd "C-S-R") 'projectile-find-file)
    (define-key map (kbd "C-S-f") 'projectile-ag)
    (define-key map (kbd "C-c k") 'kill-other-buffers)
    ;;(define-key map (kbd "C-\t") 'company-complete-common)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter nil)

(my-keys-minor-mode 1)

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;;
;; MODE CONFIGS
;;

(require 'shackle)
;;(setq shackle-default-rule '(:select t))
(setq shackle-rules
      '((compilation-mode :noselect t)
        ("*Ido Completions*" :noselect t :other t)
        ("*eshell*" :select t :same t)
        ("*Shell Command Output*" :noselect t)
        ("*shell*" :select t :same t)
        ("*Messages*" :noselect t :inhibit-window-quit t :other t)
        ("*Metahelp*" :select t :same t)
        ("*Help*" :select t :same t)
        ("*Completions*" :noselect t :other t)
        ("*Warnings*" :noselect t :other t)
        ("*Compile-Log*" :noselect t :other t)
        (magit-status-mode :select t :inhibit-window-quit t :same t)
        (magit-log-mode :select t :inhibit-window-quit t :same t)
        ;;projectile search
        ("^\\*ag search text\\:.*" :regexp t :select t :inhibit-window-quit t :same t)))
(shackle-mode 1)


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(require 'golden-ratio)
;; add ace-window jump to trigger commands
(nconc golden-ratio-extra-commands '(ace-window))
;;(setq golden-ratio-auto-scale t)
(golden-ratio-mode 1)

(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)


;;
;; whitespace mode conflicts with highlight-indent-guides-mode
;; (require 'whitespace)
;; ;; disable highlighting lines over certain limit
;; (setq whitespace-line-column -1)
;; ;; (setq whitespace-line-column 100)
;; ;;space-mark newline-mark
;; (setq whitespace-style (quote (face lines-tail trailing tabs newline tab-mark)))
;; ;; need to set this for special font cache I guess (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25148)
;; (setq inhibit-compacting-font-caches t)
;; (setq whitespace-display-mappings
;;       ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
;;       '(
;;         (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;         (newline-mark 10 [182 10]) ; LINE FEED,
;;         (tab-mark 9 [9655 9] [92 9]) ; tab
;;         ))
;; (global-whitespace-mode t)


(global-whitespace-cleanup-mode 1)


(defun my-enable-rainbow-mode ()
  "turn on rainbox mode"
  (rainbow-mode 1)
  ;; hide rainbow mode from mode line
  (diminish 'rainbow-mode))

;; web editing
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(setq scss-compile-at-save nil)
(add-hook 'css-mode-hook 'my-enable-rainbow-mode)
(add-hook 'scss-mode-hook 'my-enable-rainbow-mode)

(setq
 ;; js version (2.0)
 js2-language-version 200
 ;; ignore global libraries
 js2-global-externs '("jQuery" "$" "_" "angular")
 ;; include common keywords
 js2-include-browser-externs t
 js2-include-node-externs t
 ;; exclude rhino (mozilla js compiler) keywords
 js2-include-rhino-externs nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'my-enable-rainbow-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(rename-modeline "js2-mode" js2-mode "JS2")

(autoload 'json-mode "json-mode")
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


;; groovy mode
(autoload 'groovy-mode "groovy-mode")
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '(".gradle" . groovy-mode))


(require 'syntax-subword)
;;(setq syntax-subword-skip-spaces t)
(global-syntax-subword-mode 1)


(global-auto-complete-mode 1)


;; projectile stuff
(require 'projectile)
(setq projectile-mode-line
      '(:eval (format " Proj[%s]" (projectile-project-name))))
;;(setq projectile-mode-line nil)
;; set find-file to C-O (done in my-keys-minor-mode)
;;(setq projectile-completion-system 'grizzl)
(setq projectile-indexing-method 'alien)
(projectile-mode 1)


;; ido search, flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "C-x C-g") 'idomenu)


;; ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "M-p") 'ace-window)


;; smart parens
(require 'smartparens-config)
(smartparens-global-mode 1)


;;git commands (doesn't work very well)
;;(require 'magit)
;;(global-set-key (kbd "C-c g s") 'magit-status)


;; not working in emacs 25.2
;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)


;; persp mode
;;(define-key persp-key-map (kbd "C-c C-p") ...)
;; (setq persp-keymap-prefix (kbd "C-c C-p"))
;; (with-eval-after-load "persp-mode-autoloads"
;;       (setq wg-morph-on nil) ;; switch off animation
;;       (setq persp-autokill-buffer-on-remove 'kill-weak)
;;       (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))


(require 'workgroups2)
(setq wg-session-file (concat em-dir ".emacs-workgroups"))
;;(setq wg-session-load-on-start t)
;;(setq wg-mode-line-display-on nil)
(setq wg-emacs-exit-save-behavior 'ask) ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'ask) ; Options: 'save 'ask nil
;;(setq wg-morph-on nil)
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"
      wg-mode-line-decor-divider ":")
(define-key global-map (kbd "C-c z") 'workgroups-mode)


;; diminish stuff
(require 'diminish)
(diminish 'smartparens-mode nil)
(diminish 'auto-complete-mode nil)
;;(diminish 'whitespace-mode nil)
(diminish 'global-whitespace-mode nil)
(diminish 'whitespace-cleanup-mode nil)
;;(diminish 'rainbow-mode nil)
;;(diminish 'volatile-highlights-mode nil)
;;(diminish 'highlight-indentation-mode)
;;(diminish 'highlight-indentation-current-column-mode)
(diminish 'golden-ratio-mode nil)
(add-hook 'workgroups-mode-hook 'my-diminish-hook)
(add-hook 'prog-mode-hook 'my-diminish-hook)
(defun my-diminish-hook ()
  "diminish workgroups when toggled"
  (diminish 'workgroups-mode nil)
  (diminish 'hs-minor-mode nil))


;;(require 'web-mode)
(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-auto-close-style 2)


;; make unique names of tabs and shit
(require 'uniquify)


(save-place-mode 1)
(setq save-place-file (concat em-dir "saved-places"))


;; columns
(column-number-mode 1)
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;(require 'indent-guide)
;;(setq indent-guide-recursive t)
;;(indent-guide-global-mode 1)


;; show line numbers
(require 'nlinum)
;;(global-linum-mode 1)
;;(global-nlinum-mode 1)
(add-hook 'prog-mode-hook 'nlinum-mode)


(add-hook 'prog-mode-hook 'hs-minor-mode)


;; add recently opened files to the menu
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 20)
(recentf-mode +1)


;; colorize compilation output
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; (require 'company)
;; ;;(add-hook 'after-init-hook 'global-company-mode)
;; (setq company-idle-delay 0)
;; (global-company-mode 1)
;; (require 'flycheck)
;; (add-hook 'prog-mode-hook 'flycheck-mode)
