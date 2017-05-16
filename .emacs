;; my emacs file

(setq inhibit-startup-screen t)
;;(defvar em-home "~/AppData/Roaming/.emacs.d")
(defvar em-dir "~/.emacs.d/")
(setq user-emacs-directory em-dir)
(setq default-directory "~/")

;; get melpa packages in package.el
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)

(defvar my-packages
  ;;unused packages:
  ;;   expand-region grizzl magit gitconfig-mode gitignore-mode
  ;;   imenu+ volatile-highlights sass-mode
  '(js2-mode less-css-mode markdown-mode ansi-color workgroups2 diminish
             ace-jump-mode yaml-mode whitespace whitespace-cleanup-mode
             sws-mode jsx-mode groovy-mode smartparens syntax-subword
             rainbow-mode python-mode scss-mode nlinum auto-complete
             projectile flx-ido idomenu ido-vertical-mode json-mode
             yaml-mode gradle-mode ace-window volatile-highlights
             auto-package-update web-mode ag magit
             ;;themes
             zenburn-theme color-theme-sanityinc-tomorrow gruvbox-theme)
  "A list of packages to ensure are installed at launch.")


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
(load custom-file :noerror)

;; put backup files in single directory
(defvar backup-file-dir (concat em-dir "backup/"))
;; (defun my-backup-file-name (fpath)
;;   "Return a new file path of a given file path.
;;    If the new path's directories does not exist, create them."
;;   (message 'fpath)
;;   (let* (
;;          ;;(backup-file-dir (concat em-dir "backup/"))
;;          (filePath (replace-regexp-in-string "^[a-zA-Z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
;;          (backupFilePath (replace-regexp-in-string "//" "/" (concat backup-file-dir filePath "~") )))
;;     (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
;;     backupFilePath))
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


;; color theme
;;(load-theme 'zenburn t)
;;(setq custom-safe-themes t)
(load-theme 'gruvbox t)

;; disable tabs, use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq css-indent-offset 2)
(setq web-indent-offset 2)
;;(defvaralias 'cperl-indent-level 'tab-width)
;;(setq indent-tabs-mode nil)
;;(setq tab-width 4)
;; (custom-set-variables
;;  '(tab-width 4))

;;
(set-default 'tab-always-indent 'complete)

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

;; show line numbers
(global-nlinum-mode 1)

;;(semantic-mode 1)

;;cygwin bash
(setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
(setq shell-file-name "bash")
;;(setq shell-command-switch "-ic")
(setenv "SHELL" shell-file-name)

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

;; defines key overrides
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'my--smart-beginning-of-line)
    (define-key map (kbd "C-e") 'my--smart-end-of-line)
    (define-key map (kbd "C-S-O") 'projectile-find-file)
    (define-key map (kbd "C-t") 'no-action) ;get rid of char swapping
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter nil)

(my-keys-minor-mode 1)


;;
;; MODE CONFIGS
;;

(require 'whitespace)
(setq whitespace-line-column 100)
;;space-mark newline-mark
(setq whitespace-style (quote (face lines-tail trailing tabs newline tab-mark)))
(setq whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; LINE FEED,
        (tab-mark 9 [9655 9] [92 9]) ; tab
        ))

(global-whitespace-mode t)


;; css/scss/sass editing
;;(autoload 'sass-mode "sass-mode")
;;(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(setq scss-compile-at-save nil)
(add-hook 'css-mode-hook 'my-enable-rainbow-mode)
;;(add-hook 'sass-mode-hook 'my-enable-rainbow-mode)
(add-hook 'scss-mode-hook 'my-enable-rainbow-mode)
(defun my-enable-rainbow-mode ()
  "turn on rainbox mode"
  (rainbow-mode 1)
  ;; hide rainbow mode from mode line
  (diminish 'rainbow-mode))


;; (autoload 'json-mode "json-mode")
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


(require 'syntax-subword)
;;(setq syntax-subword-skip-spaces t)
(global-syntax-subword-mode 1)


(global-auto-complete-mode 1)


;; projectile stuff
(projectile-mode 1)
;; (setq projectile-mode-line
;;       '(:eval (format " P[%s]" (projectile-project-name))))
(setq projectile-mode-line nil)
;;set find-file to C-O (done in my-keys-minor-mode)
;;(setq projectile-completion-system 'grizzl)
;;(setq projectile-indexing-method 'alien)


(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; (require 'imenu+)
;; (setq imenup-sort-ignores-case-flag t)
;; (setq imenup-ignore-comments-flag t)


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


;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)


(global-whitespace-cleanup-mode 1)


;; smart parens
(require 'smartparens-config)
(smartparens-global-mode 1)


;;git commands (doesn't work very well)
;;(require 'magit)
;;(global-set-key (kbd "C-c g s") 'magit-status)


;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)


(require 'workgroups2)
(setq wg-session-file (concat em-dir ".emacs-workgroups"))
;;(setq wg-session-load-on-start t)
;;(setq wg-mode-line-display-on nil)
(setq wg-emacs-exit-save-behavior 'ask) ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'ask) ; Options: 'save 'ask nil
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"
      wg-mode-line-decor-divider ":")
(define-key global-map (kbd "C-c z") 'workgroups-mode)
;;(workgroups-mode 1)


(require 'diminish)
(diminish 'smartparens-mode nil)
(diminish 'auto-complete-mode nil)
;;(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode nil)
(diminish 'whitespace-cleanup-mode nil)
(diminish 'rainbow-mode)
(add-hook 'workgroups-mode-hook 'my-diminish-hook)
(defun my-diminish-hook ()
  "diminish workgroups when toggled"
    (diminish 'workgroups-mode nil))


;;(require 'web-mode)
(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-auto-close-style 2)
