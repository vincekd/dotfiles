;;
;; vincekd's .emacs
;;

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defvar is-windows (string-equal system-type "windows-nt"))

;; (when is-windows
;;   ;; make this explicit
;;   (setenv "HOME" "C:/Users/Vincent/"))

;;(defvar em-home "~/AppData/Roaming/.emacs.d")
(defvar em-dir (expand-file-name "~/.emacs.d/"))
;;(defvar my-font "Consolas")


(defvar org-mode-dir (expand-file-name "~/notes/"))

(setq user-emacs-directory em-dir)
(setq default-directory (expand-file-name "~/"))

(add-to-list 'load-path (concat em-dir "lisp/"))

;;(setq exec-path (append exec-path '((expand-file-name "~/node_modules/eslint"))))

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defvar font-families
  '("Noto Sans Mono" "Noto Mono"))
;;(defvar family ())
(set-face-attribute
  'default nil
  ;;:family "Consolas"
  ;;:family "Noto Serif"
  :family "Noto Sans Mono"
  ;;:family "Noto Mono"
  ;;:width 'ultra-expanded
  ;;:height 105
  ;;:height 100
  :height 105
  )
;;(set-frame-font "Noto Mono" nil t)

;;
;; get melpa packages in package.el
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ;;("marmalade" . "http://marmalade-repo.org/packages/")
                          ))
(package-initialize)

(require 'cl)

(defvar my-packages
  ;;unused packages:
  ;;   grizzl gitconfig-mode gitignore-mode diminish
  ;;   imenu+ sass-mode helm highlight-indentation
  ;;   indent-guide volatile-highlights persp-mode
  ;;   company company-shell company-web company-flx company-dict
  ;;   highlight-indent-guides hl-todo ess indium hydra nlinum
  '(js2-mode less-css-mode workgroups2 ace-jump-mode flyspell ggtags
     yaml-mode whitespace-cleanup-mode popup ag ispell yaml-mode
     groovy-mode groovy-imports smartparens syntax-subword magit
     python-mode scss-mode  projectile auto-complete
     flx-ido idomenu ido-vertical-mode ido-completing-read+
     gradle-mode ace-window auto-package-update rainbow-mode
     flycheck web-mode expand-region shackle golden-ratio rust-mode
     golden-ratio-scroll-screen dired-quick-sort smart-mode-line
     package-lint ert shut-up aggressive-indent json-mode dash
     smex pcre2el comment-tags typescript-mode go-mode go-autocomplete
     ;;themes
     zenburn-theme color-theme-sanityinc-tomorrow gruvbox-theme
     tangotango-theme)
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


;; auto update packages every 14 days
(require 'auto-package-update)
(auto-package-update-maybe)
(setq auto-package-update-interval 14)


;;
;; general emacs
;;
(setq transient-mark-mode t ;; enable visual feedback on selections
  scroll-step 1 ;; scroll line by line
  ;;confirm-kill-emacs 'y-or-n-p ;; don't kill emacs without prompt
  require-final-newline t
  save-interprogram-paste-before-kill t
  ;; scroll comp output
  compilation-scroll-output 1
  compilation-window-height 10
  apropos-do-all t
  mouse-yank-at-point t
  require-final-newline t
  visible-bell t
  ring-bell-function 'ignore
  load-prefer-newer t
  ediff-window-setup-function 'ediff-setup-windows-plain

  ;; buffer encoding
  buffer-file-coding-system 'utf-8-unix
  default-file-name-coding-system 'utf-8-unix
  default-keyboard-coding-system 'utf-8-unix
  default-process-coding-system '(utf-8-unix . utf-8-unix)
  default-sendmail-coding-system 'utf-8-unix
  default-terminal-coding-system 'utf-8-unix

  ;; custom file
  custom-file (concat em-dir "custom.el"))
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
              [down-mouse-4] [drag-mouse-4]
              [down-mouse-5] [drag-mouse-5]))
  ;; [mouse-4] [double-mouse-4] [triple-mouse-4]
  ;; [mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)


;; set gui window size
(if (window-system)
  (set-frame-size (selected-frame) 124 40))

;; color theme
(setq custom-safe-themes t)
;;(load-theme 'zenburn t)
(load-theme 'gruvbox t)
;;(load-theme 'tangotango t)

;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

;;no wrap lines
(set-default 'truncate-lines t)

;; save place in files
(save-place-mode 1)
(setq save-place-file (concat em-dir "saved-places"))

;; replace highlighted text when possible
(delete-selection-mode 1)

;; font-lock mode enables syntax highlighting
(global-font-lock-mode 1)

;; disable gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Parenthesis matching
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode +1)

(global-auto-revert-mode t)

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

(defun sudo-find-file ()
  "File file as root."
  (interactive)
  (let ((root-file "/sudo:root@localhost:"))
    (find-file (concat root-file
                 (read-file-name "Find file (as su): ")))))

(defun recentf-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (completing-read "Choose recent file: " recentf-list nil t)))
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
    (define-key map (kbd "C-S-O") 'recentf-find-file)
    (define-key map (kbd "C-S-R") 'projectile-find-file)
    (define-key map (kbd "C-S-f") 'projectile-ag)
    (define-key map (kbd "C-c k") 'kill-other-buffers)
    (define-key map (kbd "C-x C-r") 'sudo-find-file)
    ;;(define-key map (kbd "C-\t") 'company-complete-common)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter nil)

(my-keys-minor-mode 1)

;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; (defmacro rename-modeline (package-name mode new-name)
;;   `(eval-after-load ,package-name
;;      '(defadvice ,mode (after rename-modeline activate)
;;         (setq mode-name ,new-name))))


;;
;; Personal style
;;

;; disable tabs, use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq lisp-indent-offset 2)
(setq css-indent-offset 2)
(setq web-indent-offset 2)
(set-default 'tab-always-indent 'complete)


;;
;; MODE CONFIGS
;;
;;(setq debug-on-error t)
(autoload 'comment-tags-mode "comment-tags")
(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
    `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
       ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
       ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
       ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
       ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
       ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
       ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
       ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
    comment-tags-require-colon t
    comment-tags-case-sensitive t))
(add-hook 'prog-mode-hook 'comment-tags-mode)


(require 'smart-mode-line)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(add-to-list 'sml/replacer-regexp-list '("^~/dev/" ":DEV:") t)
(add-to-list 'sml/replacer-regexp-list '("^:DEV:workpro/main-application/" ":WP:") t)
;; TODO: fix this
(add-to-list 'sml/replacer-regexp-list '("^:WP:\([*\\]*\)/src/main/" ":WP/\1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:WP/\(.*\):groovy/org/egg/games/plugins/notebook/" ":WP/\1/GR:") t)
(add-to-list 'sml/replacer-regexp-list '("^:WP/\(.*\):resources/META-INF/resources/" ":WP:\1:GR:RS") t)

(setq rm-whitelist " *Proj\\[.*")
;;(add-to-list 'sml/replacer-regexp-list '("^:Git:\(.*\)/src/main/java/" ":G/\1/SMJ:") t)
(sml/setup)
;;(smart-mode-line-enable)


;; (autoload 'aggressive-indent-mode "aggressive-indent")
;; (with-eval-after-load "aggressive-indent"
;;   (setq aggressive-indent-excluded-modes (append
;;                                            aggressive-indent-excluded-modes
;;                                            '())))
;; (add-hook 'prog-mode-hook 'aggressive-indent-mode)


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


;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)


(require 'golden-ratio)
;; add ace-window jump to trigger commands
(nconc golden-ratio-extra-commands '(ace-window))
;;(setq golden-ratio-auto-scale t)
(golden-ratio-mode 1)

(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)


;; whitespace mode y'all
;; whitespace mode conflicts with highlight-indent-guides-mode
(autoload 'whitespace-mode "whitespace-mode")
(with-eval-after-load "whitespace"
  ;; disable highlighting lines over certain limit
  (setq whitespace-line-column -1)
  ;;space-mark newline-mark
  (setq whitespace-style (quote (face lines-tail trailing tabs tab-mark)))
  ;; need to set this for special font cache I guess (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25148)
  (setq inhibit-compacting-font-caches t)
  (setq whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '(
       (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10]) ; LINE FEED,
       (tab-mark 9 [9655 9] [92 9]) ; tab
       )))
;;(global-whitespace-mode t)
;;(add-hook 'prog-mode-hook 'whitespace-mode)


;;(global-whitespace-cleanup-mode 1)
(autoload 'whitespace-cleanup-mode "whitespace-cleanup-mode")
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)


(defun my-enable-rainbow-mode ()
  "turn on rainbox mode"
  (rainbow-mode 1))

;; web editing
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(setq scss-compile-at-save nil)
(add-hook 'css-mode-hook 'my-enable-rainbow-mode)
(add-hook 'scss-mode-hook 'my-enable-rainbow-mode)


;; syntax checker
(autoload 'flycheck-mode "flycheck-mode")
(with-eval-after-load "flycheck"
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)) ;;newline
  (setq flycheck-idle-change-delay 2)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-mode 'go-build 'go-mode)
  (flycheck-add-mode 'groovy 'groovy-mode)
  (flycheck-add-mode 'html-tidy 'web-mode))
;;(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'groovy-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)


(autoload 'js2-mode "js2-mode")
(with-eval-after-load "js2-mode"
  (setq
    ;; js version (2.0)
    js2-language-version 200
    ;; ignore global libraries
    js2-global-externs '("jQuery" "$" "_" "angular")
    ;; include common keywords
    js2-include-browser-externs t
    js2-include-node-externs t
    ;; exclude rhino (mozilla js compiler) keywords
    js2-include-rhino-externs nil
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil
    js-switch-indent-offset 4)
  (add-hook 'js2-mode-hook 'my-enable-rainbow-mode))
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; JS mode
(autoload 'js-mode "js")
(with-eval-after-load
  (setq
    ;;js-language-version 200
    js-indent-level 4
    ;;js-expr-indent-offset 8
    js-indent-first-init 'dynamic
    ;; only works in emacs 26+
    js-chain-indent t
    js-indent-align-list-continuation nil))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))


(autoload 'json-mode "json-mode")
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


;; groovy mode
;;(load-file (expand-file-name "~/dev/groovy-emacs-modes/groovy-mode.el"))
(autoload 'groovy-mode "groovy-mode")
(with-eval-after-load "groovy-mode"
  (setq groovy-highlight-assignments t))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-hook 'groovy-mode-hook 'groovy-imports-scan-file)


;; R mode
(autoload 'ess-site "ess")
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))


(autoload 'typescript-mode "typescript-mode")
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))


(require 'syntax-subword)
;;(setq syntax-subword-skip-spaces t)
(global-syntax-subword-mode 1)


(autoload 'auto-complete-mode "auto-complete")
(ac-config-default)
(global-auto-complete-mode nil)
(with-eval-after-load "auto-complete"
  (setq ac-trigger-key "TAB"
    ;;ac-use-overriding-local-map t
    ))
(add-hook 'prog-mode-hook 'auto-complete-mode)


;; ignored files for ag-search and projectile
(defvar ignored-dirs
  '("*.gradle/"
     "*gradle/"
     "*.settings/"
     "*build/"
     "*lib/"
     "*libs/"
     "*bower_components/"
     "*tinymce/"
     "*bootstrap/"
     "*.git/"
     "*media/"
     "*images/"
     "/main-application/uploads/"
     "*node_modules/"))

(require 'ag)
(with-eval-after-load "ag"
  (setq ag-resuse-window nil)
  (setq ag-reuse-buffers t)
  (setq-default ag-ignore-list (append ignored-dirs '("*.log" "*.csv" "*.min.*" "#*#"))))


;; projectile stuff
;;(load-file (expand-file-name "~/dev/projectile/projectile.el"))
(require 'projectile)
(with-eval-after-load "projectile"
  (setq projectile-mode-line
    '(:eval (format " Proj[%s]" (projectile-project-name))))
  ;;(setq projectile-mode-line nil)
  ;; set find-file to C-O (done in my-keys-minor-mode)
  (setq projectile-completion-system 'ido)
  ;;(setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-verbose nil)

  (setq projectile-globally-ignored-file-suffixes
    '(".png"
       ".pdf"
       ".class"
       ".gif"
       ".jpg"
       ".eot"
       ".ttf"
       ".woff"
       ".woff2"
       ".xlsx"
       ".doc"
       ".docx"
       ".jar"
       ".project"
       ".classpath"
       ".zip"
       ".tern-project"))
  (setq projectile-globally-ignored-files
    (append '("GRTAGS"
               "GPATH"
               "GTAGS"
               "TAGS"
               "*.min.js")
      projectile-globally-ignored-files))
  (setq projectile-globally-ignored-directories
    (append ignored-dirs
      projectile-globally-ignored-directories)))
(projectile-mode 1)


;; ido search, flx
(require 'flx-ido)
(require 'ido-completing-read+)
(setq
  ido-enable-flex-matching t
  ido-use-faces nil
  ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)


(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; (autoload 'idomenu "idomenu" nil t)
;; (global-set-key (kbd "C-x C-g") 'idomenu)


;; ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "M-p") 'ace-window)
(autoload 'ace-jump-mode "ace-jump-mode")
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode")
(autoload 'ace-window "ace-window")
(with-eval-after-load "ace-jump-mode"
  (ace-jump-mode-enable-mark-sync))


;; - layout stuff
(autoload 'smartparens-mode "smartparens")
(with-eval-after-load "smartparens"
  (require 'smartparens-config))
(add-hook 'prog-mode-hook 'smartparens-mode)


;; git stuff
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq
  ;; use ido to look for branches
  magit-completing-read-function 'magit-ido-completing-read
  ;; don't put "origin-" in front of new branch names by default
  magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
  ;; open magit status in same window as current buffer
  magit-status-buffer-switch-function 'switch-to-buffer
  ;; highlight word/letter changes in hunk diffs
  magit-diff-refine-hunk t
  ;; ask me to save buffers
  magit-save-some-buffers t
  ;; pop the process buffer if we're taking a while to complete
  magit-process-popup-time 10
  ;; ask me if I want a tracking upstream
  magit-set-upstream-on-push 'askifnotset)
;; (defhydra hydra-magit (:color blue :columns 8)
;;   "Magit"
;;   ("c" magit-status "status")
;;   ("C" magit-checkout "checkout")
;;   ("v" magit-branch-manager "branch manager")
;;   ("m" magit-merge "merge")
;;   ("l" magit-log "log")
;;   ("!" magit-git-command "command")
;;   ("$" magit-process "process"))


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


(autoload 'workgroups-mode "workgroups2")
(with-eval-after-load "workgroups2"
  (setq wg-session-file (concat em-dir ".emacs-workgroups"))
  ;;(setq wg-session-load-on-start t)
  ;;(setq wg-mode-line-display-on nil)
  (setq wg-emacs-exit-save-behavior 'ask) ; Options: 'save 'ask nil
  (setq wg-workgroups-mode-exit-save-behavior 'ask) ; Options: 'save 'ask nil
  ;;(setq wg-morph-on nil)
  (setq wg-mode-line-decor-left-brace "["
    wg-mode-line-decor-right-brace "]"
    wg-mode-line-decor-divider ":"))
(define-key global-map (kbd "C-c z") 'workgroups-mode)


(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(with-eval-after-load "web-mode"
  (setq web-mode-markup-indent-offset 2
    web-mode-enable-auto-expanding t
    web-mode-enable-current-element-highlight t
    web-mode-auto-close-style 2))


;; make unique names of tabs and shit
(require 'uniquify)


;;org modes
(autoload 'org-mode "org")
(with-eval-after-load "org"
  ;; (define-key global-map "\C-cl" 'org-store-link)
  ;; (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files (list (concat org-mode-dir "work.org")
                           (concat org-mode-dir "life.org"))))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;; flyspell
(autoload 'flyspell-mode "flyspell")
(with-eval-after-load "flyspell"
  (setq flyspell-issue-message-flag nil)
  (setq-default ispell-program-name "aspell"))
(add-hook 'org-mode-hook 'flyspell-mode)
;;(add-hook 'text-mode-hook 'flyspell-mode)
;;(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
;;(autoload 'tex-mode-flyspell-verify "flyspell" "" t)


;; columns
(column-number-mode 1)
;; (require 'highlight-indent-guides)
;; (setq highlight-indent-guides-method 'character)
;; (setq highlight-indent-guides-character ?\|)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;(require 'indent-guide)
;;(setq indent-guide-recursive t)
;;(indent-guide-global-mode 1)


;; show line numbers
;;(require 'nlinum)
;;(autoload 'nlinum-mode "nlinum")
;;(add-hook 'prog-mode-hook 'nlinum-mode)


;; hide/show (keybind in my-keys)
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; ;; add recently opened files to the menu
(require 'recentf)
(setq recentf-max-saved-items 200
  recentf-max-menu-items 20)
(recentf-mode 1)


;; ;; colorize compilation output
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; DIRED setup
;;(autoload 'dired "dired")
(with-eval-after-load "dired"
  (require 'dired-x)
  (require 'dired-quick-sort)
  (setq ls-lisp-use-insert-directory-program t
    ;;dired-omit-files (concat dired-omit-files "\\|^\\.\\.?$")
    dired-listing-switches "-Faho --color=auto --group-directories-first --sort=time")
  (dired-quick-sort-setup)
  (add-hook 'dired-mode-hook
    (lambda ()
      (dired-hide-details-mode)
      (dired-sort-toggle-or-edit)
      ;;(dired-omit-mode)
      )))

;;(defvar ls-opts (shell-command-to-string ". ~/.bashrc; echo -n $LS_OPTS"))
;; from .bashrc (probably a better way to get this) (above)


;; GGTAGS
;;(load-file "~/dev/ggtags/ggtags.el")
(autoload 'ggtags-mode "ggtags")
(with-eval-after-load "ggtags"
  (setenv "GTAGSCONF" (expand-file-name "~/.globalrc"))
  (setenv "GTAGSLABEL" "pygments")
  (setq ggtags-process-environment '("~/.globalrc"))
  (setq ggtags-use-project-gtagsconf t)
  ;; custom keys for ggtags
  (define-key ggtags-mode-map (kbd "M-g d") 'ggtags-find-definition)
  (define-key ggtags-mode-map (kbd "M-g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags))
(add-hook 'prog-mode-hook 'ggtags-mode)
