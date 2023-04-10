;; ============================================================================
;;; Emfy 0.2.0 <https://github.com/susam/emfy>
;; ============================================================================

;; Customize user interface.
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; Interactively do things.
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; ============================================================================
;; Install packages.
;; ============================================================================

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Don't forget to run this afterward:
; M-x package-refresh-contents
(dolist (package '(company
                   evil
                   flycheck
                   general
                   magit
                   ob-mermaid
                   ox-reveal
                   paredit
                   projectile
                   rainbow-delimiters
                   which-key))
  (unless (package-installed-p package) (package-install package)))

;; ============================================================================
;; Package Configuration.
;; ============================================================================

;; Enable Company.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable Evil.
(evil-mode 1)

;; Enable Flycheck
(global-flycheck-mode)

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable and Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Enable which-key
(which-key-mode)

;; org-mode configuration
(setf org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-hide-emphasis-markers t)

(setq ob-mermaid-cli-path "~/scoop/persist/nodejs/bin/")

(require 'ox-reveal)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (emacs-lisp . t)
   (js . t)
   (mermaid . t)
   (python . t)
   (R . t)
   (sqlite . t)))

(custom-set-faces '(org-block ((t (:background "#eeeeee" :extend t)))))

;; ============================================================================
;; Custom command.
;; ============================================================================

(defun open-init-el ()
  "Open the main user configuration file."
  (interactive)
  (find-file "~/.emacs.emfy/init.el"))

(defun open-main-org-file ()
  "Open the main 'org-mode' file."
  (interactive)
  (find-file "~/org/main.org"))

;; ============================================================================
;; Custom key sequences.
;; ============================================================================

; https://emacs.stackexchange.com/a/28224
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 "TAB" '(switch-to-other-buffer :which-key "prev buffer")
 "SPC" '(execute-extended-command :which-key "M-x")

 "a" '(:ignore t :which-key "Applications")

 "b" '(:ignore t :which-key "Buffers")
 "bb" '(switch-to-buffer :which-key "switch buffer")
 "bd" '(kill-buffer :which-key "delete buffer")
 "be" '(eval-buffer :which-key "eval buffer")
 "bn" '(next-buffer :which-key "next buffer")

 "f" '(:ignore t :which-key "Files")
 "fi" '(open-init-el :which-key "open init.el")
 "ff" 'find-file
 "fo" 'open-main-org-file

 "g" 'magit-dispatch

 "o" '(:ignore t :which-key "Org")
 "oa" 'org-agenda
 "os" '(:ignore t :which-key "Sync with cloud")
 "osf" 'org-rclone-fetch
 "osp" 'org-rclone-push)

;; ============================================================================
;; Start server.
;; ============================================================================

(require 'server)
(unless (server-running-p)
  (server-start))
