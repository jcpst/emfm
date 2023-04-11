;; ============================================================================
;;; Emfy 0.2.0 <https://github.com/susam/emfy>
;; ============================================================================

;; Customize user interface.
(menu-bar-mode 1)                  ; Not digging the "hide the menu bar" trend.
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
;(make-directory "~/.tmp/emacs/auto-save/" t)
;(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
;(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Auto Save
(auto-save-visited-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-use-notify nil)
;; Do not move the current file while creating backup.
;(setq backup-by-copying t)


;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; ============================================================================
;; Install packages.
;; ============================================================================

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(evil
                   general
                   magit
                   paredit
                   projectile
                   rainbow-delimiters
                   which-key))
  (unless (package-installed-p package) (package-install package)))

;; ============================================================================
;; Package Configuration.
;; ============================================================================

;; Enable Evil.
(evil-mode 1)

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (emacs-lisp . t)
   (js . t)
   (ledger . t)
   (lilypond . t)
   (python . t)
   (R . t)
   (sqlite . t)))

(custom-set-faces
 '(org-block ((t (:background "#212121" :extend t)))))

;; ============================================================================
;; Custom command.
;; ============================================================================

(defun open-init-el ()
  "Open the main user configuration file."
  (interactive)
  (find-file "~/.emacs.emfy/init.el"))

(defun open-main-org-file ()
  "Open the main org-mode file."
  (interactive)
  (find-file "~/org/main.org"))

;; ============================================================================
;; Custom key sequences.
;; ============================================================================

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 "TAB" '(switch-to-other-buffer :which-key "prev buffer")
 "SPC" '(execute-extended-command :which-key "M-x")

 "a" '(:ignore t :which-key "Applications")

 "b" '(:ignore t :which-key "Buffers")
 "bd" '(kill-buffer :which-key "delete buffer")

 "f" '(:ignore t :which-key "Files")
 "fi" '(open-init-el :which-key "open init.el")
 "ff" 'find-file
 "fo" 'open-main-org-file

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
