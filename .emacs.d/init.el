;; ----- {{{
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-surround rust-mode markdown-mode gist minimap magit dracula-theme evil ranger cdlatex auctex graphviz-dot-mode avy flycheck-irony company-irony irony undo-tree company helm yasnippet))))
;; https://melpa.org/#/getting-started
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; }}}

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ---- {{{
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))
;; }}}

;; ----- {{{
;; Theme
(load-theme 'dracula t)
;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    ;; :family "Inconsolata for Powerline"
                    :height 110
                    :weight 'normal
                    :width 'normal)
;; }}}

;; Auto start server if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

;; List buffers in current window (instead of list-buffers)
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Disable ^Z
;; https://stackoverflow.com/a/28205749/5842888
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'my-suspend-frame)
(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Not to split window at startup
;; https://emacs.stackexchange.com/a/5875
(setq inhibit-startup-screen t)

;; Prompt for confirmation when leaving Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Display column number in mode line
(setq column-number-mode t)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Show Paren mode
(show-paren-mode 1)

;; Enable Global Undo-Tree mode
(global-undo-tree-mode)

;; Electric Pair mode
(electric-pair-mode 1)

;; ----- yasnippet {{{
;; Enable Yas-Global mode
(yas-global-mode 1)
;; }}}

;; ----- helm {{{
(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
;; }}}

;; ----- company {{{
;; Integration with company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1)
;; Speed up company-mode
;; https://emacs.stackexchange.com/a/23937
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0) ;; This is believed to be power-comsuming
;; }}}

;; ----- irony-mode {{{
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; flycheck & flycheck-irony
(global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; }}}

;; ----- nlinum-relative {{{
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
;; }}}

;; ----- avy {{{
(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)
;; }}}

;; ----- evil {{{
;; Disable visual-mode automatically copies selected text
;; https://stackoverflow.com/a/38286385
(fset 'evil-visual-update-x-selection 'ignore)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
;; ----- +++++ evil-surround {{{
(add-hook 'prog-mode-hook 'turn-on-evil-surround-mode)
;; }}}
;; }}}

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 100
      recentf-max-saved-items 100)

(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/saves/")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ; use versioned backups

;; ----- {{{
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "cdlatex")
(load-library "cfparser")
;; }}}
