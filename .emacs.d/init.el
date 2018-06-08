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
    (magit dracula-theme evil ranger cdlatex auctex graphviz-dot-mode avy flycheck-irony company-irony irony undo-tree nlinum company helm yasnippet))))
;; https://melpa.org/#/getting-started
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

;; ----- nlinum {{{
(add-hook 'prog-mode-hook 'nlinum-mode)
;; }}}

;; ----- cfparser {{{
(add-to-list 'load-path "~/git/github/gnull/cfparser")
;; https://derdaniel.me/emcas-how-to-check-if-a-package-is-installed
(when (package-installed-p 'cf-mode)
  (require 'cf-mode)
  (add-hook 'find-file-hook 'cf-mode)  ; enable cf-mode for all open files
  (setq cf-test-command
	(concat
	 "set -eo pipefail; "
	 ;; "g++ -g -Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wshift-overflow=2 -Wduplicated-cond -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover -fstack-protector sol.cpp; "
	 ;; "g++ -g -Wall -Wextra -O2 -Wno-misleading-indentation -D_GLIBCXX_DEBUG -D_FORTIFY_SOURCE=2 sol.cpp; "
	 "g++ -g -Wall -Wextra -pedantic -std=c++17 -O2 -Wformat=2 -Wfloat-equal -Wlogical-op -Wshift-overflow=2 -Wduplicated-cond -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=undefined -fno-sanitize-recover -fstack-protector -Wno-misleading-indentation sol.cpp; "
	 "for i in `ls *.in | sed 's/.in//'`; do "
	 "  echo \"\nTest #$i:\"; "
	 "  echo \"Running...\"; "
	 "  ./a.out < $i.in | tee out; "
	 "  if [[ -f $i.out ]]; then "
	 "    echo \"Checking...\"; "
	 "    diff -b out $i.out && rm -f out; "
	 "  else "
	 "    echo \"No output file...\"; "
	 "  fi; "
	 "done;")))
;; }}}

;; ----- avy {{{
(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)
;;}}}

;; ----- evil {{{
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
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
;; }}}
