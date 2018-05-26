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
    (cdlatex auctex graphviz-dot-mode avy flycheck-irony company-irony irony undo-tree nlinum company helm yasnippet))))
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

;; ----- {{{
;; Theme
(load-theme 'leuven)
(set-face-attribute 'default nil :height 105)
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
(add-to-list 'load-path "/home/anh-phuc/git/github/gnull/cfparser")
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
       "done;"))
;; }}}

;; ----- avy {{{
(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)
;;}}}

;; ----- cdlatex {{{
(setq cdlatex-simplify-sub-super-scripts nil)
(setq cdlatex-math-modify-alist
      '((?B "\\mathbb" nil t nil nil)
        (?R "\\textrm" nil t nil nil)
        (?F "\\mathfrak" nil t nil nil)
        (?_ "_" nil t nil nil)
        (?^ "^" nil t nil nil)
        (?l "_" nil t nil nil)
        (?h "^" nil t nil nil)
        (?a "" nil t nil nil "(" ")") ; Needs custom cdlatex
        ))
(setq cdlatex-math-symbol-alist
      '((?B "\\mathbb{?}" "\\mathbf{?}")
        (?R "\\sqrt{?}" "\\sqrt[?]{}")
        (?I "\\implies")
        (?. "\\cdot" "\\circ" "\\dots")
        ;; (?: ";")
        ))
(setq cdlatex-command-alist
      ;; Assuming the matching pair is already inserted
      ;; TODO: Handle existing matching pair through hooks
      '(
        ;; ("p{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ;; ("p(" "Insert a \\( \\) pair" "\\(?\\" cdlatex-position-cursor nil t t)
        ;; TODO: Figure out how to get this work
        ;; ("\\(" "Insert a \\( \\) pair" "\\(?\\" cdlatex-position-cursor nil t t)
        ;; ("\\{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ;; ("l[" "Insert a \\[ ... \\] pair" "\\[ ? \\]" cdlatex-position-cursor nil t t)
        ("lp" "Insert a \\[ ... \\] pair" "\\[ ? \\]" cdlatex-position-cursor nil t t)
        ;; ("l{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ("dim" "Insert \\dim" "\\dim" cdlatex-position-cursor nil t t)
        ("rank" "Insert \\rank" "\\rank" cdlatex-position-cursor nil t t)
        ("nullity" "Insert \\nullity" "\\nullity" cdlatex-position-cursor nil t t)
        ("le" "Insert \\le" "\\le" cdlatex-position-cursor nil t t)
        ("ge" "Insert \\ge" "\\ge" cdlatex-position-cursor nil t t)
        ("lpi" "Insert \\pi" "\\pi" cdlatex-position-cursor nil t t)
        ("lb" "Insert a \\{ \\} pair" "\\{?\\}" cdlatex-position-cursor nil t t)
        ("lB" "Insert a \\{ \\} pair" "\\{ ? \\}" cdlatex-position-cursor nil t t)
        ("l8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("-8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("int" "Insert \\int_{}^{}" "\\int_{?}^{}" cdlatex-position-cursor nil t t)
        ("int0" "Insert \\int" "\\int" cdlatex-position-cursor nil t t)
        ("in" "Insert \\in" "\\in" cdlatex-position-cursor nil t t)
        ("to" "Insert \\to" "\\to" cdlatex-position-cursor nil t t)
        ("pm" "Insert \\pm" "\\pm" cdlatex-position-cursor nil t t)
        ("mod" "Insert \\mod" "\\mod" cdlatex-position-cursor nil t t)
        ("gcd" "Insert \\gcd" "\\gcd" cdlatex-position-cursor nil t t)
        ("lcm" "Insert \\lcm" "\\lcm" cdlatex-position-cursor nil t t)
        ("df" "Insert \\dfrac{}{}" "\\dfrac{?}{}" cdlatex-position-cursor nil t t)
        ("tf" "Insert \\tfrac{}{}" "\\tfrac{?}{}" cdlatex-position-cursor nil t t)
        ("root" "Insert \\sqrt{}" "\\sqrt{?}" cdlatex-position-cursor nil t t)
        ("abs" "Insert \\abs{}" "\\abs{?}" cdlatex-position-cursor nil t t)
        ("norm" "Insert \\norm{}" "\\norm{?}" cdlatex-position-cursor nil t t)
        ("th" "Insert \\theta" "\\theta" cdlatex-position-cursor nil t t)
        ("sin" "Insert \\sin" "\\sin" cdlatex-position-cursor nil t t)
        ("cos" "Insert \\cos" "\\cos" cdlatex-position-cursor nil t t)
        ("tan" "Insert \\tan" "\\tan" cdlatex-position-cursor nil t t)
        ("sec" "Insert \\sec" "\\sec" cdlatex-position-cursor nil t t)
        ("asin" "Insert \\arcsin" "\\arcsin" cdlatex-position-cursor nil t t)
        ("acos" "Insert \\arccos" "\\arccos" cdlatex-position-cursor nil t t)
        ("atan" "Insert \\arctan" "\\arctan" cdlatex-position-cursor nil t t)
        ("rvert" "Insert \\rvert" "\\rvert" cdlatex-position-cursor nil t t)
        ("ln" "Insert \\ln" "\\ln" cdlatex-position-cursor nil t t)
        ("prodl" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("int" "Insert \\int_{}^{}" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
        ("liml" "Insert \\lim\\limits_{}" "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
        ;; ("cen" "Insert an inline CENTER environment template" "\\begin{center} ? \\end{center}" cdlatex-position-cursor nil t t)
        ("ctr" "Insert a CENTER environment template" "" cdlatex-environment ("center") t nil) ; This is a bit buggy, and does not respect indentation
        ))
(setq cdlatex-math-symbol-prefix ?\;)
(add-hook 'org-cdlatex-mode-hook
          (lambda ()
            ;; (define-key org-cdlatex-mode-map (kbd "'") 'nil)
            (define-key org-cdlatex-mode-map (kbd ";") 'cdlatex-math-symbol)))
;; }}}

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 100
      recentf-max-saved-items 100)
