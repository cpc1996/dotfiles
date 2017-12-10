;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; (require 'package) ;; Might not be needed in latest versions

(setq package-list '(package-selected-packages))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(font-use-system-font t)
 '(package-selected-packages
   (quote
    (focus avy racket-mode org-download org-ref org cdlatex auctex undo-tree company)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 105 :width normal)))))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (p package-selected-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Automatically install packages: https://stackoverflow.com/a/10093312

;; Auto start server if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

(desktop-save-mode 1)

;; Not to split at startup
;; https://emacs.stackexchange.com/a/5875
(setq inhibit-startup-screen t)

(setq confirm-kill-emacs 'y-or-n-p)

;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
;; make electric-pair-mode work on more brackets
(electric-pair-mode 1)
(defvar org-electric-pairs
  '((?\( . ?\)) ;; Not working when having bullet points such as 1) 2) 3)...
    ;; (?\" . ?\")
    ;; (?\{ . ?\})
    ;; (?\[ . ?\])
    (?\$ . ?\$) ; LaTeX
    ;; (?\* . ?\*)
    ;; (?\| . ?\|) ; LaTeX
    ))
(setq electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
(setq electric-pair-text-pairs electric-pair-pairs)

(setq cdlatex-simplify-sub-super-scripts nil)
(setq org-pretty-entities t)
(setq cdlatex-math-modify-alist
      '((?B "\\mathbb" nil t nil nil)
        (?R "\\textrm" nil t nil nil)
        (?_ "_" nil t nil nil)
        (?^ "^" nil t nil nil)
        (?l "_" nil t nil nil)
        (?h "^" nil t nil nil)))
(setq cdlatex-math-symbol-alist
      '((?B "\\mathbb{?}")
        (?R "\\sqrt{?}" "\\sqrt[?]{}")
        (?I "\\implies")))
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
        ("lb" "Insert a \\{ \\} pair" "\\{?\\}" cdlatex-position-cursor nil t t)
        ("lB" "Insert a \\{ \\} pair" "\\{ ? \\}" cdlatex-position-cursor nil t t)
        ("l8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("in" "Insert \\in" "\\in" cdlatex-position-cursor nil t t)
        ("pm" "Insert \\pm" "\\pm" cdlatex-position-cursor nil t t)
        ("mod" "Insert \\mod" "\\mod" cdlatex-position-cursor nil t t)
        ("gcd" "Insert \\gcd" "\\gcd" cdlatex-position-cursor nil t t)
        ("lcm" "Insert \\lcm" "\\lcm" cdlatex-position-cursor nil t t)
        ("abs" "Insert | |" "|?|" cdlatex-position-cursor nil t t)
        ("th" "Insert \\theta" "\\theta" cdlatex-position-cursor nil t t)
        ("sin" "Insert \\sin" "\\sin" cdlatex-position-cursor nil t t)
        ("cos" "Insert \\cos" "\\cos" cdlatex-position-cursor nil t t)
        ("ln" "Insert \\ln" "\\ln" cdlatex-position-cursor nil t t)
        ("prodl" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("liml" "Insert \\lim\\limits_{}" "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("cen" "Insert an inline CENTER environment template" "\\begin{center} ? \\end{center}" cdlatex-position-cursor nil t t)
        ("ctr" "Insert a CENTER environment template" "" cdlatex-environment ("center") t nil) ; This is a bit buggy, and does not respect indentation
        ))

(setq cdlatex-math-symbol-prefix ?\;)
(add-hook 'org-cdlatex-mode-hook
          (lambda ()
            ;; (define-key org-cdlatex-mode-map (kbd "'") 'nil)
            (define-key org-cdlatex-mode-map (kbd ";") 'cdlatex-math-symbol)))

;; (require 'cdlatex)
;; (cdlatex-reset-mode)
;; (with-eval-after-load 'cdlatex)


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

;; TODO: Reorganize these.
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(load-theme 'leuven) ;; Very clear.
;; (load-theme 'wheatgrass)
;; (load-theme 'whiteboard)
;; (load-theme 'tango-dark)
;; (load-theme 'light-blue) ;; Mediocre.
;; (load-theme 'tsdh-light)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Copied from AUCTeX Quick Start
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)


(global-undo-tree-mode)

(require 'org)

;; allow alphabetical list
(setq org-list-allow-alphabetical t)

;; (setq org-latex-create-formula-image-program 'dvipng) ;; This might
;; not be necessary.

;; https://github.com/purcell/emacs.d/issues/273#issuecomment-295058188
(setq inhibit-compacting-font-caches t)

;; auto-fill mode
;;(setq-default auto-fill-function 'do-auto-fill) ;; on all major mods
(add-hook 'org-mode-hook 'do-auto-fill)

(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

;; auto CDLaTeX for Org
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; auto pretty entities for Org
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(setq
 org-startup-with-inline-images t
 org-startup-with-latex-preview t)

;; auto enable Org-Indent minor mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Scale Org LaTeX overlay up a bit
(setq org-format-latex-options (plist-put org-format-latex-options
                                          :scale 1.35))

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Disable tool bar
(tool-bar-mode -1)

;; Integration with company-mode
(add-hook 'after-init-hook 'global-company-mode)
;; (defun add-pcomplete-to-capf ()
  ;; (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
(setq company-minimum-prefix-length 1)

;; Speed up company-mode
;; https://emacs.stackexchange.com/a/23937
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0) ;; This is commented to be power-comsuming
;; TODO: Disable company-mode when in LaTeX fragments

;; Currently just for LaTeX with Org
;; (require 'company)

;; https://emacs.stackexchange.com/a/13290
(with-eval-after-load 'company
  (define-key company-active-map [tab] nil)
  ;; https://github.com/syl20bnr/spacemacs/issues/1372#issuecomment-96466139
  (define-key company-active-map [escape] 'company-abort))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; racket-mode {{{

;; TAB should do completion as well as indentation
(setq tab-always-indent 'complete)
;; }}}


;; linum-mode appears to have low performance
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'linum-mode)



;; some default bindings for avy
(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char)
;; (require 'avy)

;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/
;; (setq org-image-actual-width (/ (display-pixel-width) 3))
;; (setq org-image-actual-width '(300))
;; (setq org-image-actual-width 400)


;; Key mapping {{{
(global-set-key "\C-x\C-b" 'buffer-menu) ;; Instead of list-buffers (list buffers in other window)
;; }}}
