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
    (avy racket-mode org-download org-ref org cdlatex auctex undo-tree)))
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

;; Not to split at startup
;; https://emacs.stackexchange.com/a/5875
(setq inhibit-startup-screen t)

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

;; auto CDLaTeX for Org
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; auto pretty entities for Org
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(setq
 org-startup-with-inline-images t
 org-startup-with-latex-preview t)

;; auto enable Org-Indent minor mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Scale Org LaTeX overlay up a bit
(setq org-format-latex-options (plist-put org-format-latex-options
                                          :scale 1.25))

;; https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Disable tool bar
(tool-bar-mode -1)

;; racket-mode {{{

;; TAB should do completion as well as indentation
(setq tab-always-indent 'complete)
;; }}}


;; some default bindings for avy
(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char)
;; (require 'avy)

;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/
;; (setq org-image-actual-width (/ (display-pixel-width) 3))
;; (setq org-image-actual-width '(300))
;; (setq org-image-actual-width 400)
