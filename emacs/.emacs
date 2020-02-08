;; Config --- Summary

;;; Commentary:
;;; I want to satisfy the linter.  I live to satisfy the linter.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; Auto install packages through use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Emacs config

;; Remove bits of UI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Don't litter so much
(setq make-backup-files nil)

;; Spaces not tabs. I'm not a monster.
(setq-default indent-tabs-mode nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 4
      scroll-conservatively 100000)

(electric-pair-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)

(defconst prettify-symbols-alist
  '(("lambda"  . ?λ)))
(global-prettify-symbols-mode +1)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/life.org"))

;; Better formatting for interactive SQL buffers
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; Set to ssh as opposed to scp
(setq tramp-default-method "ssh")

(defun edit-remote (user host)
  "Open home directory of USER on HOST."
  (interactive "Muser: \nMhost: ")
  (find-file (concat "/ssh:" user "@" host ":~/")))

(defun edit-remote-sudo (user host)
  "Hop to HOST as USER then open root directory of HOST as root."
  (interactive "Muser: \nMhost: ")
  (find-file (concat "/ssh:" user "@" host "|sudo:root@" "host" ":/")))

(add-to-list 'default-frame-alist '(font . "-ADBO-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(set-frame-font "-ADBO-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Packages
(use-package evil
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-c k") 'counsel-ag))

(use-package swiper)
(use-package counsel)
(use-package hydra)
(use-package ivy-hydra)

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile)

(use-package night-owl-theme
  :config
  (load-theme 'night-owl t))

(use-package flycheck
  :after hydra
  :init
  (global-flycheck-mode)
  :bind ("C-c ! !" . hydra-flycheck/body)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-eslint)
                        '(javascript-jshint)
                        '(racket)))

  (flycheck-add-mode 'javascript-standard 'js2-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)

  (set-face-attribute 'flycheck-error nil
                      :underline nil
                      :weight 'extra-bold
                      :foreground "red")

  (defhydra hydra-flycheck (:color blue)
    "
    ^
    ^Flycheck^          ^Errors^            ^Checker^
    ^────────^──────────^──────^────────────^───────^─────
    _q_ quit            _N_ previous        _?_ describe
    _M_ manual          _n_ next            _d_ disable
    _v_ verify setup    _f_ check           _m_ mode
    ^^                  _l_ list            _s_ select
    ^^                  ^^                  ^^
    "
    ("q" nil)
    ("N" flycheck-previous-error :color pink)
    ("n" flycheck-next-error :color pink)
    ("?" flycheck-describe-checker)
    ("M" flycheck-manual)
    ("d" flycheck-disable-checker)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors)
    ("m" flycheck-mode)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup)))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pipenv
  :after projectile

  ;; Not sure if pipenv-activate should be necessary
  ;; but seems like it doesn't autmatically happen
  ;; otherise!
  :hook (python-mode . pipenv-mode)
        (python-mode . pipenv-activate))

(use-package pyvenv
  :after projectile
  :config

  (defun pyvenv-autoload ()
    "Automatically load virtual env using .venv."

    (let* ((pdir (projectile-project-root))
           (pfile (concat pdir ".venv")))
      (if (file-exists-p pfile)
          (pyvenv-activate (with-temp-buffer
                             (insert-file-contents pfile)
                             (concat pdir
                                     (car (split-string (buffer-string)))))))))

  (add-hook 'python-mode-hook 'pyvenv-autoload))

(use-package rust-mode)
(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package kotlin-mode)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)

         (js2-mode . tide-setup)
         (rjsx-mode . tide-setup)
         (typescript-mode . tide-setup)
         (web-mode . tide-setup))
  :config
  (tide-hl-identifier-mode +1)
  (flycheck-add-next-checker 'javascript-standard 'javascript-tide 'append)
  (flycheck-add-next-checker 'javascript-standard 'jsx-tide 'append))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil))

(use-package rjsx-mode
  :mode ("components\\/.*\\.js\\'"
         "layouts\\/.*\\.js\\'"
         "pages\\/.*\\.js\\'")
  :config
  (set-face-attribute 'rjsx-attr nil
                      :underline nil
                      :weight 'bold
                      :foreground "#f8f8f2")
  (set-face-attribute 'rjsx-tag nil
                      :underline nil
                      :weight 'bold
                      :foreground "#50fa7b"))

(use-package json-mode)

(use-package tern)

(use-package indium)

;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/images/vim.png")
  (setq dashboard-banner-logo-title "Press any button to continue"))

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (emacs-lisp . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (racket-mode . paredit-mode)
         (emacs-lisp . paredit-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode)))

(use-package cider)

(use-package geiser
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "/home/milo/guile-prefix/lib/guile/2.2/ccache")
    (add-to-list 'geiser-guile-load-path "/home/milo/guile-prefix/lib/guile/2.2/site-ccache")))

(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package terraform-mode)

(use-package magit
  :config
  (setq git-commit-summary-max-length 80)
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package evil-magit)

(use-package forge
  :after magit)

(use-package vterm)

(use-package spaceline
  :config
  (spaceline-emacs-theme))

(use-package olivetti)

(use-package counsel-spotify
  :config
  (load-file "~/.emacs.d/spotify-secrets.el"))

(use-package twittering-mode
  :config
  (setq twittering-icon-mode t)
  (setq twittering-connection-type-order '(wget curl urllib-http native urllib-https))
  (setq twittering-use-master-password t))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-emoji-set "openmoji"))

(use-package pollen-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (swiper ivy use-package emojify twittering-mode ivy-hydra counsel-spotify counsel terraform-mode spaceline multi-term rjsx-mode tide pipenv glsl-mode tern evil-magit magit yaml-mode rainbow-mode evil-surround rainbow-delimiters cider indium company olivetti beacon dashboard paredit js2-mode web-mode flycheck projectile dracula-theme evil)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:background "#0788f9" :foreground "#f8f8f2"))))
 '(powerline-active2 ((t (:background "#0788f9" :foreground "#f8f8f2"))))
 '(spaceline-evil-normal ((t (:background "#ffc405" :foreground "#000000" :inherit (quote mode-line)))))
 '(spaceline-highlight-face ((t (:background "#ffc405" :foreground "#000000" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "#ffc405" :foreground "#000000" :inherit (quote mode-line))))))

(provide '.emacs)
;;; .emacs ends here
