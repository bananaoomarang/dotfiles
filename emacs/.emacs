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

(global-so-long-mode 1)

;; As recommended for lsp-mode performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(setq dired-listing-switches "-alh --group-directories-first")

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
;; (add-to-list 'default-frame-alist '(font . "Iosevka"))
;; (set-frame-font "Iosevka")

;; Packages

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(vterm dired magit sly))
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\|pdf\\|docx\\)$"
           "xdg-open" (file))))
  (openwith-mode +1))

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)

  :custom-face
  (ivy-current-match ((t
                       (:background "#1D3B53"
                                    :foreground "#ffffff"))))

  :bind
  ("C-c s" . swiper-isearch)
  ("C-s" . counsel-rg)
  ("M-y" . counsel-yank-pop)
  ("C-x b" . counsel-switch-buffer)

  :config
  (ivy-mode 1))

(use-package swiper)
(use-package counsel)
(use-package hydra)
(use-package ivy-hydra)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-du)

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; (use-package counsel-projectile
;;   :after projectile
;;   :config
;;   (counsel-projectile-mode))

(use-package eglot
  :hook
  (python-mode . eglot-ensure))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook
;;   (python-mode
;;    .
;;    (lambda ()
;;      (setq lsp-pyright-multi-root nil)
;;      (require 'lsp-pyright)
;;      (lsp))))

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
;;   (setq lsp-completion-provider :capf)
;;   (setq lsp-enable-snippet nil)
;;   (setq lsp-pyls-plugins-flake8-enabled t)
;;   (setq lsp-pyright-multi-root nil)
;;   (setq lsp-pyright-venv-path "/home/milo/.cache/pypoetry/virtualenvs")
;;   (setq lsp-enable-file-watchers nil)
;;   ;;(setq lsp-ui-doc-enable nil)

;;   (add-hook
;;    'lsp-managed-mode-hook
;;    (lambda ()
;;      (when (derived-mode-p 'python-mode)
;;        (flycheck-add-next-checker 'lsp 'python-flake8))))

;;   :commands lsp)

;; (use-package lsp-ui :commands lsp-ui-mode)

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook
;;   (python-mode
;;    .
;;    (lambda ()
;;      (setq lsp-pyright-multi-root nil)
;;      (require 'lsp-pyright)
;;      (lsp))))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package solarized-theme
  :config
  ;; This is a workaround to get rid of an ugly underline: https://github.com/bbatsov/solarized-emacs/issues/220
  (advice-add #'enable-theme :after
              (lambda (&rest _)
                (set-face-attribute 'mode-line nil :height 0.8 :overline nil :underline nil)))
  (load-theme 'solarized-dark t))

(use-package flycheck
  :after hydra
  :init
  (global-flycheck-mode)
  :bind ("C-c ! !" . hydra-flycheck/body)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(python-mypy)
                        '(javascript-eslint)
                        '(javascript-jshint)
                        '(racket)))

  ;; (flycheck-add-mode 'javascript-standard 'js2-mode)
  (flycheck-add-mode 'javascript-standard 'rjsx-mode)

  (set-face-attribute 'flycheck-error nil
                      :underline nil
                      :weight 'extra-bold
                      :foreground "red"))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . #'company-complete-selection)))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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

  (defun pyvenv-poetry-autoload ()
    "Automatically load poetry venv"

    (let* ((pname (projectile-project-name))
           (pdir (projectile-project-root))
           (pyproj (concat pdir "pyproject.toml")))
      (when (and
             (not (cl-search pname pyvenv-virtual-env))
             (file-exists-p pyproj))
        (let ((env-path (substring (shell-command-to-string "poetry env info -p") 0 -1)))
          (pyvenv-activate env-path)))))

  (add-hook 'python-mode-hook 'pyvenv-autoload)
  (add-hook 'python-mode-hook 'pyvenv-poetry-autoload))

(use-package rust-mode)
(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package kotlin-mode)

(use-package web-mode
  :mode (".tsx$")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; (use-package js2-mode)

(use-package rjsx-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  
  (set-face-attribute 'rjsx-attr nil
                      :underline nil
                      :weight 'bold
                      :foreground "#f8f8f2")
  (set-face-attribute 'rjsx-tag nil
                      :underline nil
                      :weight 'bold
                      :foreground "#50fa7b"))

(use-package tide
  :after (company flycheck)
  :hook
  ;; (js2-mode . tide-setup)
  ;; (js2-mode . tide-hl-identifier-mode)

  (rjsx-mode . tide-setup)
  (rjsx-mode . tide-hl-identifier-mode)

  (web-mode . tide-setup)
  (web-mode . tide-hl-identifier-mode)

  :config
  (tide-hl-identifier-mode +1)
  (flycheck-add-next-checker 'javascript-standard 'javascript-tide 'append))

(use-package json-mode
  :config
  (setq js-indent-level 2))

(use-package add-node-modules-path
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook 'add-node-modules-path))
  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook 'add-node-modules-path)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/images/vim-pixel.png")
  (setq dashboard-banner-logo-title "Press any button to continue"))

;; (use-package paredit
;;   :hook
;;   (clojure-mode . paredit-mode)
;;   (clojurescript-mode . paredit-mode)
;;   (lisp-mode . paredit-mode)
;;   (emacs-lisp-mode . paredit-mode)
;;   (scheme-mode . paredit-mode)
;;   (racket-mode . paredit-mode)
;;   (slime-repl-mode . paredit-mode))

(use-package lispy
  :hook
  (clojure-mode . lispy-mode)
  (clojurescript-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (racket-mode . lispy-mode))


(use-package lispyville
  :after lispy
  :hook
  (clojure-mode . lispyville-mode)
  (clojurescript-mode . lispyville-mode)
  (lisp-mode . lispyville-mode)
  (emacs-lisp-mode . lispyville-mode)
  (scheme-mode . lispyville-mode)
  (racket-mode . lispyville-mode)
  (sly-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators commentary slurp/barf-lispy c-w (escape inert) (additional-movement normal visual motion))))

(use-package yaml-mode
  ;;:hook
  ;; (yaml-mode
  ;;  .
  ;;  (lambda ()
  ;;    (lsp)))

  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (when (featurep 'yaml-mode)
    (define-derived-mode cfn-mode yaml-mode
      "Cloudformation"
      "Cloudformation template mode.")
    
    (add-to-list 'magic-mode-alist
                 '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-mode))

    (when (featurep 'flycheck)
      (flycheck-define-checker cfn-lint
        "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

        :command ("cfn-lint" "-f" "parseable" source)
        :error-patterns ((warning line-start (file-name) ":" line ":" column
                                  ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                  (id "W" (one-or-more digit)) ":" (message) line-end)
                         (error line-start (file-name) ":" line ":" column
                                ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                (id "E" (one-or-more digit)) ":" (message) line-end))
        :modes (cfn-mode))

      (add-to-list 'flycheck-checkers 'cfn-lint)
      (add-hook 'cfn-mode-hook 'flycheck-mode))))

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode)))

(use-package flycheck-clj-kondo
  :config
  (require 'flycheck-clj-kondo))

(use-package cider)

(use-package geiser
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "/home/milo/guile-prefix/lib/guile/2.2/ccache")
    (add-to-list 'geiser-guile-load-path "/home/milo/guile-prefix/lib/guile/2.2/site-ccache")))

(use-package sly
  :config
  (setq inferior-lisp-program "/sbin/sbcl"))

(use-package sly-quicklisp
  :after sly)

(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package magit
  :config
  (setq git-commit-summary-max-length 80)
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package forge
  :after magit)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package olivetti
  :config
  (global-set-key (kbd "C-c o") 'olivetti-mode))

(use-package vterm)

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
  ;;:config
  ;;(setq emojify-emoji-set "openmoji")
)

(use-package pollen-mode)

(use-package brightscript-mode
  :mode "\\.brs\\'")

(use-package csv-mode)

;; Spellcheck in org mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(brightscript-mode-indent-offset 4)
 '(css-indent-offset 2)
 '(dired-du-size-format t)
 '(native-comp-async-report-warnings-errors nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(swiper ivy use-package emojify twittering-mode ivy-hydra counsel-spotify counsel multi-term rjsx-mode tide glsl-mode tern magit yaml-mode rainbow-mode evil-surround rainbow-delimiters cider indium company olivetti beacon dashboard paredit js2-mode web-mode flycheck projectile dracula-theme evil))
 '(safe-local-variable-values
   '((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "#1D3B53" :foreground "#ffffff")))))

(provide '.emacs)
;;; .emacs ends here
