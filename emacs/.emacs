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

(pixel-scroll-precision-mode)

(setq vc-follow-symlinks t)

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

(setq-default tab-width 4)

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

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(setq go-ts-mode-indent-offset 4) 

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

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

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-du)

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (if (null (string-match ".*exited abnormally.*" str))
                  ;;no errors, make the compilation window go away in a few seconds
                  (progn
                    (run-at-time
                     "2 sec" nil 'delete-windows-on
                     (get-buffer-create "*compilation*"))
                    (message "No Compilation Errors!"))))))

;; (use-package counsel-projectile
;;   :after projectile
;;   :config
;;   (counsel-projectile-mode))

(use-package go-mode)

(use-package yaml)

(use-package eglot
  :hook
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (project-find-functions . project-find-go-module)
  :config
  (require 'project)
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

;;(use-package tree-sitter
;;  :config
;;  (global-tree-sitter-mode)
;;  :hook
;;  (tree-sitter-mode . tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05)
  :diminish which-key-mode)

;; (use-package solarized-theme
;;   :config
;;   ;; This is a workaround to get rid of an ugly underline: https://github.com/bbatsov/solarized-emacs/issues/220
;;   (advice-add #'enable-theme :after
;;               (lambda (&rest _)
;;                 (set-face-attribute 'mode-line nil :height 0.8 :overline nil :underline nil)))
;;   (load-theme 'solarized-dark t))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t))

(use-package flycheck
  :after hydra
  :init
  (global-flycheck-mode)
  :bind
  ("C-c ! !" . hydra-flycheck/body)
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error)
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

  (add-hook 'python-ts-mode-hook 'pyvenv-autoload)
  (add-hook 'python-ts-mode-hook 'pyvenv-poetry-autoload))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package kotlin-mode)

(use-package lua-mode)

(use-package web-mode
  :mode (".tsx$")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

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
  (typescript-ts-mode . tide-setup)

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
  (setq dashboard-startup-banner "~/images/vim.png")
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
  (setq inferior-lisp-program "/sbin/sbcl")
  
  :bind
  ;; IDK Why but C-M-x doesn't seem 2 work maybe already bound
  ("C-M-z" . sly-eval-defun))

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
 '(custom-safe-themes
   '("3e28aca1f46237e88e02283ce5c57b26db8968abd9ad2b84847f84c9feafb294" "cde5d9cde639f437ace4b8a36c174f730fb17b27f1183394e8ed9db1c6ba96ee" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8da34297ccd16aa9fdf75596dc06d519a5f036179fbff95107bbecdaadf965c4" default))
 '(dired-du-size-format t)
 '(native-comp-async-report-warnings-errors nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(inkpot-theme swiper ivy use-package emojify twittering-mode ivy-hydra counsel-spotify counsel multi-term rjsx-mode tide glsl-mode tern magit yaml-mode rainbow-mode evil-surround rainbow-delimiters cider indium company olivetti beacon dashboard paredit js2-mode web-mode flycheck projectile dracula-theme evil))
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
