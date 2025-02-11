;; Configuraciónes basicas
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(auto-fill-mode t)
(cua-mode 1)
(set-frame-font "CaskaydiaCove Nerd Font Mono-18" nil t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])  ; 32 SPACE, 183 MIDDLE DOT '·', 46 FULL STOP '.'
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE
                                     ; RIGHT-POINTING TRIANGLE '▷', 92
                                        ; BACKSLASH '\'
        ))
(global-whitespace-mode 1)


;; straight.el
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(eval-when-compile
  (require 'use-package))

;; Temas locales
(setq custom-safe-themes t)
(use-package emacs
  :straight nil
  :load-path "themes/"
  :init
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
  :config
  (load-theme 'gruber-darker t))


;; Ido
(require 'ido)
(ido-mode t)
(ido-everywhere 1)


;; Company
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))


;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; Org-mode
(use-package org
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence  "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "|" "CANCELED(c)"" NOOPTIONS(n)")))
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.25))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1))))
  :hook
  (org-mode . org-indent-mode)
  :bind ("C-c a". org-agenda))
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Wich-hey
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))


;; Icons
(use-package all-the-icons
  :ensure t)
(use-package nerd-icons
  :ensure t)


;; Ver imagenes desde links
(use-package uimage
  :ensure t
  :diminish
  :custom 
  (org-startup-with-inline-images t)
  :hook
  (org-mode . uimage-mode))


;; Multi-cursores
(use-package multiple-cursors
  :ensure t
  :config
          (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
          (global-set-key (kbd "C->")         'mc/mark-next-like-this)
          (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
          (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
          (global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
          (global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this))


;; Odin-mode
(load-file "~/.emacs.d/treesiter/odin-ts-mode/odin-ts-mode.el")
(setq treesit-language-source-alist
  '((odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("~/.odin/ols"))))
(add-hook 'odin-ts-mode-hook #'eglot-ensure)
