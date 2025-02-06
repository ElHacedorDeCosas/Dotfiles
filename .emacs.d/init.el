
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font Mono-18"))

(menu-bar-mode 1)
(tool-bar-mode 1)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'after-init-hook 'global-company-mode)

(setq shell-file-name "/usr/bin/fish")
(setq explicit-shell-file-name "/usr/bin/fish")
(add-to-list 'exec-path "/home/gonzaloc/.odin/")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


(setq whitespace-style '(face spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])  ; 32 SPACE, 183 MIDDLE DOT '·', 46 FULL STOP '.'
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE '▷', 92 BACKSLASH '\'
        ))
(global-whitespace-mode 1)

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

(elpaca (transient :host github :repo "magit/transient"))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;;(use-package evil :ensure t :demand t)


;; Doom-themes
(elpaca doom-themes)
(elpaca auto-complete)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

(use-package company :ensure t :config (add-hook 'after-init-hook 'global-company-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


(load-file "~/.emacs.d/treesiter/odin-ts-mode/odin-ts-mode.el")

(setq treesit-language-source-alist
  '((odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")))

;;(use-package odin-ts-mode
;;  :ensure (local :repo "~/.emacs.d/treesiter/odin-ts-mode/")
;;  :mode ("\\.odin\\'" . odin-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("~/.odin/ols"))))

(add-hook 'odin-ts-mode-hook #'eglot-ensure)

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
