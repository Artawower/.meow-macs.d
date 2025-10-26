;;; -*- lexical-binding: t; -*-

;; [[file:README.org::*Env variables][Env variables:1]]
(setenv "LSP_USE_PLISTS" "true")
;; Env variables:1 ends here

;; [[file:README.org::*Variables][Variables:1]]
(setq my/user-email "artawower33@gmail.com")
;; Variables:1 ends here

;; [[file:README.org::*Paths][Paths:1]]
(setq my/config-dir "~/apps/flat-emacs/")
(setq my/config-file (concat my/config-dir "README.org"))
;; Paths:1 ends here

;; [[file:README.org::*Paths][Paths:2]]
(defun my/get-config-related-path (path)
  "Get PATH relative to the Emacs config directory."
  (concat my/config-dir path))
;; Paths:2 ends here

;; [[file:README.org::*Paths][Paths:3]]
(setq my/org-roam-dir (expand-file-name "~/Yandex.Disk.localized/Dropbox/org-roam"))
(setq my/config-path (my/get-config-related-path "README.org"))
(setq my/backup-dir (expand-file-name "~/tmp/emacs-backups"))
(setq my/private-config-path (my/get-config-related-path "private.el"))
(setq my/doom-theme-directory (expand-file-name "~/.doom.d/themes"))
(setq my/templates-dir (my/get-config-related-path "templates"))
;; Paths:3 ends here

;; [[file:README.org::*UI variables][UI variables:1]]
(setq my/font-height 14)
(setq my/font-default "Monaspace Neon Frozen")
(setq my/font-funny   "Monaspace Radon Frozen")
(setq my/ui-box-padding 8)
(setq my/org-tag-color "#ff5555")
;; UI variables:1 ends here

;; [[file:README.org::*Garbage collector][Garbage collector:1]]
(let* ((normal-gc-cons-threshold (* 100 1024 1024))
       (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold (* 20 1024 1024)))))
;; Garbage collector:1 ends here

;; [[file:README.org::*Max process][Max process:1]]
(setq read-process-output-max (* 1024 1024))
;; Max process:1 ends here

;; [[file:README.org::*Package quick start][Package quick start:1]]
(setq package-quickstart t)
;; Package quick start:1 ends here

;; [[file:README.org::*Initial mode][Initial mode:1]]
(setq initial-major-mode 'fundamental-mode)
;; Initial mode:1 ends here

;; [[file:README.org::*Buffer naming startup][Buffer naming startup:1]]
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist startup/file-name-handler-alist)
            (makunbound 'startup/file-name-handler-alist)))
;; Buffer naming startup:1 ends here

;; [[file:README.org::*Disable modeline and backups while loading][Disable modeline and backups while loading:1]]
(setq
 mode-line-format nil
 make-backup-files nil
 backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
;; Disable modeline and backups while loading:1 ends here

;; [[file:README.org::*Elpaca. Package manager.][Elpaca. Package manager.:1]]
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; NOTE: Lock file for reproducible package versions
;; (setq elpaca-lock-file "/Users/darkawower/apps/flat-emacs/elpaca.lock")
;; Elpaca. Package manager.:1 ends here

;; [[file:README.org::*Use package][Use package:1]]
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  (setq use-package-compute-statistics t))

;; Block until current queue processed.
(elpaca-wait)
;; Use package:1 ends here

;; [[file:README.org::*Melpa][Melpa:1]]
(require 'package)

(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "http://melpa.org/packages/")
                          ("melpa-mirror" . "http://melpa.milkbox.net/packages/")
                          ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ))
(customize-set-variable 'package-enable-at-startup nil)
;; NOTE: Package initialization is handled by elpaca
;; (package-initialize)
;; Melpa:1 ends here

;; [[file:README.org::*Push multiple values][Push multiple values:1]]
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))
;; Push multiple values:1 ends here

;; [[file:README.org::*Env][Env:1]]
;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > ~/.emacs.d/.local/env'
(defconst my/local-dir (concat user-emacs-directory ".local/"))

(defconst my/env-file (concat my/local-dir "env"))

(defun my/load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my/env-file))
  (my/load-envvars-file my/env-file))
;; Env:1 ends here

;; [[file:README.org::*Backups][Backups:1]]
(when my/backup-dir
  (setq backup-directory-alist `(("." . ,my/backup-dir))))
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
;; Backups:1 ends here

;; [[file:README.org::*Warning level][Warning level:1]]
(setq warning-minimum-level :emergency)
(setq warning-suppress-log-types '((comp) (undo discard-info)))
;; Warning level:1 ends here

;; [[file:README.org::*No littering][No littering:1]]
(use-package no-littering
  :demand t
  :config
  (when my/backup-dir
    (setq backup-directory-alist `(("." . ,my/backup-dir))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-by-copying t))
;; No littering:1 ends here

;; [[file:README.org::*Load private configs][Load private configs:1]]
(ignore-errors
  (when my/private-config-path
    (load my/private-config-path)))
;; Load private configs:1 ends here

;; [[file:README.org::*Browser][Browser:1]]
(when (eq system-type 'darwin)
  (setq browse-url-firefox-program nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))
;; Browser:1 ends here

;; [[file:README.org::*Wakatime][Wakatime:1]]
(use-package wakatime-mode
  :defer 2
  :config
  (global-wakatime-mode))
;; Wakatime:1 ends here

;; [[file:README.org::*Open youtrack][Open youtrack:1]]
(defvar my/youtrack-url "https://verifika.youtrack.cloud/issue")
(defconst my/youtrack-url-regexp "VW-[[:digit:]]*")

(defun my/open-current-task-youtrack-url ()
  "Open current task in YouTrack"
  (interactive)
  (when-let* ((branch-name (magit-get-current-branch))
              (matched-res (string-match my/youtrack-url-regexp branch-name))
              (task-name (substring branch-name (match-beginning 0) (match-end 0))))

    (browse-url (concat my/youtrack-url "/" task-name))))
;; Open youtrack:1 ends here

;; [[file:README.org::*Function to apply frame fonts][Function to apply frame fonts:1]]
(defun my/apply-fonts-to-frame (&optional frame)
  "Apply font settings to FRAME (or selected frame if called interactively)."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (when (display-graphic-p frame)
        (set-frame-font (format "%s-%d" my/font-default my/font-height) nil t)
        (set-face-attribute 'default frame :family my/font-default :height (* 10 my/font-height))
        (set-face-attribute 'fixed-pitch frame :family my/font-default :height (* 10 my/font-height))
        (set-face-attribute 'variable-pitch frame :family my/font-default :height (* 10 (1+ my/font-height)))
        (set-face-attribute 'font-lock-string-face  frame :family my/font-funny :height (* 10 my/font-height))
        (set-face-attribute 'font-lock-comment-face frame :family my/font-funny :height (* 10 my/font-height))
        (set-face-attribute 'font-lock-keyword-face frame :family my/font-funny :height (* 10 my/font-height))

        (when (fboundp 'catppuccin-get-color)
          (set-face-attribute 'font-lock-variable-name-face frame
                              :foreground (catppuccin-get-color 'flamingo)
                              :height (* 10 my/font-height)
                              :bold nil
                              :family my/font-default))

        (setq-local line-spacing 0.2)
        (when (fboundp 'set-fontset-font)
          (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))))))
;; Function to apply frame fonts:1 ends here

;; [[file:README.org::*Mode line and header line faces][Mode line and header line faces:1]]
(defun my/setup-modeline ()
  (interactive)
  (let* ((bg (or (face-attribute 'org-block :background nil 'default)
                 (face-attribute 'mode-line :background nil 'default)))
         (bg-inactive (face-attribute 'mode-line-inactive :background nil 'default)))
    (set-face-attribute 'mode-line nil
                        :background bg
                        :box `(:line-width ,my/ui-box-padding :color ,bg))
    (when (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil
                          :background bg
                          :box `(:line-width ,my/ui-box-padding :color ,bg)))
    (set-face-attribute 'mode-line-inactive nil
                        :box `(:line-width ,my/ui-box-padding :color ,bg-inactive))
    (set-face-attribute 'header-line nil :inherit 'mode-line)
    (set-face-attribute 'header-line-inactive nil :inherit 'mode-line-inactive)))
;; Mode line and header line faces:1 ends here

;; [[file:README.org::*Mode line and header line faces][Mode line and header line faces:2]]
(defun my/setup-modeline-once ()
  "Setup modeline once on first focus"
  (my/setup-modeline)
  (remove-hook 'focus-in-hook #'my/setup-modeline-once))
;; Mode line and header line faces:2 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package font-core
  :ensure nil
  :config
  (add-hook 'emacs-startup-hook #'my/setup-modeline)
  (add-hook 'after-load-theme-hook #'my/setup-modeline)

  (if (daemonp)
      ;; In daemon mode wait for first frame focus
      (add-hook 'focus-in-hook #'my/setup-modeline-once)
    (add-hook 'window-setup-hook #'my/setup-modeline))

  (add-hook 'after-make-frame-functions #'my/apply-fonts-to-frame)
  (my/apply-fonts-to-frame (selected-frame)))
;; Core package:1 ends here

;; [[file:README.org::*Macos title bar][Macos title bar:1]]
(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :config
  (ns-auto-titlebar-mode))
;; Macos title bar:1 ends here

;; [[file:README.org::*Hook for theme loading][Hook for theme loading:1]]
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(advice-add 'load-theme :after
            (lambda (&rest _)
              (run-hooks 'after-load-theme-hook)))
;; Hook for theme loading:1 ends here

;; [[file:README.org::*Doom themes][Doom themes:1]]
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t  
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (setq custom-theme-directory my/doom-theme-directory)
  (doom-themes-org-config))
;; Doom themes:1 ends here

;; [[file:README.org::*Dynamic catppuccin flavor][Dynamic catppuccin flavor:1]]
(defun my/set-catppuccin-theme ()
  "Set Catppuccin theme based on dark mode state."
  (interactive)
  (when (boundp 'auto-dark--last-dark-mode-state)
    (setq catppuccin-flavor
          (if (eq auto-dark--last-dark-mode-state 'dark)
              'macchiato
            'latte))
    (when (featurep 'catppuccin-reload)
      (catppuccin-reload))
    (run-hooks 'after-load-theme-hook)))
;; Dynamic catppuccin flavor:1 ends here

;; [[file:README.org::*Auto dark mode][Auto dark mode:1]]
(use-package auto-dark
  :after doom-themes
  :hook
  (emacs-startup . auto-dark-mode)
  (auto-dark-dark-mode . my/set-catppuccin-theme)
  (auto-dark-light-mode . my/set-catppuccin-theme)
  :custom
  (auto-dark-dark-theme 'doom-outrun-electric)
  ;; (auto-dark-light-theme 'doom-one-light)
  ;; (auto-dark-dark-theme 'catppuccin)
  ;; (auto-dark-light-theme 'catppuccin)
  (auto-dark-themes '((doom-tokyo-night)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil))
;; Auto dark mode:1 ends here

;; [[file:README.org::*Fringes][Fringes:1]]
(use-package spacious-padding
  :hook (emacs-startup . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 20 :header-line-width 0 :mode-line-width 0
                            :tab-width 0 :right-divider-width 1
                            :scroll-bar-width 1 :fringe-width 8)))
;; Fringes:1 ends here

;; [[file:README.org::*Auto zoom windows][Auto zoom windows:1]]
(use-package zoom
  :bind (("C-c z m" . zoom-mode))
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-buffer-name-regexps '("^\\*calc" "^\\*vterm" "^\\*combobulate-query-builder" "^\\*dape" "^\\*claude" "^\\*Ediff" "<eca-chat"))
  (zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode claude-code-prompt-mode ediff-mode eca-mode eca-chat-mode))
  (zoom-ignore-predicates '((lambda () (window-parameter nil 'window-side))))
  :config
  (defun my/fix-ediff-size ()
    (let* ((buf (or (and (boundp 'ediff-control-buffer) ediff-control-buffer)
                    (car (seq-filter
                          (lambda (b)
                            (string-match-p "^\\*Ediff Control Panel" (buffer-name b)))
                          (buffer-list)))))
           (win (and buf (get-buffer-window buf t))))
      (when (window-live-p win)
        (with-selected-window win
          (setq window-size-fixed t)
          (window-resize (selected-window)
                         (- 5 (window-total-height))
                         nil t)))))

  (add-hook 'ediff-after-setup-windows-hook 'my/fix-ediff-size))
;; Auto zoom windows:1 ends here

;; [[file:README.org::*Function to setup tabar faces][Function to setup tabar faces:1]]
(defun my/setup-tab-bar-faces ()
    (let ((bg       (or (face-background 'mode-line nil t) "gray50"))
          (bg-inact (or (face-background 'tab-bar-tab-inactive nil t)
                        (face-background 'mode-line-inactive nil t)
                        "gray40")))
      (custom-set-faces
       `(tab-bar-echo-area-tab
         ((t (:inherit mode-line-active
                       :box (:line-width 6 :color ,bg)))))
       `(tab-bar-echo-area-tab-inactive
         ((t (:inherit tab-bar-tab-inactive
                       :box (:line-width 6 :color ,bg-inact))))))))
;; Function to setup tabar faces:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package tab-bar-echo-area
  :hook 
  (emacs-startup . tab-bar-echo-area-mode)
  (after-load-theme-hook . my/setup-tab-bar-faces)
  :bind (("C-c TAB TAB" . tab-bar-echo-area-display-tab-names))
  :custom
  (tab-bar-echo-area-display-tab-names-format-string " %s ")
  :config
  (my/setup-tab-bar-faces))
;; Core package:1 ends here

;; [[file:README.org::*Tab bar manager][Tab bar manager:1]]
(use-package tab-bar-lost-commands :defer t)
;; Tab bar manager:1 ends here

;; [[file:README.org::*Rotate windows][Rotate windows:1]]
(use-package rotate
  :bind (("C-c w R" . rotate-layout)
         ("C-c w r" . rotate-window)))
;; Rotate windows:1 ends here

;; [[file:README.org::*Folding, outline][Folding, outline:1]]
(use-package outline-indent
  :hook 
  ((dart-mode
    yaml-mode
    web-mode
    yaml-ts-mode
    python-mode
    html-mode
    scss-mode
    ng2-html-mode
    emacs-lisp-mode
    json-mode) . outline-indent-minor-mode)
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼"))
;; Folding, outline:1 ends here

;; [[file:README.org::*Hide modeline mode][Hide modeline mode:1]]
(use-package hide-mode-line
  :hook
  (fundamental-mode . hide-mode-line-mode)
  (compilation-mode . hide-mode-line-mode)
  (magit-mode . hide-mode-line-mode)
  (prog-mode . hide-mode-line-mode)
  (text-mode . hide-mode-line-mode)
  (org-mode . hide-mode-line-mode))
;; Hide modeline mode:1 ends here

;; [[file:README.org::*Icons][Icons:1]]
(use-package nerd-icons :defer t)
;; Icons:1 ends here

;; [[file:README.org::*Better emacs docs. Helpful][Better emacs docs. Helpful:1]]
(use-package helpful
  :bind (("C-h k" . helpful-key)
         ("C-h C-k" . helpful-key)
         ("C-h p" . describe-package)
         ("C-h C-p" . describe-package)
         ("C-h v" . helpful-variable)
         ("C-h C-v" . helpful-variable)
         ("C-h f" . helpful-function)
         ("C-h C-f" . helpful-function)
         ("C-h m" . describe-mode)
         ("C-h C-m" . describe-mode)
         ("C-h ." . helpful-at-point)
         ("C-h F" . describe-face))
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*Help\\*"
     (my/smart-display-buffer-function)))
  (add-to-list
   'display-buffer-alist
   '("\\*helpful"
     (my/smart-display-buffer-function))))
;; Better emacs docs. Helpful:1 ends here

;; [[file:README.org::*Alias for yes/no][Alias for yes/no:1]]
(ignore-errors
  (setq confirm-kill-emacs 'y-or-n-p)
  (defalias 'yes-or-no-p 'y-or-n-p))
;; Alias for yes/no:1 ends here

;; [[file:README.org::*Disable menu bar mode and other stuff][Disable menu bar mode and other stuff:1]]
(set-frame-parameter nil 'alpha-background 0.1)
(add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . t))
(add-to-list 'frameset-filter-alist '(ns-appearance . dark))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq scroll-step 1)
(setq scroll-margin 1)
;; Disable menu bar mode and other stuff:1 ends here

;; [[file:README.org::*Disable menu bar mode and other stuff][Disable menu bar mode and other stuff:2]]
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 0)
(window-divider-mode +1)
;; Disable menu bar mode and other stuff:2 ends here

;; [[file:README.org::*Disable menu bar mode and other stuff][Disable menu bar mode and other stuff:3]]
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
;; Disable menu bar mode and other stuff:3 ends here

;; [[file:README.org::*Truncate lines][Truncate lines:1]]
(set-default 'truncate-lines t)
;; Truncate lines:1 ends here

;; [[file:README.org::*Disable default splash screen][Disable default splash screen:1]]
(setq
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-buffer-menu t)
;; Disable default splash screen:1 ends here

;; [[file:README.org::*Full size from startup][Full size from startup:1]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Full size from startup:1 ends here

;; [[file:README.org::*Reset themes before apply][Reset themes before apply:1]]
(advice-add 'load-theme :before
            (lambda (&rest _)
              (mapc #'disable-theme custom-enabled-themes)))
;; Reset themes before apply:1 ends here

;; [[file:README.org::*No distraction package][No distraction package:1]]
(use-package no-distraction
  :hook ((typescript-ts-mode html-ts-mode) . no-distraction-mode)
  :ensure (no-distraction :host github :repo "artawower/no-distraction.el"))
;; No distraction package:1 ends here

;; [[file:README.org::*No resize when completion][No resize when completion:1]]
(use-package stillness-mode
  :defer 2
  :ensure (:host github :repo "neeasade/stillness-mode.el" :branch "main"))
;; No resize when completion:1 ends here

;; [[file:README.org::*Show paren mode][Show paren mode:1]]
(use-package paren
  :ensure nil
  :hook
  (prog-mode . show-paren-mode)
  (org-mode . show-paren-mode))
;; Show paren mode:1 ends here

;; [[file:README.org::*Paren face][Paren face:1]]
(use-package paren-face
  :defer t 
  :hook (prog-mode . paren-face-mode)
  :config
  (setq paren-face-regexp (rx (any ?\( ?\) ?\[ ?\] ?\{ ?\}))))
;; Paren face:1 ends here

;; [[file:README.org::*Buffer positionin][Buffer positionin:1]]
(defun my/smart-display-buffer-function (buffer alist)
  "Smart display function that:
   - Reuses existing buffer window if buffer is already visible
   - Uses only two windows maximum
   - If in left window, opens buffer in right window
   - If in right window, opens buffer in left window
   - If only one window exists, splits it horizontally"
  (let ((buffer-window (get-buffer-window buffer)))
    (cond
     ;; If buffer is already visible, just switch to it
     (buffer-window
      (select-window buffer-window)
      buffer-window)
     
     ;; If we have exactly 2 windows
     ((= (length (window-list)) 2)
      (let* ((current-window (selected-window))
             (other-window (next-window current-window 'no-minibuf))
             (target-window (if (eq current-window (window-at 0 0))
                                other-window  ; Current is left window, use right window
                              other-window))) ; Current is right window, use left window
        (set-window-buffer target-window buffer)
        (select-window target-window)
        target-window))
     
     ;; If only one window exists, split it horizontally
     (t
      (let ((new-window (split-window-horizontally)))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        new-window)))))
;; Buffer positionin:1 ends here

;; [[file:README.org::*Highlight keywords][Highlight keywords:1]]
(use-package hl-todo
  :ensure (hl-todo :depth nil)
  :hook 
  (prog-mode . hl-todo-mode)
  (test-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#E5C07B")
     ("FIXME"  . "#E06C75")
     ("DEBUG"  . "#C678DD")
     ("REFACTOR"  . "#C678DD")
     ("GOTCHA" . "#FF4500")
     ("NOTE"   . "#98C379")
     ("QUESTION"   . "#98C379")
     ("STUB"   . "#61AFEF")))
  :config
  (global-hl-todo-mode 1))
;; Highlight keywords:1 ends here

;; [[file:README.org::*Highlight colors. Rainbow extension][Highlight colors. Rainbow extension:1]]
(use-package colorful-mode
  :ensure (:type git :host github :repo "DevelopmentCool2449/colorful-mode")
  :hook ((html-mode css-mode scss-mode emacs-lisp-mode org-mode help-mode helpful-mode) . colorful-mode))
;; Highlight colors. Rainbow extension:1 ends here

;; [[file:README.org::*Topsy html][Topsy html:1]]
(defun my/treesit-html-breadcrumbs ()
  "Return a string of breadcrumbs."
  (let ((node (treesit-node-at (point) 'html))
        result)
    (while node
      (when (string= "element" (treesit-node-type node))
        (push (treesit-node-text (treesit-node-child
                                  (treesit-node-child node 0) 1))
              result))
      (setq node (treesit-node-parent node)))
    (mapconcat #'identity result " > ")))
;; Topsy html:1 ends here

;; [[file:README.org::*Topsy for json][Topsy for json:1]]
(defun my/treesit-json-breadcrumbs ()
  "Return a string of breadcrumbs for json."
  (let ((node (treesit-node-parent (treesit-node-at (point) 'json)))
        result
        next-child-type)
    (while node
      (setq next-child-type (treesit-node-type (treesit-node-child node 2)))
      (when (and (string= "pair" (treesit-node-type node))
                 (or (string= "object" next-child-type)
                     (string= "array" next-child-type)))
        (push (treesit-node-text (treesit-node-child (treesit-node-child node 0) 1))
              result))
      (setq node (treesit-node-parent node)))
    (mapconcat #'identity result " > ")))
;; Topsy for json:1 ends here

;; [[file:README.org::*Topsy for scss][Topsy for scss:1]]
(defun my/scss-breadcrumbs ()
  "Return a string of breadcrumbs for SCSS mode, showing only nested class selectors."
  (save-excursion
    (let ((current-point (point))
          (classes '())
          (current-indent nil)
          (in-selector nil)
          (line ""))
      
      ;; First check if we're inside a selector or a property
      (beginning-of-line)
      (setq line (buffer-substring-no-properties 
                  (line-beginning-position) 
                  (line-end-position)))
      
      ;; If the current line has a '{', we're likely in a selector
      (setq in-selector (and (string-match-p "{" line)
                             (not (string-match-p "}" line))))
      
      ;; If we're in a property (not in a selector), move to parent selector
      (when (not in-selector)
        (while (and (> (point) (point-min))
                    (not (looking-at-p ".*{[ \t]*$")))
          (forward-line -1)))
      
      ;; Get the current line's indentation
      (setq current-indent (current-indentation))
      
      ;; Start collecting selectors moving backward
      (while (> (point) (point-min))
        ;; Get the current line
        (setq line (buffer-substring-no-properties 
                    (line-beginning-position) 
                    (line-end-position)))
        
        ;; Check if this line is a selector (ends with '{')
        (when (string-match-p ".*{[ \t]*$" line)
          ;; Check if indent is less than or equal to our current tracking indent
          (when (or (null current-indent) (<= (current-indentation) current-indent))
            ;; Update our current tracking indent
            (setq current-indent (current-indentation))
            
            ;; Extract only class selectors from the line
            (let ((class-selectors '()))
              ;; Match class selectors ('.something')
              (with-temp-buffer
                (insert line)
                (goto-char (point-min))
                (while (re-search-forward "\\.[a-zA-Z0-9_-]+" nil t)
                  (let ((class (match-string 0)))
                    ;; Remove anything after a space, comma, pseudo-class, etc.
                    (when (string-match "^\\.[a-zA-Z0-9_-]+" class)
                      (push (match-string 0 class) class-selectors)))))
              
              ;; Add to our breadcrumbs
              (when class-selectors
                (push (mapconcat #'identity (reverse class-selectors) ", ") classes)))))
        
        ;; Move to previous line
        (forward-line -1))
      
      ;; Combine the breadcrumbs
      (if classes
          (mapconcat #'identity (reverse classes) " > ")
        ""))))
;; Topsy for scss:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package topsy
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode)
  :config
  (add-to-list 'topsy-mode-functions '(scss-mode . my/scss-breadcrumbs))
  (add-to-list 'topsy-mode-functions '(html-ts-mode . my/treesit-html-breadcrumbs))
  (add-to-list 'topsy-mode-functions '(json-ts-mode . my/treesit-json-breadcrumbs)))
;; Core package:1 ends here

;; [[file:README.org::*Imenu][Imenu:1]]
(use-package imenu :defer t :ensure nil)
;; Imenu:1 ends here

;; [[file:README.org::*Edit after/before eol][Edit after/before eol:1]]
(defun my/edit-after-eol ()
  "Edit the current line to the end of the line like A in the VIM."
  (interactive)
  (meow-line 1)
  (meow-append))

(defun my/edit-before-bol ()
  "Edit the current line to the beginning of the line like I in the VIM."
  (interactive)
  (meow-join 0)
  (meow-append))
;; Edit after/before eol:1 ends here

;; [[file:README.org::*Yank below][Yank below:1]]
(defun my/meow-yank-below ()
  "Yank below the current line."
  (interactive)
  (forward-line)
  (meow-yank))
;; Yank below:1 ends here

;; [[file:README.org::*Meow change till EOL][Meow change till EOL:1]]
(defun my/meow-change-till-eol ()
  "Change till the end of line."
  (interactive)
  (let ((meow-eol-thing 108))
    (meow-end-of-thing meow-eol-thing)
    (meow-change)))
;; Meow change till EOL:1 ends here

;; [[file:README.org::*Meow select till EOL][Meow select till EOL:1]]
(defun my/meow-select-till-eol ()
  "Select till the end of line."
  (interactive)
  (let ((meow-eol-thing 108))
    (meow-end-of-thing meow-eol-thing)))
;; Meow select till EOL:1 ends here

;; [[file:README.org::*Meow back till][Meow back till:1]]
(defun my/meow-backward-till (n ch)
  "Move backward till the first character that is not in the list of characters."
  (interactive "p\ncTill:")
  (meow-till -1 ch))
;; Meow back till:1 ends here

;; [[file:README.org::*KILL Navigation][KILL Navigation:1]]
(require 'cl-lib)
(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (cl-remove-if-not (apply-partially #'string-match-p regex)
                                           (mapcar #'buffer-name (buffer-list))))))
;; KILL Navigation:1 ends here

;; [[file:README.org::*Meow Keybindings][Meow Keybindings:1]]
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; C-c C-j C-/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use C-c C-`C-0-9 for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("?" . meow-keypad-describe-key))
  (meow-normal-define-key
   '("*" . (lambda () (interactive)
             (call-interactively 'meow-mark-symbol)
             (call-interactively 'meow-search)))
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("s-o" . meow-last-buffer)
   '("ge" . end-of-buffer)
   '("gg" . beginning-of-buffer)
   '("6" . meow-expand-6)
   '("@" . meow-end-or-call-kmacro)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("$" . my/meow-select-till-eol)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("s-d" . meow-inner-of-thing)
   '("s-D" . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("i" . meow-append)
   '("o" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("F" . meow-find)
   '("f" . avy-goto-word-1)
   '("q" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("a" . meow-insert)
   '("O" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("/" . meow-block)
   '("?" . meow-to-block)
   '("P" . meow-yank)
   '("C" . my/meow-change-till-eol)
   '("p" . my/meow-yank-below)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . my/meow-backward-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-visual-line)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("Q" . meow-pop-selection)
   '("'" . repeat)
   '("=" . indent-for-tab-command)
   '(":" . query-replace-regexp)
   '("A" . my/edit-after-eol)
   '("I" . my/edit-before-bol)
   '("<escape>" . ignore))

  (meow-define-keys 'motion
    '("\\n" . meow-normal-mode)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '("\\q" . kill-current-buffer))

  (meow-define-keys 'insert
   '("s-o" . meow-last-buffer)
   '("s-p" . xah-paste-from-register-1))


  (meow-define-keys 'normal
    '("\\q" . kill-current-buffer)
    '("z z" . recenter)
    '("C-<tab>" . indent-rigidly-right)
    '("<backtab>" . indent-rigidly-left)
    '("g c" . comment-or-uncomment-region)))
;; Meow Keybindings:1 ends here

;; [[file:README.org::*Meow thing register for better selection][Meow thing register for better selection:1]]
(defun my/meow-thing-register ()
  (meow-thing-register 'whitespace '(regexp " \\|\n" " \\|\n") '(regexp " \\|\n" " \\|\n"))
  (add-to-list 'meow-char-thing-table '(?w . whitespace))

  (meow-thing-register 'non-whitespace
                         '(syntax . "-")
                         '(syntax . "-"))
  (add-to-list 'meow-char-thing-table '(?e . non-whitespace))

  (add-to-list 'meow-char-thing-table '(?\" . quoted))
  (add-to-list 'meow-char-thing-table '(?' . quoted))
  (add-to-list 'meow-char-thing-table '(?< . angle))

  (add-to-list 'meow-char-thing-table '(?\( . round))
  (add-to-list 'meow-char-thing-table '(?\) . round))

  (add-to-list 'meow-char-thing-table '(?{ . curly))
  (add-to-list 'meow-char-thing-table '(?} . curly))
  
  (add-to-list 'meow-char-thing-table '(?\[ . square))
  (add-to-list 'meow-char-thing-table '(?\] . square))

  (meow-thing-register 'quoted
                       '(regexp "\"\\|'\\|`" "\"\\|'\\|`")
                       '(regexp "\"\\|'\\|`" "\"\\|'\\|`"))

  (meow-thing-register 'angle
                       '(regexp "<" ">")
                       '(regexp "<" ">")))
;; Meow thing register for better selection:1 ends here

;; [[file:README.org::*Meow, custom modes][Meow, custom modes:1]]
(defun my/meow-setup-custom-modes ()
  (setq meow-paren-keymap (make-keymap))
  (meow-define-state paren
    "meow state for interacting with smartparens"
    :lighter " [P]"
    :keymap meow-paren-keymap)

  ;; meow-define-state creates the variable
  (setq meow-cursor-type-paren 'hollow)

  (meow-define-keys 'paren
    '("<escape>" . meow-normal-mode)
    '("l" . sp-forward-sexp)
    '("h" . sp-backward-sexp)
    '("j" . sp-down-sexp)
    '("k" . sp-up-sexp)
    '("n" . sp-forward-slurp-sexp)
    '("b" . sp-forward-barf-sexp)
    '("v" . sp-backward-barf-sexp)
    '("c" . sp-backward-slurp-sexp)
    '("u" . meow-undo)))
;; Meow, custom modes:1 ends here

;; [[file:README.org::*Meow, pair modes][Meow, pair modes:1]]
(defun my/meow-setup-state-per-modes ()
  (add-to-list 'meow-mode-state-list '(elpaca-info-mode . normal))
  (add-to-list 'meow-mode-state-list '(flycheck-error-list-mode . normal))
  (add-to-list 'meow-mode-state-list '(magit-process-mode . normal))
  (add-to-list 'meow-mode-state-list '(compilation-mode . normal))
  (add-to-list 'meow-mode-state-list '(helpful-mode . normal))
  (add-to-list 'meow-mode-state-list '(help-mode . normal))
  (add-to-list 'meow-mode-state-list '(detached-compilation-mode-map . normal))
  (add-to-list 'meow-mode-state-list '(messages-buffer-mode . normal))
  (add-to-list 'meow-mode-state-list '(debug-mode . normal))
  (add-to-list 'meow-mode-state-list '(debugger-mode . normal))
  (add-to-list 'meow-mode-state-list '(grep-mode . normal)))
;; Meow, pair modes:1 ends here

;; [[file:README.org::*Meow, core package][Meow, core package:1]]
(use-package meow
  :custom
  (meow-use-clipboard t)
  :config
  (meow-setup)
  (set-face-attribute 'meow-position-highlight-number-1 nil :foreground "#61AFEF" :background "#3E4451" :bold t :underline nil)
  (set-face-attribute 'meow-position-highlight-number-2 nil :foreground "#4E90CF" :bold t :underline nil)
  (define-key mode-specific-map (kbd "j") nil)
  (my/meow-thing-register)
  (my/meow-setup-custom-modes)
  (my/meow-setup-state-per-modes)
  (advice-add #'meow-change :after 
              (lambda ()
                (when (string-match-p "^\\s-*$" (thing-at-point 'line t))
                  (indent-for-tab-command))))
  (meow-global-mode 1))
;; Meow, core package:1 ends here

;; [[file:README.org::*Meow. Treesit integration][Meow. Treesit integration:1]]
(use-package meow-tree-sitter
  :ensure (:host github :repo "skissue/meow-tree-sitter")
  :after meow
  :config
  (meow-tree-sitter-register-defaults))
;; Meow. Treesit integration:1 ends here

;; [[file:README.org::*Meow vterm][Meow vterm:1]]
(use-package meow-vterm
  :ensure (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :after vterm
  :config
  (setq vterm-keymap-exceptions '("C-c"))
  (meow-vterm-enable))
;; Meow vterm:1 ends here

;; [[file:README.org::*macOS modifiers][macOS modifiers:1]]
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq ns-function-modifier 'hyper))
;; macOS modifiers:1 ends here

;; [[file:README.org::*Standard editing][Standard editing:1]]
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-c") 'meow-save)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key "\C-u" 'backward-kill-line)
;; Standard editing:1 ends here

;; [[file:README.org::*Window management][Window management:1]]
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "C-c w s") 'split-window-vertically)
(global-set-key (kbd "C-c w v") 'split-window-horizontally)
(global-set-key (kbd "s-w") 'delete-window)
;; Window management:1 ends here

;; [[file:README.org::*File operations][File operations:1]]
(global-set-key (kbd "C-c .") 'find-file)
(global-set-key (kbd "C-c f D") 'my/delete-this-file)
(global-set-key (kbd "C-c o F") 'ns-open-file-using-panel)
;; File operations:1 ends here

;; [[file:README.org::*Buffer operations][Buffer operations:1]]
(global-set-key (kbd "C-c o l") 'my/open-messages)
(global-set-key (kbd "C-c o M") 'my/open-clear-messages)
(global-set-key (kbd "s-n") (lambda () (interactive) (switch-to-buffer (generate-new-buffer "Untitled"))))
(global-set-key (kbd "C-c q") 'kill-current-buffer)
;; Buffer operations:1 ends here

;; [[file:README.org::*Package manager][Package manager:1]]
(global-set-key (kbd "C-c p m") 'elpaca-manager)
(global-set-key (kbd "C-c p M") 'elpaca-log)
;; Package manager:1 ends here

;; [[file:README.org::*Theme][Theme:1]]
(global-set-key (kbd "C-h C-t") 'load-theme)
;; Theme:1 ends here

;; [[file:README.org::*Code annotations][Code annotations:1]]
(global-set-key (kbd "C-c t t") 'my/insert-todo-by-current-git-branch)
(global-set-key (kbd "C-c t i") 'my/insert-todo-by-current-git-branch)
(global-set-key (kbd "C-c t d") 'my/insert-debug-by-current-git-branch)
(global-set-key (kbd "C-c t n") 'my/insert-note-by-current-git-branch)
;; Code annotations:1 ends here

;; [[file:README.org::*Narrowing][Narrowing:1]]
(global-set-key (kbd "C-c +") 'narrow-to-region)
(global-set-key (kbd "C-c -") 'widen)
;; Narrowing:1 ends here

;; [[file:README.org::*Minibuffer navigation][Minibuffer navigation:1]]
(define-key minibuffer-local-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-k") 'previous-history-element)
(define-key read--expression-map (kbd "C-j") 'next-history-element)
(define-key read--expression-map (kbd "C-k") 'previous-history-element)
;; Minibuffer navigation:1 ends here

;; [[file:README.org::*Override meow keypad parsing][Override meow keypad parsing:1]]
(defun my/meow--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (setq key `(,(car key) . ,(concat (mapcar (lambda (c) (reverse-im--translate-char c t)) (cdr key)))))
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (meow--keypad-format-upcase (cdr key))))
    (both (format "C-M-%s" (meow--keypad-format-upcase (cdr key))))
    (literal (cdr key))))
;; Override meow keypad parsing:1 ends here

;; [[file:README.org::*Dependency][Dependency:1]]
(use-package char-fold
  :ensure nil
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))
;; Dependency:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package reverse-im
  :demand t 
  :after char-fold 
  :custom
  (reverse-im-char-fold t) ; use lax matching
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("russian-computer")) ; translate these methods
  :config
  (reverse-im-mode t)
  (advice-add 'meow--keypad-format-key-1 :override #'my/meow--keypad-format-key-1))
;; Core package:1 ends here

;; [[file:README.org::*Avy, quick jump to words][Avy, quick jump to words:1]]
(use-package avy
  :defer t
  :bind (("s-r" . (lambda () (interactive) (set-mark-command nil) (call-interactively 'avy-goto-char) (right-char 1)))
         ("C-c d l" . avy-kill-whole-line)
         ("M-l" . avy-goto-line)
         :map meow-normal-state-keymap
         ("f" . avy-goto-word-1)
         ("\\f" . avy-goto-char-timer))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))
;; Avy, quick jump to words:1 ends here

;; [[file:README.org::*Ace window. Fast jump between opened windows and frames][Ace window. Fast jump between opened windows and frames:1]]
(use-package ace-window
  :defer t
  :bind (("s-." . ace-window))
  :config
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))
;; Ace window. Fast jump between opened windows and frames:1 ends here

;; [[file:README.org::*Functions][Functions:1]]
(defun my/better-jump-preserve-pos-advice (oldfun &rest args)
  "Preserve position when jumping."
  (let ((old-pos (point)))
    (apply oldfun args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
             1)
      (better-jumper-set-jump old-pos))))
;; Functions:1 ends here

;; [[file:README.org::*Functions][Functions:2]]
(defun my/better-jump-save-prog-mode-pos (&rest args)
  "Function for preserve better jump befor buffer changed only for prog mode"
  (when (or (derived-mode-p 'prog-mode)
            (eq major-mode 'html-ts-mode)
            (eq major-mode 'html-mode))
    (call-interactively #'better-jumper-set-jump)))
;; Functions:2 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package better-jumper
  :defer 2
  :bind
  (("C-o" . better-jumper-jump-backward)
   ("C-i" . better-jumper-jump-forward)
   ("C-c 1" . better-jumper-set-jump))
  :custom
  (better-jumper-use-evil-jump-advice nil)
  :config
  (advice-add 'meow-end-of-thing :around #'my/better-jump-preserve-pos-advice)
  (advice-add 'avy-goto-word-1 :around #'my/better-jump-preserve-pos-advice)
  ;; TODO: move to husky package
  (advice-add 'husky-actions-find-definition :before (lambda () (call-interactively #'better-jumper-set-jump)))
  (advice-add 'husky-lsp-find-definition :before (lambda () (call-interactively #'better-jumper-set-jump)))
  (advice-add 'husky-lsp-find-definition :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'find-file :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'project-find-file :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'consult-buffer :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'lsp-find-references :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'flycheck-next-error :before #'my/better-jump-save-prog-mode-pos)
  (advice-add 'flycheck-previous-error :before #'my/better-jump-save-prog-mode-pos)
  (better-jumper-mode 1))
;; Core package:1 ends here

;; [[file:README.org::*Bookmarks][Bookmarks:1]]
(use-package bm
  :defer t
  :custom-face
  (bm-face ((t (:inherit font-lock-builtin-face :background unspecified))))
  :custom
  (bm-in-lifo-order t)
  :bind (("M-s-n" . bm-next)
         ("M-s-p" . bm-previous)
         ("s-b" . bm-toggle)))
;; Bookmarks:1 ends here

;; [[file:README.org::*Scroll golden ratio][Scroll golden ratio:1]]
(use-package golden-ratio-scroll-screen
  :custom-face
  (golden-ratio-scroll-highlight-line-face ((t (:background "#ffb6c1" :extend t))))
  :custom
  (golden-ratio-scroll-highlight-delay '(0.1 . 0.2))
  :config
  (define-key global-map (kbd "C-v") #'golden-ratio-scroll-screen-up)
  (define-key global-map (kbd "M-v") #'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))
;; Scroll golden ratio:1 ends here

;; [[file:README.org::*Ultrascroll, smooth scrolling][Ultrascroll, smooth scrolling:1]]
(use-package ultra-scroll
  :hook ((after-init . ultra-scroll-mode))
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))
;; Ultrascroll, smooth scrolling:1 ends here

;; [[file:README.org::*Global configs][Global configs:1]]
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; Global configs:1 ends here

;; [[file:README.org::*Delete backward][Delete backward:1]]
(defun my/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
;; Delete backward:1 ends here

;; [[file:README.org::*Copy with AI context][Copy with AI context:1]]
(defun my/copy-with-ai-context (beg end)
  "Copy selected region with context for AI tools.

The format is:
<project-related-path>, line numbers <from>:<to>
```<lang>
<code>
```"
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((rb (min beg end))
         (re (max beg end))
         (from (line-number-at-pos rb))
         (to (line-number-at-pos
              (if (and (> re rb)
                       (eq (char-before re) ?\n))
                  (1- re)
                re)))
         (code (buffer-substring-no-properties rb re))
         ;; Determine language from file extension or major mode
         (ext  (and buffer-file-name (downcase (or (file-name-extension buffer-file-name) ""))))
         (mode (downcase (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))))
         (lang (cond
                ((and ext (not (string-empty-p ext))) ext)
                ((and mode (not (string-empty-p mode))) mode)
                (t "text")))
         ;; Get project-related path using file-info--get-project-related-path if available
         (path (if (fboundp 'file-info--get-project-related-path)
                   (or (file-info--get-project-related-path)
                       (or buffer-file-name (buffer-name)))
                 (or buffer-file-name (buffer-name))))
         ;; Format the final string
         (payload (format "%s, line numbers %d:%d\n```%s\n%s\n```"
                          path from to lang code)))
    ;; Copy to kill-ring (and clipboard if enabled)
    (kill-new payload)
    (message "Copied %d chars: %s, lines %d:%d"
             (length payload) path from to)))
;; Copy with AI context:1 ends here

;; [[file:README.org::*Autorevert mode][Autorevert mode:1]]
(use-package autorevert
    :ensure nil
    :hook ((after-init . global-auto-revert-mode))
    :custom
    (auto-revert-verbose nil)
    (auto-revert-use-notify nil))
;; Autorevert mode:1 ends here

;; [[file:README.org::*Persistent values][Persistent values:1]]
(use-package persistent-values
  :defer t
  :bind (("s-S" . persistent-values-add)
         ("s-I" . persistent-values-search))
  :ensure (persistent-values :host github :repo "artawower/persistent-values.el")
  :config
  (persistent-values-mode 1))
;; Persistent values:1 ends here

;; [[file:README.org::*Expand region][Expand region:1]]
(use-package expand-region
  :bind (("s-x" . er/expand-region))
  :defer t)
;; Expand region:1 ends here

;; [[file:README.org::*Narrowing][Narrowing:1]]
(use-package narrow-indirect
  :ensure (:host github :repo "emacsmirror/narrow-indirect")
  :bind (("C-c 0" . ni-narrow-to-region-indirect-other-window))
  :defer t)
;; Narrowing:1 ends here

;; [[file:README.org::*Undo mode][Undo mode:1]]
(use-package undo-fu
  :defer t
  :bind (("C-r" . undo-fu-only-redo)
         :map meow-normal-state-keymap
              ("u" . undo-fu-only-undo)))
;; Undo mode:1 ends here

;; [[file:README.org::*Undo sessions (history saving)][Undo sessions (history saving):1]]
(use-package undo-fu-session
  :after undo-fu
  :hook (after-init . undo-fu-session-mode))
;; Undo sessions (history saving):1 ends here

;; [[file:README.org::*Autopairs][Autopairs:1]]
(use-package electric
  :ensure nil
  :hook
  ((html-mode . sgml-electric-tag-pair-mode)
   (ng2-html-mode . sgml-electric-tag-pair-mode)
   (prog-mode . electric-pair-mode))
  :config
  (when (boundp 'electric-pair-pairs)
    (setq electric-pair-pairs
          (append electric-pair-pairs '((?< . ?>)))))
  
  (setq electric-pair-preserve-balance nil
        electric-pair-delete-adjacent-pairs t
        electric-pair-skip-whitespace nil
        electric-pair-open-newline-between-pairs t))
;; Autopairs:1 ends here

;; [[file:README.org::*Surround 2.0][Surround 2.0:1]]
(use-package surround
  :ensure t
  :defer t
  :bind-keymap ("M-'" . surround-keymap))
;; Surround 2.0:1 ends here

;; [[file:README.org::*Turbo log. Quick log inserting][Turbo log. Quick log inserting:1]]
(use-package turbo-log
  :defer t
  :ensure (turbo-log :type git :host github :repo "Artawower/turbo-log.el")
  :commands (turbo-log--get-context)
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-x" . turbo-log-delete-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger)
         ("C-s-]" . turbo-log-paste-as-logger-immediately))
  :custom
  (turbo-log-payload-format-template "%s: ")
  (turbo-log-allow-insert-without-treesit-p t)
  (turbo-log-allow-insert-without-tree-sitter-p t)
  :config
  (turbo-log-configure
   :modes (typescript-ts-mode tsx-ts-mode typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (apheleia-format-buffer lsp)
   :msg-format-template "'✎: %s'"))
;; Turbo log. Quick log inserting:1 ends here

;; [[file:README.org::*Auto rename tags pairs][Auto rename tags pairs:1]]
(use-package auto-rename-tag
  :hook ((html-mode . auto-rename-tag-mode)
         (ng2-html-mode . auto-rename-tag-mode)
         (web-mode . auto-rename-tag-mode))
  :defer t)
;; Auto rename tags pairs:1 ends here

;; [[file:README.org::*Macros][Macros:1]]
(use-package persistent-soft
  :defer t)
;; Macros:1 ends here

;; [[file:README.org::*Macros][Macros:2]]
(use-package persistent-kmacro
  :ensure (:host github :repo "artawower/persistent-kmacro.el")
  :defer t
  :bind (("C-c m e" . persistent-kmacro-execute-macro)
         ("C-c m a" . persistent-kmacro-name-last-kbd-macro)
         ("C-c m r" . persistent-kmacro-remove-macro)
         :map meow-normal-state-keymap
         ("#" . persistent-kmacro-apply))
  :config
  (setq persistent-kmacro-macro-file "persistent-kmacro-store.el"))
;; Macros:2 ends here

;; [[file:README.org::*Tempel][Tempel:1]]
(use-package tempel
  :bind
  (("s-y" . tempel-complete)
   ("s-Y" . tempel-insert)
   ("s-\\" . tempel-done))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  :config
  (setq tempel-auto-reload nil)
  (setq tempel-path
        (directory-files my/templates-dir t "\\`[^.].*\\'")))
;; Tempel:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package treesit
  :ensure nil
  :hook
  (go-ts-mode . (lambda () (setq-local tab-width 2)))
  :mode 
  ("\\.js\\'" . js-ts-mode)
  ("\\.mjs\\'" . typescript-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  :custom
  (go-ts-mode-indent-offset 2)
  :config
  (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (scss "https://github.com/serenadeai/tree-sitter-scss")
          (vue "https://github.com/ikatyang/tree-sitter-vue")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))
  (setq treesit-font-lock-level 3)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; Core package:1 ends here

;; [[file:README.org::*Navigation][Navigation:1]]
(use-package combobulate
  :after treesit
  :ensure (:host github :repo "mickeynp/combobulate")
  :bind (("s-q" . combobulate-mark-node-dwim)
         ("s-<down>" . combobulate-navigate-logical-next)
         ("s-<up>" . combobulate-navigate-logical-previous)
         ("s-N" . combobulate-navigate-next)
         ("s-P" . combobulate-navigate-previous)
         :map combobulate-key-map
         ("C-M-t" . nil))
  :custom
  (combobulate-key-prefix "C-c c o"))
;; Navigation:1 ends here

;; [[file:README.org::*Treesitter folding][Treesitter folding:1]]
(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "abougouffa/treesit-fold")
  :hook ((web-mode
          ng2-html-mode
          ng2-ts-mode
          typescript-mode
          js-mode
          js-ts-mode
          python-ts-mode
          python-mode
          html-mode
          json-mode
          json-ts-mode
          go-mode
          bash-mode
          css-mode
          scss-mode
          go-ts-mode
          typescript-ts-mode) . treesit-fold-mode)
  :config
  (add-to-list 'treesit-fold-range-alist '(ng2-ts-mode . ((export_clause . ts-fold-range-seq)
                                                          (statement_block . treesit-fold-range-seq)
                                                          (comment . treesit-fold-range-c-like-comment))) t)

  (add-to-list 'treesit-fold-range-alist '(web-mode . (html-mode
                                                       (element . treesit-fold-range-html)
                                                       (comment treesit-fold-range-seq 1 -1))))

  (add-to-list 'treesit-fold-range-alist '(ng2-html-mode . (html-mode
                                                            (element . treesit-fold-range-html)
                                                            (comment treesit-fold-range-seq 1 -1))))
  (add-to-list 'treesit-fold-range-alist '(scss-mode . ((keyframe_block_list . ts-fold-range-seq)
                                                        (block . treesit-fold-range-seq)
                                                        (comment . treesit-fold-range-c-like-comment))) t)


  (add-to-list 'treesit-fold-range-alist
               '(typescript-ts-mode
                 .
                 ((export_clause . ts-fold-range-seq)
                  (statement_block . treesit-fold-range-seq)
                  (comment . treesit-fold-range-c-like-comment)
                  (array . treesit-fold-range-seq)
                  (object . treesit-fold-range-seq))))

  (add-to-list 'treesit-fold-range-alist `(typescript-ts-mode . ,(treesit-fold-parsers-typescript)))
  (add-to-list 'treesit-fold-range-alist `(go-ts-mode . ,(treesit-fold-parsers-go)))

  ;; TODO: DOESN'T WORK for scss, needs another rules (check it later for custom pareser)
  (add-to-list 'treesit-fold-range-alist '(scss-mode . (css-mode
                                                        (keyframe_block_list . treesit-fold-range-seq)
                                                        (block . treesit-fold-range-seq)
                                                        (comment . treesit-fold-range-c-like-comment)))))
;; Treesitter folding:1 ends here

;; [[file:README.org::*Apheleia formatter (prettier)][Apheleia formatter (prettier):1]]
(use-package apheleia
  :hook (prog-mode . apheleia-global-mode)
  :bind (:map meow-normal-state-keymap
              ("\\p" . apheleia-format-buffer))
  :config
  (setf (alist-get 'kdlfmt apheleia-formatters)
        '("kdlfmt" "format" "-"))
  (add-to-list 'apheleia-mode-alist '(kdl-mode . kdlfmt))

  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(html-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier)))
;; Apheleia formatter (prettier):1 ends here

;; [[file:README.org::*Open configuration file][Open configuration file:1]]
(defun my/open-emacs-config ()
  "Open folder with emacs config"
  (interactive)
  (let ((default-directory my/config-file))
    (call-interactively 'find-file)))
;; Open configuration file:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package dired
  :defer t
  :ensure nil
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)
              ("\\c" . dired-create-empty-file))
  :config
  (setq dired-dwim-target t)
  ;; Use GNU ls if available (install via: brew install coreutils)
  (when (executable-find "gls")
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  (add-hook 'dired-mode-hook 'auto-revert-mode))
;; Core package:1 ends here

;; [[file:README.org::*Dired icons][Dired icons:1]]
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
;; Dired icons:1 ends here

;; [[file:README.org::*File info. Information about currently opened file][File info. Information about currently opened file:1]]
(use-package file-info
  :bind ("s-'" . file-info-show)
  :defer t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params '(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))
;; File info. Information about currently opened file:1 ends here

;; [[file:README.org::*Reveal in osx finder][Reveal in osx finder:1]]
(use-package reveal-in-osx-finder
  :defer t
  :bind (("C-c o f" . reveal-in-osx-finder))
  :ensure t)
;; Reveal in osx finder:1 ends here

;; [[file:README.org::*Dirvish.][Dirvish.:1]]
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind (("C-c o p" . dirvish-side))
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  (dirvish-side-width 45)
  (dirvish-side-attributes '(vc-state subtree-state file-name collapse))
  (dirvish-default-layout '(0 0.35 0.65))
  (dirvish-emerge-group '(("Recent files"  (predicate . recent-files-2h))
                          ("Documents"     (extensions "pdf" "tex" "bib" "epub"))
                          ("Video"         (extensions "mp4" "mkv" "webm"))
                          ("Pictures"      (extensions "jpg" "png" "svg" "gif"))
                          ("Audio"         (extensions "mp3" "flac" "wav" "ape" "aac"))
                          ("Archives"      (extensions "gz" "rar" "zip"))))

  :config
  (set-face-attribute 'dirvish-hl-line nil :foreground nil :background "#3E4451")
  (setq dirvish-attributes
        '(file-time collapse subtree-state))
  (setq delete-by-moving-to-trash t)
  :bind
  (:map dirvish-mode-map
        ("q" . dirvish-quit)
        :map dired-mode-map ; Dirvish respects all the keybindings in this map
        ("q" . dirvish-quit)
        ("h" . dired-up-directory)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("l" . dired-find-file)
        ("i" . wdired-change-to-wdired-mode)
        ("." . dired-omit-mode)
        ("b"   . dirvish-bookmark-jump)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-last)
        ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
        ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
        ("TAB" . dirvish-subtree-toggle)
        ("M-n" . dirvish-history-go-forward)
        ("M-p" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-f" . dirvish-toggle-fullscreen)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump)))
;; Dirvish.:1 ends here

;; [[file:README.org::*Clear history][Clear history:1]]
(defun my/clear-term-history ()
  "Clear terminal history inside vterm."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (vterm--self-insert)
    (vterm-send-string "clear")
    (vterm-send-return)))
;; Clear history:1 ends here

;; [[file:README.org::*Open in current buffer][Open in current buffer:1]]
(defun my/vterm-open-here ()
  "Open vterm inside current buffer."
  (interactive)
  (let ((bn vterm-buffer-name))
    (if (get-buffer bn)
        (switch-to-buffer bn)
      (vterm))))
;; Open in current buffer:1 ends here

;; [[file:README.org::*Vterm][Vterm:1]]
(use-package vterm
  :defer t
  :bind (("C-c o t" . vterm-toggle)
         ("C-c o v" . vterm)
         ("C-c o V" . my/vterm-open-here)
         :map vterm-mode-map
         ("C-u" . vterm--self-insert)
         ("C-c m c" . vterm-copy-mode))
  :custom
  (vterm-max-scrollback 5000)
  :config
  (setq vterm-clear-scrollback t)
  (advice-add 'vterm-clear-scrollback :before #'my/clear-term-history))
;; Vterm:1 ends here

;; [[file:README.org::*Toggle vterm][Toggle vterm:1]]
(use-package vterm-toggle
  :defer t
  :commands (vterm-toggle-show)
  :config
  (setq vterm-always-compile-module t)
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-toggle-scope 'project)
  (setq vterm-toggle-fullscreen-p nil)

  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (and (not (string-match-p "^\\*vterm" (buffer-name buffer)))
                            (or (eq major-mode 'vterm-mode)
                                (string-prefix-p vterm-buffer-name (buffer-name buffer)))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))
;; Toggle vterm:1 ends here

;; [[file:README.org::*Detached. Background commands][Detached. Background commands:1]]
(use-package detached
  :defer t
  :init
  (detached-init)
  :bind (([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ([remap detached-open-session] . detached-consult-session))
  :custom
  (detached-show-output-on-attach t)
  (detached-terminal-data-command system-type))
;; Detached. Background commands:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package magit
  :commands
  (magit-get-current-branch)
  :bind
  (("C-M-g" . magit-status)
   ("C-c g g" . magit-status)
   :map magit-mode-map
   ("C-1" . magit-section-show-level-1)
   ("C-2" . magit-section-show-level-2)
   ("C-3" . magit-section-show-level-3)
   ("C-j" . magit-section-forward)
   ("C-k" . magit-section-backward)
   ("C-4" . magit-section-show-level-4)
   ("q" . kill-current-buffer)
   ("<escape>" . kill-current-buffer)
   ("Q" . bury-buffer)
   (";" . meow-reverse)
   ("Z" . magit-stash)
   ("P" . magit-fetch)
   ("p" . magit-push)
   ("`" . magit-process-buffer)
   ("f" . avy-goto-word-1)
   ("x" . magit-discard)
   :map magit-status-mode-map
   ("x" . meow-line)
   ("X" . magit-discard)
   ("z z" . recenter)
   ("C-j" . magit-section-forward)
   ("C-k" . magit-section-backward)
   :map magit-diff-mode-map
   ("C-j" . magit-section-forward)
   ("C-k" . magit-section-backward))
  :custom-face
  (git-commit-overlong-summary ((t :inherit error :weight bold)))
  :custom
  (magit-diff-refine-hunk t)
  (magit-process-timestamp-format "%H:%M")
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-process-finish-apply-ansi-colors t)
  :hook
  (magit-process-mode . compilation-minor-mode)
  :config
  ;; Reset magit numbers
  (dolist (key (mapcar (lambda (n) (kbd (number-to-string n))) (number-sequence 0 9)))
    (define-key magit-mode-map key nil)
    (define-key magit-status-mode-map key nil))
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (define-key magit-file-section-map (kbd "C-j") 'magit-section-forward)
  (define-key magit-hunk-section-map (kbd "C-j") 'magit-section-forward)
  (setcdr magit-process-mode-map (cdr (make-keymap)))
  (set-keymap-parent magit-process-mode-map special-mode-map))
;; Core package:1 ends here

;; [[file:README.org::*Open git diff for current blame line][Open git diff for current blame line:1]]
(defun my/magit-show-commit-at-point ()
  "Open Magit revision for commit that last modified the current line."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (hash (string-trim
                (shell-command-to-string
                 (format "git blame -L %d,%d --porcelain %s | sed -n '1s/ .*//p'"
                         line line file)))))
    (if (string-match-p "^[0-9a-f]\\{7,40\\}$" hash)
        (magit-show-commit hash)
      (message "No commit found for current line."))))
;; Open git diff for current blame line:1 ends here

;; [[file:README.org::*Magit todos][Magit todos:1]]
(use-package magit-todos
  :bind (("C-c g n" . magit-todos-list)
         ("C-c l T" . magit-todos-list)
         ("C-c t l" . magit-todos-list)))
;; Magit todos:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package git-gutter
  :hook (after-init . git-gutter-mode)
  :bind (("C-M-[" . git-gutter:previous-hunk)
         ("C-M-]" . git-gutter:next-hunk)
         ("C-M-r" . git-gutter:revert-hunk))
  :custom
  (git-gutter:update-interval 2))
;; Core package:1 ends here

;; [[file:README.org::*Better gitgutter fringe][Better gitgutter fringe:1]]
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
;; Better gitgutter fringe:1 ends here

;; [[file:README.org::*Blamer][Blamer:1]]
(use-package blamer
  :defer t 
  :hook ((prog-mode . blamer-mode))
  ;; From MELPA, uncomment below for development version:
  ;; :ensure (:type git :host github :repo "artawower/blamer.el" :branch "fix/emacs-buffers-spawns")
  :bind (("C-c b i" . blamer-show-commit-info)
         ("C-c b h" . (lambda () (interactive) (blamer-show-commit-info 'visual)))
         ("s-i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.8)
  (blamer-min-offset 20)
  (blamer-max-commit-message-length 65)
  (blamer-commit-formatter "◉ %s")
  (blamer-view 'overlay)
  (blamer-uncommitted-changes-message "uncommitted yet")
  (blamer-tooltip-function 'blamer-tooltip-commit-message)
  :config
  ;; Setup face with font variables
  (set-face-attribute 'blamer-face nil
                      :inherit 'font-lock-comment-face
                      :italic t
                      :font (format "%s %d" my/font-funny my/font-height)
                      :height 0.8
                      :background 'unspecified)
  
  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (forge-browse-commit commit-hash))))

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff))))
;; Blamer:1 ends here

;; [[file:README.org::*Diff hl][Diff hl:1]]
;; Diff faces customization - relying on theme for colors
(use-package diff-hl)
;; Diff hl:1 ends here

;; [[file:README.org::*Face setup][Face setup:1]]
(defun my/ediff-faces-setup ()
  "Make Ediff faces copy exact colors from Diff faces."
  (let* ((add-bg (face-attribute 'diff-added :background nil 'default))
         (add-fg (face-attribute 'diff-added :foreground nil 'default))
         (rem-bg (face-attribute 'diff-removed :background nil 'default))
         (rem-fg (face-attribute 'diff-removed :foreground nil 'default))
         (chg-bg (if (facep 'diff-changed)
                     (face-attribute 'diff-changed :background nil 'default)
                   add-bg))
         (chg-fg (if (facep 'diff-changed)
                     (face-attribute 'diff-changed :foreground nil 'default)
                   add-fg))
         (radd-bg (if (facep 'diff-refine-added)
                      (face-attribute 'diff-refine-added :background nil 'default)
                    add-bg))
         (radd-fg (if (facep 'diff-refine-added)
                      (face-attribute 'diff-refine-added :foreground nil 'default)
                    add-fg))
         (rrem-bg (if (facep 'diff-refine-removed)
                      (face-attribute 'diff-refine-removed :background nil 'default)
                    rem-bg))
         (rrem-fg (if (facep 'diff-refine-removed)
                      (face-attribute 'diff-refine-removed :foreground nil 'default)
                    rem-fg))
         (rchg-bg (if (facep 'diff-refine-changed)
                      (face-attribute 'diff-refine-changed :background nil 'default)
                    radd-bg))
         (rchg-fg (if (facep 'diff-refine-changed)
                      (face-attribute 'diff-refine-changed :foreground nil 'default)
                    radd-fg)))
    (set-face-attribute 'ediff-current-diff-A nil :inherit nil :background rem-bg :foreground rem-fg :extend t)
    (set-face-attribute 'ediff-current-diff-B nil :inherit nil :background add-bg :foreground add-fg :extend t)
    (set-face-attribute 'ediff-current-diff-C nil :inherit nil :background chg-bg :foreground chg-fg :extend t)
    (set-face-attribute 'ediff-fine-diff-A    nil :inherit nil :background rrem-bg :foreground rrem-fg :extend t)
    (set-face-attribute 'ediff-fine-diff-B    nil :inherit nil :background radd-bg :foreground radd-fg :extend t)
    (set-face-attribute 'ediff-fine-diff-C    nil :inherit nil :background rchg-bg :foreground rchg-fg :extend t)
    (dolist (f '(ediff-even-diff-A ediff-even-diff-B ediff-even-diff-C
                                   ediff-odd-diff-A ediff-odd-diff-B ediff-odd-diff-C
                                   ediff-current-diff-Ancestor ediff-even-diff-Ancestor
                                   ediff-odd-diff-Ancestor ediff-fine-diff-Ancestor))
      (set-face-attribute f nil :inherit 'default :foreground 'unspecified :background 'unspecified :extend t))))
;; Face setup:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package ediff
  :ensure nil
  :defer t
  :config
  ;; ediff inherit from diff-added, diff removed and diff base
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-highlight-all-diffs t)

  (add-hook 'ediff-load-hook #'my/ediff-faces-setup)
  (add-hook 'ediff-prepare-buffer-hook (lambda () (toggle-truncate-lines -1)))
  (add-hook 'ediff-prepare-buffer-hook #'display-line-numbers-mode)
  (add-hook 'ediff-load-hook #'zoom-mode)
  (add-hook 'ediff-cleanup-hook
            (lambda ()
              (dolist (buf (list ediff-buffer-A ediff-buffer-B
                                 (when (boundp 'ediff-buffer-C) ediff-buffer-C)))
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (display-line-numbers-mode -1))))
              (zoom-mode))))
;; Core package:1 ends here

;; [[file:README.org::*Timemachine][Timemachine:1]]
(use-package git-timemachine
  :hook (git-timemachine-mode . font-lock-mode)
  :bind (("C-M-t" . git-timemachine)
         ("C-M-m" . git-timemachine)
         :map git-timemachine-mode-map
         ("C-k" . git-timemachine-show-previous-revision)
         ("q" . git-timemachine-quit)
         ("<escape>" . git-timemachine-quit)
         ("Q" . git-timemachine-quit)
         ("C-j" . git-timemachine-show-next-revision)))
;; Timemachine:1 ends here

;; [[file:README.org::*Smerge][Smerge:1]]
(use-package smerge-mode
  :ensure nil
  :bind (("C-c C-l" . smerge-keep-lower)
         ("C-c C-u" . smerge-keep-upper)
         ("C-c C-a" . smerge-keep-all)
         ("C-c C-j" . smerge-next)
         ("C-c C-k" . smerge-prev)))
;; Smerge:1 ends here

;; [[file:README.org::*Browse at remote][Browse at remote:1]]
(use-package browse-at-remote
  :bind (("C-c o r" . browse-at-remote)))
;; Browse at remote:1 ends here

;; [[file:README.org::*Buffer navigation Functions][Buffer navigation Functions:1]]
(defun my/tabspaces--internal-buffer-p (buf)
  "Return non-nil if BUF is internal (minibuffer or name starts with a space)."
  (or (minibufferp buf)
      (let ((n (buffer-name buf)))
        (and n (> (length n) 0) (eq (aref n 0) ?\s)))))

(defun my/tabspaces--allowed-p (buf)
  "Return non-nil if BUF belongs to the current tabspace and is not internal."
  (and (memq buf (tabspaces--buffer-list))
       (buffer-live-p buf)
       (not (my/tabspaces--internal-buffer-p buf))))

(defun my/tabspaces-switch-to-prev-buffer ()
  "Like `switch-to-prev-buffer' but stay within the current tabspace.
Falls back to `switch-to-prev-buffer' if `tabspaces-mode' is not active."
  (interactive)
  (if (bound-and-true-p tabspaces-mode)
      (let ((start (current-buffer))
            ;; Safety to avoid infinite loops in degenerate histories.
            (limit 100)
            (found nil))
        (while (and (> limit 0) (not found))
          (setq limit (1- limit))
          (switch-to-prev-buffer)
          (setq found (or (eq (current-buffer) start) ; wrapped around
                          (my/tabspaces--allowed-p (current-buffer)))))
        (unless (my/tabspaces--allowed-p (current-buffer))
          (switch-to-buffer start)
          (user-error "No previous buffer in this tabspace")))
    (call-interactively #'switch-to-prev-buffer)))

(defun my/tabspaces-switch-to-next-buffer ()
  "Like `switch-to-next-buffer' but stay within the current tabspace.
Falls back to `switch-to-next-buffer' if `tabspaces-mode' is not active."
  (interactive)
  (if (bound-and-true-p tabspaces-mode)
      (let ((start (current-buffer))
            (limit 100)
            (found nil))
        (while (and (> limit 0) (not found))
          (setq limit (1- limit))
          (switch-to-next-buffer)
          (setq found (or (eq (current-buffer) start)
                          (my/tabspaces--allowed-p (current-buffer)))))
        (unless (my/tabspaces--allowed-p (current-buffer))
          (switch-to-buffer start)
          (user-error "No next buffer in this tabspace")))
    (call-interactively #'switch-to-next-buffer)))
;; Buffer navigation Functions:1 ends here

;; [[file:README.org::*Safe kill tab for daemon mode][Safe kill tab for daemon mode:1]]
(defun my/safe-tabspaces-kill-buffers-close-workspace ()
  "Safely kill buffers and close workspace.
Prevents closing the last frame in daemon mode on macOS."
  (interactive)
  (let ((tab-count (length (tab-bar-tabs)))
        (frame-count (length (frame-list))))
    (cond
     ((and (daemonp)
           (= tab-count 1)
           (= frame-count 1))
      (message "Cannot close the last workspace in daemon mode. Killing buffers only.")
      (tabspaces-clear-buffers))
     ((= tab-count 1)
      (message "Closing last tab in this frame...")
      (tabspaces-kill-buffers-close-workspace))
     (t
      (tabspaces-kill-buffers-close-workspace)))))
;; Safe kill tab for daemon mode:1 ends here

;; [[file:README.org::*Core package. Tabspaces][Core package. Tabspaces:1]]
(use-package tabspaces
  :ensure (:type git :host github :repo "mclear-tools/tabspaces")
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :bind (("s-1" . (lambda () (interactive) (tab-bar-select-tab 1)))
         ("s-2" . (lambda () (interactive) (tab-bar-select-tab 2)))
         ("s-3" . (lambda () (interactive) (tab-bar-select-tab 3)))
         ("s-4" . (lambda () (interactive) (tab-bar-select-tab 4)))
         ("s-5" . (lambda () (interactive) (tab-bar-select-tab 5)))
         ("s-6" . (lambda () (interactive) (tab-bar-select-tab 6)))
         ("C-c b0" . tabspaces-clear-buffers)
         ("C-c TAB p" . tabspaces-open-or-create-project-and-workspace)
         ("C-c TAB n" . tabspaces-switch-or-create-workspace)
         ("C-c bt" . tabspaces-switch-to-buffer)
         ("C-c TAB b" . tabspaces-switch-to-buffer)
         ("C-c TAB B" . switch-to-buffer)
         ("s-w" . delete-window)
         ("C-c TAB d" . my/safe-tabspaces-kill-buffers-close-workspace)
         ("C-c TAB S" . tabspaces-save-current-project-session)
         ("C-c TAB s" . tabspaces-save-session)
         ("C-c TAB r" . tab-rename)
         ("C-c TAB l" . tabspaces-restore-session)
         ("C-c ]" . my/tabspaces-switch-to-next-buffer)
         ("C-c [" . my/tabspaces-switch-to-prev-buffer))
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tabspaces-mode 1)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "README.org")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '())
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  (tab-bar-new-tab-choice nil))
;; Core package. Tabspaces:1 ends here

;; [[file:README.org::*Tab bar][Tab bar:1]]
(use-package tab-bar
  :defer t
  :bind (("C-c TAB w" . tab-switch)
         ("C-c w w" . tab-switch))
  :ensure nil
  :custom
  (tab-bar-show nil))
;; Tab bar:1 ends here

;; [[file:README.org::*Project.el][Project.el:1]]
(use-package project
  :after meow
  :ensure nil
  :bind (("C-c p p" . project-switch-project)
         ("C-c p d" . project-forget-project)
         ("C-c SPC" . project-find-file)
         ("C-c f f" . project-find-file)
         ("C-c f d" . project-find-dir)
         ("C-c p c" . project-compile)
         ("C-c p r" . project-recompile)
         ("C-c p b" . project-switch-to-buffer)))
;; Project.el:1 ends here

;; [[file:README.org::*Ibuffer][Ibuffer:1]]
(use-package ibuffer
  :defer t
  :ensure nil
  :bind ("C-c i b" . ibuffer))
;; Ibuffer:1 ends here

;; [[file:README.org::*Icons][Icons:1]]
(use-package nerd-icons-ibuffer 
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
;; Icons:1 ends here

;; [[file:README.org::*Helper functions][Helper functions:1]]
(defun my/vertico-insert-home ()
  "Kill line and insert home directory."
  (interactive)
  (kill-whole-line)
  (insert "~/"))

(defun my/vertico-insert-root ()
  "Kill line and insert root directory."
  (interactive)
  (kill-whole-line)
  (insert "/"))
;; Helper functions:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package vertico
  :ensure (:host github :repo "minad/vertico" :files ("vertico.el" "extensions/*.el"))
  :init
  ;; Add prompt indicator to `completing-read-multiple'
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  
  :hook (emacs-startup-hook . vertico-mode)
  :bind (("C-c ;" . vertico-repeat-last)
         ("C-c '" . vertico-repeat)
         :map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-n" . vertico-next-group)
         ("C-p" . vertico-previous-group)
         ("C-l" . vertico-quick-jump)
         ("C-o" . embark-act)
         ("C-q" . vertico-exit-input)
         ("C-n" . vertico-next-group)
         ("C-p" . vertico-previous-group)
         ("<escape>" . abort-minibuffers)
         ("C-d" . my/vertico-insert-home)
         ("C-r" . my/vertico-insert-root))
  :custom
  (vertico-cycle t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (set-face-attribute 'vertico-current nil :inherit 'region)
  (vertico-mouse-mode 1)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (and vertico-mode
                          (fboundp 'consult-completion-in-region))
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))
;; Core package:1 ends here

;; [[file:README.org::*Vertico repeat][Vertico repeat:1]]
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))
;; Vertico repeat:1 ends here

;; [[file:README.org::*Orderless][Orderless:1]]
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
;; Orderless:1 ends here

;; [[file:README.org::*Save hist][Save hist:1]]
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))
;; Save hist:1 ends here

;; [[file:README.org::*Bulk grep operation][Bulk grep operation:1]]
(use-package wgrep
  :bind ("C-c C-w" . wgrep-change-to-wgrep-mode))
;; Bulk grep operation:1 ends here

;; [[file:README.org::*Embark preview function][Embark preview function:1]]
(defun my/vertico-embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))
;; Embark preview function:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package embark
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-SPC" . my/vertico-embark-preview)
   ("C-u" . backward-kill-line)
   :map read--expression-map
   ("C-SPC" . my/vertico-embark-preview)
   ("C-u" . backward-kill-line)
   :map vertico-map
   ("C-SPC" . my/vertico-embark-preview)
   ("C-u" . backward-kill-line))
  
  :config
  (add-to-list 'display-buffer-alist '("^\\*Embark Export\\*$" (display-buffer-in-side-window) (window-height . 0.4)))
  (defun copy-grep-results-as-kill (strings)
    (embark-copy-as-kill
     (mapcar (lambda (string)
               (substring string
                          (1+ (next-single-property-change
                               (1+ (next-single-property-change 0 'face string))
                               'face string))))
             strings)))
  
  (add-to-list 'embark-multitarget-actions 'copy-grep-results-as-kill)
  
  (defvar-keymap embark-consult-grep-map
    :doc "Keymap for actions for consult-grep results."
    :parent embark-general-map
    "w" #'copy-grep-results-as-kill)
  
  (setf (alist-get 'consult-grep embark-keymap-alist) 'embark-consult-grep-map))
;; Core package:1 ends here

;; [[file:README.org::*Embark consult][Embark consult:1]]
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; Embark consult:1 ends here

;; [[file:README.org::*Completion annotations marginalia][Completion annotations marginalia:1]]
(use-package marginalia
  :hook (emacs-startup . marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :config
  (pushnew! marginalia-command-categories
          '(+default/find-file-under-here . file)
          '(flycheck-error-list-set-filter . builtin)
          '(persp-switch-to-buffer . buffer)))
;; Completion annotations marginalia:1 ends here

;; [[file:README.org::*Search in line or region][Search in line or region:1]]
(defun my/consult-line-or-region ()
  "Search in buffer depends or visual or normal line"
  (interactive)
  (if (region-active-p)
      (let ((substring (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-line substring))
    (consult-line)))
;; Search in line or region:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package consult
  :bind (("s-f" . my/consult-line-or-region)
         ("C-c b b" . consult-buffer-other-tab)
         ("C-c b a" . consult-buffer)
         ("C-c b b" . consult-project-buffer)
         ("C-c f P" . my/open-emacs-config) 
         ("C-c f p" . consult-ripgrep)
         ("C-c *" . (lambda () (interactive) (consult-ripgrep nil (thing-at-point 'symbol))))
         ("C-c s i" . consult-imenu)
         ("C-c RET" . consult-bookmark)
         ("C-c c m" . consult-mark)
         ("C-c f r" . consult-recent-file)
         ("C-c f R" . consult-recent-file))
  :custom
  (consult-preview-key "C-SPC")
  (recentf-max-saved-items 3000)
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (recentf-max-menu-items 3000)
  (consult-narrow-key "<") ;; (kbd "C-SPC")
  :init
  (recentf-mode 1)
  

  :config
  (consult-customize
   consult-git-grep consult-grep consult-projectile-recentf consult-ripgrep
   consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key (list :debounce 0.2 "C-SPC"))

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))
;; Core package:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package corfu
  :defer 2
  :custom
  (corfu-auto t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-auto-delay 0.1)
  (corfu-echo-documentation t) ;; Do not show documentation in the echo area
  (corfu-preselect-first nil)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s) 
  :bind
  (("C-x C-o" . completion-at-point)
   ("C-SPC" . completion-at-point)
   :map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("C-j" . corfu-next)
   ("C-k" . corfu-previous)
   ("S-TAB" . corfu-previous)
   ("<return>" . corfu-insert)
   ("RET" . corfu-insert)
   ("C-<return>" . corfu-insert-seporator)
   ([backtab] . corfu-previous))
  :config
  (add-hook 'meow-insert-exit-hook (lambda () (corfu-quit)))
  (setq lsp-completion-provider :none)

  (global-corfu-mode))
;; Core package:1 ends here

;; [[file:README.org::*Corfu history][Corfu history:1]]
(use-package corfu-history
  :ensure (corfu-history :host github :repo "minad/corfu" :files ("extensions/corfu-history.el"))
  :after corfu
  :config
  (with-eval-after-load 'safehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
   (setq corfu-sort-override-function 'corfu-history--sort)

  (corfu-history-mode))
;; Corfu history:1 ends here

;; [[file:README.org::*Corfu popup doc][Corfu popup doc:1]]
(use-package corfu-popupinfo
  :ensure (corfu-popupinfo :host github :repo "minad/corfu" :files ("extensions/corfu-popupinfo.el"))
  :bind (:map corfu-map
              ("C-m" . corfu-popupinfo-documentation))
  :custom
  (corfu-echo-delay nil)
  (corfu-popupinfo-delay '(0.5 . 0.5))
  :after corfu
  :config
  (corfu-popupinfo-mode))
;; Corfu popup doc:1 ends here

;; [[file:README.org::*Pretty icon][Pretty icon:1]]
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; Pretty icon:1 ends here

;; [[file:README.org::*Cape][Cape:1]]
(use-package cape
  :bind ("C-c C-e" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; Cape:1 ends here

;; [[file:README.org::*Completion icons][Completion icons:1]]
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-mode))
;; Completion icons:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package lsp-mode
  :hook
  (prog-mode . lsp-deferred)
  :bind
  (("C-c f n" . flycheck-next-error)
   ("C-c l a" . lsp-execute-code-action)
   ("C-c i m" . lsp-ui-imenu)
   ("C-c l d" . lsp-workspace-folders-remove)
   ("C-c l t" . lsp-find-type-definition)
   ("C-c l f" . lsp-find-references)
   ("C-c l i" . lsp-find-implementation)
   ("C-c l w" . lsp-workspace-restart)
   ("C-c r l" . lsp)
   ("C-c l r" . lsp-rename)
   ("C-c e c" . flycheck-copy-errors-as-kill)
   :map meow-normal-state-keymap
   ("gt" . lsp-goto-type-definition)
   ("gi" . lsp-goto-implementation)
   ("\\l" . lsp-execute-code-action))
  :bind-keymap
  ("s-9" . lsp-command-map)
  :init
  (setq lsp-keymap-prefix "s-9")
  (setq lsp-volar-take-over-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :custom
  (lsp-log-io nil)
  (lsp-idle-delay 0.3)
  (lsp-auto-guess-root t)
  (lsp-completion-provider :capf)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-enable-indentation nil)
  (lsp-apply-edits-after-file-operations nil)
  (lsp-clients-typescript-server-args '("--stdio"))
  (lsp-completion-default-behaviour :insert)
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  (lsp-disabled-clients '(html-ls vls vue-semantic-server))
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  (lsp-enable-snippet nil)
  (lsp-use-plists t)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 8000)
  (lsp-modeline-code-actions-mode nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-modeline-code-action-icons-enable nil)
  (lsp-modeline-diagnostics-mode nil)
  (lsp-modeline-diagnostics-enable nil)
  :config
  (setq lsp-pyright-venv-path (concat (getenv "HOME") "/.virtualenvs"))
  (setq lsp-clients-angular-language-server-command
        '("node" "./node_modules/@angular/language-server/index.js"
          "--stdio"
          "--tsProbeLocations" "."
          "--ngProbeLocations" "."))
  (define-key lsp-mode-map (kbd "s-9") lsp-command-map)
  (setq lsp-auto-execute-action nil)
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-json-schemas
        `[
          (:fileMatch ["ng-openapi-gen.json"] :url "https://raw.githubusercontent.com/cyclosproject/ng-openapi-gen/master/ng-openapi-gen-schema.json")
          (:fileMatch ["package.json"] :url "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/package.json")
          (:fileMatch ["opencode.json"] :url "https://opencode.ai/config.json")
          ])
  (set-face-attribute 'lsp-face-highlight-read nil :foreground "#61AFEF" :bold t :underline nil)
  ;; Eslint
  (setq lsp-eslint-download-url "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/dbaeumer/vsextensions/vscode-eslint/3.0.10/vspackage") ;; latest VSCode eslint extension from marketplace
  (setq lsp-eslint-server-command `("node"
                                    "/Users/darkawower/.vscode/extensions/dbaeumer.vscode-eslint-3.0.10/server/out/eslintServer.js"
                                    "--stdio"))
  (setq lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
        lsp-pyls-plugins-flake8-enabled nil)
  (set-face-foreground 'lsp-face-highlight-read "#61AFEF")
  (set-face-foreground 'lsp-face-highlight-textual "#61AFEF")

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bun\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.npm\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.act\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pnpm-store\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (set-face-attribute 'lsp-face-highlight-textual nil :background "#c0caf5")
  (my/setup-compilation-errors)
  (setq lsp-volar-hybrid-mode nil)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vtsls" "--stdio"))
    :activation-fn (lsp-activate-on "typescript" "javascript" "typescriptreact" "javascriptreact")
    :priority 1 
    :server-id 'vtsls
    :multi-root t
    :initialization-options (lambda ()
                              (list :typescript (list :tsdk "/path/to/typescript/lib"))))))
;; Core package:1 ends here

;; [[file:README.org::*Lsp optimisation. Lsp booster.][Lsp optimisation. Lsp booster.:1]]
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;; Lsp optimisation. Lsp booster.:1 ends here

;; [[file:README.org::*Custom vujs server][Custom vujs server:1]]
;;;; Vue Take Over Mode via tsserver + @vue/typescript-plugin
;;;; Paste into init.el / config.el  -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode

;; -------- USER OPTIONS --------
(defvar my/vue-ts-plugin-location nil
  "Absolute path to @vue/typescript-plugin directory (where package.json is).
If nil — search automatically (project -> global -> inside @vue/language-server).")

(defvar my/vue-root-markers
  '("pnpm-workspace.yaml" "yarn.lock" "bun.lockb" "package.json"
    "tsconfig.json" "jsconfig.json" ".git")
  "Files/folders to determine project root.")

(setq lsp-auto-guess-root t
      lsp-enable-file-watchers t
      lsp-clients-typescript-max-ts-server-memory 4096)

(require 'lsp-mode)
(require 'subr-x)

;; -------- helpers --------
(defun my/trim (s) (string-trim (or s "")))

(defun my/vue-project-root ()
  (or (when (fboundp 'project-current)
        (when-let* ((pr (project-current nil))) (car (project-roots pr))))
      (when (fboundp 'lsp-workspace-root) (lsp-workspace-root))
      default-directory))

(defun my/find-up (start markers)
  (seq-some (lambda (m)
              (let ((dir (locate-dominating-file start m)))
                (when dir (expand-file-name dir))))
            markers))

(defun my/vue-find-root ()
  (or (my/find-up (or buffer-file-name default-directory) my/vue-root-markers)
      (file-name-directory (or buffer-file-name default-directory))
      default-directory))

(defun my/vue-find-plugin-in-project ()
  (when-let* ((root (my/vue-project-root))
              (nm   (locate-dominating-file root "node_modules")))
    (let* ((p1 (expand-file-name "node_modules/@vue/typescript-plugin" nm))
           (p2 (expand-file-name "node_modules/@vue/language-server/node_modules/@vue/typescript-plugin" nm)))
      (cond ((file-exists-p p1) p1)
            ((file-exists-p p2) p2)))))

(defun my/npm-root-global ()
  (when-let* ((npm (executable-find "npm")))
    (my/trim (with-temp-buffer
               (ignore-errors (call-process npm nil t nil "root" "-g"))
               (buffer-string)))))

(defun my/vue-find-plugin-global ()
  (when-let* ((root (my/npm-root-global)))
    (let* ((p1 (expand-file-name "@vue/typescript-plugin" root))
           (p2 (expand-file-name "@vue/language-server/node_modules/@vue/typescript-plugin" root)))
      (cond ((file-exists-p p1) p1)
            ((file-exists-p p2) p2)))))

(defun my/vue-plugin-location ()
  (or my/vue-ts-plugin-location
      (my/vue-find-plugin-in-project)
      (my/vue-find-plugin-global)))

(defun my/vue-plugins-vector ()
  (let ((loc (my/vue-plugin-location)))
    (unless (and loc (file-exists-p loc))
      (message "[vue-takeover] WARN: @vue/typescript-plugin not found. Set `my/vue-ts-plugin-location` or install it."))
    (vector (list :name "@vue/typescript-plugin"
                  :location (or loc (my/vue-project-root))
                  :languages ["vue"]))))

;; -------- language-ids & tsserver defaults --------
(with-eval-after-load 'lsp-mode
  ;; *.vue should have id "vue"
  (dolist (pair '((vue-ts-mode . "vue") (vue-mode . "vue") (web-mode . "vue")))
    (add-to-list 'lsp-language-id-configuration pair))
  ;; ensure TS/JS mapping
  (dolist (pair '((typescript-mode     . "typescript")
                  (typescript-ts-mode  . "typescript")
                  (tsx-ts-mode         . "typescriptreact")
                  (js-mode             . "javascript")
                  (js2-mode            . "javascript")
                  (js-ts-mode          . "javascript")
                  (rjsx-mode           . "javascriptreact")))
    (add-to-list 'lsp-language-id-configuration pair)))

(with-eval-after-load 'lsp-javascript
  (setq lsp-clients-typescript-prefer-use-project-ts-server t
        lsp-clients-typescript-plugins (my/vue-plugins-vector)))

;; -------- Vue buffers: start ts-ls, disable Volar/VLS --------
(defun my/vue-buffer-p ()
  (and buffer-file-name (string-equal (file-name-extension buffer-file-name) "vue")))

(defun my/disable-vue-servers ()
  (setq-local lsp-disabled-clients '(vue-semantic-server volar vls vetur-ls)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("typescript-language-server" "--stdio"))
    :server-id 'ts-ls-vue
    :multi-root t
    :priority -1
    :add-on? nil
    :activation-fn (lambda (&rest _) (my/vue-buffer-p))
    :initialization-options (lambda ()
                              (list :plugins (vconcat (or lsp-clients-typescript-plugins
                                                          (my/vue-plugins-vector)))))
    :language-id "vue"))

  (defun my/vue-ensure-lsp ()
    (when (my/vue-buffer-p)
      (my/disable-vue-servers)
      (let* ((root (my/vue-find-root))
             (tru-root (file-truename root))
             (session (lsp-session)))
        (unless (seq-some (lambda (f) (string-equal (file-truename f) tru-root))
                          (lsp-session-folders session))
          (lsp-workspace-folders-add tru-root))
        (let ((default-directory tru-root))
          (lsp-deferred)))))

  (dolist (hook '(vue-ts-mode-hook vue-mode-hook web-mode-hook))
    (add-hook hook #'my/vue-ensure-lsp)))
)
;; Custom vujs server:1 ends here

;; [[file:README.org::*Tailwind css][Tailwind css:1]]
(use-package lsp-tailwindcss
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save))
;; Tailwind css:1 ends here

;; [[file:README.org::*LSP. Pyright][LSP. Pyright:1]]
(use-package lsp-pyright
  :defer t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-diagnostic-mode "workspace"))
;; LSP. Pyright:1 ends here

;; [[file:README.org::*YAML PRO][YAML PRO:1]]
(use-package yaml-pro
  :defer t
  :hook (yaml-mode . yaml-pro-mode))
;; YAML PRO:1 ends here

;; [[file:README.org::*LSP UI][LSP UI:1]]
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure (:host github :repo "emacs-lsp/lsp-ui" :rev "9d28c2ca1e79614215ea678583435d5d1189e4bc")
  :after lsp-mode
  :bind (:map meow-normal-state-keymap
         ("\\h" . lsp-ui-doc-toggle)
         :map lsp-ui-peek-mode-map
         ("C-j" . lsp-ui-peek--select-next)
         ("C-k" . lsp-ui-peek--select-prev))
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 100
        lsp-ui-sideline-diagnostic-max-lines 8
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-delay 1
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil))
;; LSP UI:1 ends here

;; [[file:README.org::*Flutter (dart)][Flutter (dart):1]]
(use-package lsp-dart
  :defer t
  :hook (dart-mode . (lambda () (interactive)
                       (add-hook 'after-save-hook
                                 (lambda ()
                                   ;; (flutter-run-or-hot-reload)
                                   (flutter-hot-restart)
                                   )
                                 t t)))
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  :config
  (defun lsp-dart-flutter-widget-guide--add-overlay-to (buffer line col string)
    "Add to BUFFER at LINE and COL a STRING overlay."
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (move-to-column col)
      (when (string= lsp-dart-flutter-widget-guide-space (string (following-char)))
        (let ((ov (make-overlay (point) (1+ (point)) buffer)))
          (overlay-put ov 'category 'lsp-dart-flutter-widget-guide)
          (overlay-put ov 'display (propertize string
                                               'face 'custom-comment-tag)))))))
;; Flutter (dart):1 ends here

;; [[file:README.org::*LSP java][LSP java:1]]
(use-package lsp-java
  :hook (java-mode . lsp-deferred))
;; LSP java:1 ends here

;; [[file:README.org::*Flymake, copy errors][Flymake, copy errors:1]]
(defun my/flymake--format-diagnostic (diag)
  "Return a formatted string for DIAG.
Format: LINE:COL [TYPE] MESSAGE."
  (let* ((beg  (flymake-diagnostic-beg diag))
         (line (line-number-at-pos beg))
         (col  (save-excursion (goto-char beg) (1+ (current-column))))
         (typ  (flymake-diagnostic-type diag))
         (msg  (flymake-diagnostic-text diag)))
    (format "%d:%d [%s] %s" line col typ msg)))

(defun my/flymake-kill-error ()
  "Copy the Flymake diagnostic under point to the kill ring.
If multiple diagnostics are present at point, copy the first one."
  (interactive)
  (let* ((diags (flymake-diagnostics (point) (point))))
    (if (null diags)
        (user-error "No Flymake diagnostic at point")
      (let* ((diag (car diags))
             (text (my/flymake--format-diagnostic diag)))
        (kill-new text)
        (message "Copied: %s" text)))))

(defun my/flymake-kill-errors ()
  "Copy all Flymake diagnostics in the current buffer to the kill ring.
Each diagnostic is written on a separate line."
  (interactive)
  (let* ((diags (flymake-diagnostics)))
    (if (null diags)
        (user-error "No Flymake diagnostics in this buffer")
      (let* ((lines (mapcar #'my/flymake--format-diagnostic diags))
             (blob  (string-join lines "\n")))
        (kill-new blob)
        (message "Copied %d Flymake diagnostics" (length diags))))))
;; Flymake, copy errors:1 ends here

;; [[file:README.org::*Core Package][Core Package:1]]
(use-package flymake
  :ensure nil
  :after (lsp-mode)
  :bind
  (("C-j" . flymake-goto-next-error)
   ("C-k" . flymake-goto-prev-error)
   ("C-c e l" . flymake-show-diagnostics-buffer)
   ("C-c l e" . flymake-show-diagnostics-buffer)
   ("C-c e c" . my/flymake-kill-error)
   ("C-c e C" . my/flymake-kill-errors))
  :hook 
  (prog-mode . flymake-mode)
  (flymake-diagnostics-buffer-mode . meow-normal-mode)
  :config
  (setq lsp-diagnostics-provider :flymake)
  (setq flymake-show-diagnostics-at-end-of-line nil)
  
  (defun my/lsp-diagnostics--flymake-update-diagnostics ()
      "Report new diagnostics to flymake."
      (funcall lsp-diagnostics--flymake-report-fn
               (-some->> (lsp-diagnostics t)
                 (gethash (lsp--fix-path-casing buffer-file-name))
                 (--map (-let* (((&Diagnostic :message :severity?
                                              :range (range &as &Range
                                                            :start (&Position :line start-line :character)
                                                            :end (&Position :line end-line))) it)
                                ((start . end) (lsp--range-to-region range)))
                          (when (= start end)
                            (if-let* ((region (flymake-diag-region (current-buffer)
                                                                  (1+ start-line)
                                                                  character)))
                                (setq start (car region)
                                      end (cdr region))
                              (lsp-save-restriction-and-excursion
                                (goto-char (point-min))
                                (setq start (line-beginning-position (1+ start-line))
                                      end (line-end-position (1+ end-line))))))
                          (flymake-make-diagnostic (current-buffer)
                                                   start
                                                   end
                                                   (cl-case severity?
                                                     (1 :error)
                                                     (2 :warning)
                                                     (t :note))
                                                   message))))))
  
  (advice-add #'lsp-diagnostics--flymake-update-diagnostics
                :override #'my/lsp-diagnostics--flymake-update-diagnostics)
  (setq flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t
        flymake-start-on-newline t
        flymake-no-changes-timeout 0.1)
  
  )
;; Core Package:1 ends here

;; [[file:README.org::*Flymake, stylelint][Flymake, stylelint:1]]
(use-package flymake-stylelint
  :ensure (flymake-stylelint
             :type git
             :host github
             :repo "orzechowskid/flymake-stylelint")
  :hook ((css-mode . flymake-stylelint-enable)
         (scss-mode . flymake-stylelint-enable)
         (less-mode . flymake-stylelint-enable)
         (web-mode . flymake-stylelint-enable))
  :config
  (setq flymake-stylelint-executable "stylelint"))
;; Flymake, stylelint:1 ends here

;; [[file:README.org::*Eldoc][Eldoc:1]]
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0)
  :config
  (setq eldoc-message-function (lambda (&rest params) nil))
  (global-eldoc-mode))
;; Eldoc:1 ends here

;; [[file:README.org::*Run vitest][Run vitest:1]]
(defun my/run-vitest-from-current-buffer ()
  "Run vitest for the current buffer's file."
  (interactive)
  (let* ((file (file-info--get-project-related-path)))
    (if file
        (compile (format "bun run test %s" (shell-quote-argument file)))
      (message "Buffer is not visiting a file."))))
;; Run vitest:1 ends here

;; [[file:README.org::*Find filename for eslint error][Find filename for eslint error:1]]
(defun compile-eslint--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      (when (re-search-backward (rx bol (group "/" (+ any)) eol))
        (list (match-string 1))))))
;; Find filename for eslint error:1 ends here

;; [[file:README.org::*Buffer position][Buffer position:1]]
(defun my/get-rightmost-window ()
  "Get the rightmost window on the current frame."
  (let ((windows (window-list)))
    (car (sort windows (lambda (a b)
                        (> (nth 0 (window-edges a))
                           (nth 0 (window-edges b))))))))

(defun my/display-compilation-buffer (buffer alist)
  "Custom display function for compilation buffer.
   If there are two windows, reuse the rightmost one.
   If there's only one window, split and use the right window."
  (let ((windows (window-list)))
    (cond
     ;; If we have exactly two windows
     ((= (length windows) 2)
      (set-window-buffer (my/get-rightmost-window) buffer)
      (my/get-rightmost-window))
     ;; If we have one window
     (t
      (let ((new-window (split-window-right)))
        (set-window-buffer new-window buffer)
        new-window)))))

(add-to-list 'display-buffer-alist
             `("^\\*compilation\\*$"
               (display-buffer-reuse-window my/display-compilation-buffer)
               (reusable-frames . visible)))
;; Buffer position:1 ends here

;; [[file:README.org::*Buffer position][Buffer position:2]]
(defun my/get-visible-buffers-cnt ()
  "Get the number of visible buffers"
  (let ((visible-buffers 0)
        (buffer-list (buffer-list)))
    (dolist (buffer buffer-list)
      (if (get-buffer-window buffer)
          (cl-incf visible-buffers)))
    visible-buffers))

(defun my/display-buffer-other-vertical (buffer &optional alist)
  "Display BUFFER in another window. If only one window, split vertical before."
  (if (one-window-p)
      (split-window-horizontally))
  (display-buffer-use-some-window buffer alist))
  

(add-to-list 'display-buffer-alist
             '(("\\*Messages\\*"
                (display-buffer-reuse-window display-buffer-in-side-window my/display-buffer-other-vertical display-buffer-use-some-window)
                (side . right))))
;; Buffer position:2 ends here

;; [[file:README.org::*Setup compilation errors][Setup compilation errors:1]]
(defun my/setup-compilation-errors ()
  (interactive)
  (setq compilation-scroll-output t)
  (setq compilation-error-regexp-alist '())
  (setq compilation-error-regexp-alist-alist '())


  ;; eslint https://github.com/Fuco1/compile-eslint/blob/master/compile-eslint.el
  (when (not compilation-error-regexp-alist-alist)
    (setq compilation-error-regexp-alist-alist '()))

  (let ((form `(eslint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-eslint--find-filename
                2 3 2 1)))

    (if (assq 'eslint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))
  (push 'eslint compilation-error-regexp-alist)



  (add-to-list 'compilation-error-regexp-alist '("^[[:blank:]]*\\([/_-\\.[:alnum:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error.*$" 1 2 3))
  ;; React
  (add-to-list 'compilation-error-regexp-alist '("[[:blank:]]*\\([/_\\.[:alnum:]-]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error.*$" 1 2 3))
  ;; Angular
  (add-to-list 'compilation-error-regexp-alist '("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3))
  ;; Angular vite
  (add-to-list 'compilation-error-regexp-alist '("\\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  ;; Tests
  (add-to-list 'compilation-error-regexp-alist
             '("^Error:[[:blank:]]*\\([[:alnum:]_./-]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3))

  (add-to-list 'compilation-error-regexp-alist '("\\([_[:alnum:]-/.]*\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3))

  (add-to-list 'compilation-error-regexp-alist '("FILE[[:blank:]]*\\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))


  ;; Flutter
  (add-to-list 'compilation-error-regexp-alist 'dart-analyze)
  (add-to-list 'compilation-error-regexp-alist-alist '(dart-analyze "\\([^ ]*\\.dart\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
;; Setup compilation errors:1 ends here

;; [[file:README.org::*Core package][Core package:1]]
(use-package compile
  :defer t
  :ensure nil
  :bind
  (("C-c p c" . project-compile)
   ("C-c a r" . recompile)
   ("C-c C-c C-r" . recompile)
   ("C-c C-c r" . recompile)
   :map compilation-mode-map
   ("C-j" . compilation-next-error)
   ("C-k" . compilation-previous-error)
   :map compilation-minor-mode-map
   ("C-j" . compilation-next-error)
   ("C-k" . compilation-previous-error))
  :custom
  (compile-command "bun run build")
  :config
  
  (defun display-buffer-from-compilation-p (_buffer-name _action)
    (unless current-prefix-arg
      (with-current-buffer (window-buffer)
        (derived-mode-p 'compilation-mode))))
  
  (push '(display-buffer-from-compilation-p
          display-buffer-use-least-recent-window
          (inhibit-same-window . nil))
        display-buffer-alist)
  (my/setup-compilation-errors))
;; Core package:1 ends here

;; [[file:README.org::*Codemetrics][Codemetrics:1]]
(use-package cognitive-complexity
  :hook (prog-mode . cognitive-complexity-mode)
  :ensure (:host github :repo "abougouffa/cognitive-complexity")
  :config
  (cognitive-complexity-mode 1))
;; Codemetrics:1 ends here

;; [[file:README.org::*Debugger DAPE][Debugger DAPE:1]]
(use-package dape
  :ensure (dape :type git :host github :repo "svaante/dape")
  :bind (("C-c d d" . dape)
         ("C-c d n" . dape-next)
         ("C-c d i" . dape-step-in)
         ("C-c d o" . dape-step-out)
         ("C-c d b" . dape-breakpoint-toggle)
         ("C-c d c" . dape-continue)
         ("C-c d q" . dape-disconnect-quit)
         ("C-c d r" . dape-restart)
         ("C-c d k" . dape-kill)
         ("C-c d X" . dape-remove-breakpoint-at-point)
         ("C-c d s" . dape-select-stack)
         ("C-c d t" . dape-select-thread)
         ("C-c d x" . dape-breakpoint-remove-all))
  :config

  (setq dape-buffer-window-arrangment 'right)
  (setq dape-inline-variables t))
;; Debugger DAPE:1 ends here

;; [[file:README.org::*Repl][Repl:1]]
(defun my/open-ipython-repl-here ()
  "Open IPython REPL in current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (run-python (python-shell-calculate-command) t t)))
;; Repl:1 ends here

;; [[file:README.org::*Repl][Repl:2]]
(use-package python
  :ensure nil
  :defer t
  :bind (("C-c r p" . my/open-ipython-repl-here)
         :map inferior-python-mode-map
         ("C-k" . comint-previous-input)
         ("C-j" . comint-next-input))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-level 4)
  :config
  (add-to-list 'display-buffer-alist '("^\\*Python\\*$" . (display-buffer-same-window))))
;; Repl:2 ends here

;; [[file:README.org::*Pipenv][Pipenv:1]]
(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.virtualenvs"))
  

  (add-hook 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  ;; This hook will copy venv from pyvenv to lsp pyright
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                           (setq lsp-pyright-venv-directory pyvenv-virtual-env)
                                           (lsp-restart-workspace)))

  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))
;; Pipenv:1 ends here

;; [[file:README.org::*Pyvenv][Pyvenv:1]]
(use-package pyvenv :defer t)
;; Pyvenv:1 ends here

;; [[file:README.org::*Auto virtualenv][Auto virtualenv:1]]
(use-package auto-virtualenv
  :after pyvenv
  :defer t
  :hook ((python-mode . auto-virtualenv-setup)
         (python-ts-mode . auto-virtualenv-setup)))
;; Auto virtualenv:1 ends here

;; [[file:README.org::*Nix][Nix:1]]
(use-package nix-mode :defer t)
;; Nix:1 ends here

;; [[file:README.org::*Main mode][Main mode:1]]
(use-package elisp-mode
  :defer t
  :ensure nil
  :hook ((emacs-lisp-mode . (lambda () (setq fill-column 80))))
  :bind (("C-c o c" . outline-cycle)
         ("C-c o a" . outline-show-all)
         ("C-c o m" . outline-hide-body)
         ("C-c o ]" . outline-next-heading)
         ("C-c o [" . outline-previous-heading)
         ("C-c o e" . outline-hide-entry)
         ("C-c o n" . outline-toggle-children)
         ("C-c o b" . outline-cycle-buffer)
         ("C-c C-c e" . eval-buffer)))
;; Main mode:1 ends here

;; [[file:README.org::*Package development][Package development:1]]
(use-package package-build :defer t)
(use-package package-lint :defer t)
;; Package development:1 ends here

;; [[file:README.org::*Semantic highlight][Semantic highlight:1]]
(use-package semel
  :ensure (semel :repo "eshelyaron/semel" :host github)
  :hook (emacs-lisp-mode . semel-mode))
;; Semantic highlight:1 ends here

;; [[file:README.org::*Main mode][Main mode:1]]
(use-package clojure-mode
  :defer t)
;; Main mode:1 ends here

;; [[file:README.org::*Repl][Repl:1]]
(use-package cider
  :hook (clojure-mode . cider-mode)
  :defer t)
;; Repl:1 ends here

;; [[file:README.org::*Typescript package][Typescript package:1]]
(use-package typescript-mode
  :defer t  
  :mode ("\\.mts\\'" . typescript-ts-mode)
  :hook (typescript-mode . (lambda () (setq-local fill-column 120)))
  :custom
  (lsp-clients-typescript-server-args '("--stdio"))
  (typescript-indent-level 2)
  :config
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log"))
;; Typescript package:1 ends here

;; [[file:README.org::*Angular][Angular:1]]
(use-package ng2-mode
  :after (typescript-mode lsp-mode)
  :defer t)
;; Angular:1 ends here

;; [[file:README.org::*Main mode][Main mode:1]]
(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode)
  :custom
  (js-indent-level 2))
;; Main mode:1 ends here

;; [[file:README.org::*PKG. NPM Script runner][PKG. NPM Script runner:1]]
(use-package pkg-run
  :ensure (:host github :repo "Artawower/pkg-run")
  :bind (("C-c j r" . pkg-run-script)
         ("C-c j m" . pkg-run-menu)
         ("C-c s r" . pkg-run-script))
  :custom
  (pkg-run-package-manager 'bun)
  :defer t)
;; PKG. NPM Script runner:1 ends here

;; [[file:README.org::*NodeJs repl][NodeJs repl:1]]
(use-package nodejs-repl
  :ensure t
  :bind
  (("C-c r n" . nodejs-repl)
   :map nodejs-repl-mode-map
   ("C-j" . comint-next-input)
   ("C-k" . comint-previous-input))
  :defer t
  :config
  (add-to-list 'display-buffer-alist '("^\\*nodejs\\*$" . (display-buffer-same-window))))
;; NodeJs repl:1 ends here

;; [[file:README.org::*WEB mode development][WEB mode development:1]]
(use-package web-mode
  :defer t
  :mode
  ("\\.tsx\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.astro\\'" . web-mode)
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))
;; WEB mode development:1 ends here

;; [[file:README.org::*Vue][Vue:1]]
(use-package vue-ts-mode
    :ensure (:host github :repo "8uff3r/vue-ts-mode")
    :mode ("\\.vue\\'" . vue-ts-mode))
;; Vue:1 ends here

;; [[file:README.org::*PUG][PUG:1]]
(use-package pug-mode :defer t)
;; PUG:1 ends here

;; [[file:README.org::*Emmet. Quick tag expanding][Emmet. Quick tag expanding:1]]
(use-package emmet-mode
  :defer t
  :hook
  (scss-mode . emmet-mode)
  (css-mode . emmet-mode)
  (ng2-html-mode . emmet-mode)
  (html-mode . emmet-mode)
  (vue-mode . emmet-mode)
  (vue-ts-mode . emmet-mode)
  :bind (:map emmet-mode-keymap
              ("s-e" . emmet-expand-line)
              ("C-j" . nil)))
;; Emmet. Quick tag expanding:1 ends here

;; [[file:README.org::*JQ, json query tool][JQ, json query tool:1]]
(use-package jq-mode :defer t)
;; JQ, json query tool:1 ends here

;; [[file:README.org::*Golang][Golang:1]]
(use-package go-playground
  :defer t)
;; Golang:1 ends here

;; [[file:README.org::*Rust][Rust:1]]
(use-package rustic
  :defer t
  :bind (:map rustic-mode-map
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-format-on-save t)
  (rustic-format-display-method 'ignore))
;; Rust:1 ends here

;; [[file:README.org::*Dart][Dart:1]]
(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))
;; Dart:1 ends here

;; [[file:README.org::*Flutter mode][Flutter mode:1]]
(use-package flutter
  :after dart-mode
  :bind
  (("C-c m f s" . flutter-run)
   ("C-c m f R" . flutter-hot-restart)
   ("C-c m f r" . flutter-run-or-hot-reload)
   :map dart-mode-map
   ("C-c C-r" . flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path (or (getenv "FLUTTER_ROOT") "/Applications/flutter/")))
;; Flutter mode:1 ends here

;; [[file:README.org::*Docker compose][Docker compose:1]]
(use-package docker-compose-mode
  :defer t)
;; Docker compose:1 ends here

;; [[file:README.org::*Docker][Docker:1]]
(use-package docker
  :defer t
  :bind ("C-c o d" . docker))
;; Docker:1 ends here

;; [[file:README.org::*Jenkins][Jenkins:1]]
(use-package jenkinsfile-mode
  :defer t)
;; Jenkins:1 ends here

;; [[file:README.org::*K8S][K8S:1]]
(use-package kubernetes
  :defer t
  :commands (kubernetes-overview)
  :bind (("C-c o K" . kubernetes-overview))
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

(use-package k8s-mode
  :defer t)
;; K8S:1 ends here

;; [[file:README.org::*NGINX][NGINX:1]]
(use-package nginx-mode
  :defer t)
;; NGINX:1 ends here

;; [[file:README.org::*Jinja][Jinja:1]]
(use-package jinja2-mode
  :defer t)
;; Jinja:1 ends here

;; [[file:README.org::*Outline][Outline:1]]
(defun my/markdown-outline-setup ()
  (setq-local outline-regexp "^\\(#{1,6}\\)\\s-+")
  (setq-local outline-level (lambda () (looking-at outline-regexp) (length (match-string 1))))
  (outline-minor-mode 1))
(add-hook 'markdown-ts-mode-hook #'my/markdown-outline-setup)
;; Outline:1 ends here

;; [[file:README.org::*Realtime preview][Realtime preview:1]]
(use-package grip-mode
  :defer t
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (when credential
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))
;; Realtime preview:1 ends here

;; [[file:README.org::*Xwidget preview][Xwidget preview:1]]
(use-package markdown-xwidget
  :after markdown-mode
  :ensure (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "light")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))
;; Xwidget preview:1 ends here

;; [[file:README.org::*Kdl mode][Kdl mode:1]]
(use-package kdl-mode :defer t)
;; Kdl mode:1 ends here

;; [[file:README.org::*Fish][Fish:1]]
(use-package fish-mode :defer t)
;; Fish:1 ends here

;; [[file:README.org::*Functions][Functions:1]]
(defun my/open-last-view-diff ()
  "Open latest view diff from eacs"
  (interactive)
  (let* ((win   (selected-window))
         (pt    (point))
         (start (window-start win)))
    (unwind-protect
        (progn
          (save-restriction
            (widen))
          (goto-char (point-max))
          (unless (re-search-backward "view diff" nil t)
            (user-error "No 'view diff' found in buffer"))
          (goto-char (match-beginning 0))
          (eca-chat--key-pressed-return))
      (when (window-live-p win)
        (set-window-start win start t)
        (set-window-point win pt)))))
;; Functions:1 ends here

;; [[file:README.org::*Package][Package:1]]
(use-package eca
  :defer t
  :ensure (:type git :host github :repo "editor-code-assistant/eca-emacs" :files (:defaults "*.el"))
  :custom
  (eca-chat-use-side-window nil)
  :bind (("C-c e e" . eca)
         ("C-c e m" . eca-transient-menu)
         :map eca-chat-mode-map
         ("<tab>" . eca-chat-toggle-expandable-block)
         ("C-c C-v" . my/open-last-view-diff)
         ("s-<return>" . eca-chat--key-pressed-return)
         ("<return>" . eca-chat--key-pressed-newline)
         ("RET" . eca-chat--key-pressed-newline)
         ("C-k" . eca-chat--key-pressed-previous-prompt-history)
         ("C-j" . eca-chat--key-pressed-next-prompt-history)
         ("C-S-j" . eca-chat-go-to-next-expandable-block)
         ("C-S-k" . eca-chat-go-to-prev-expandable-block))
         ;; ("C-p" . eca-chat-go-to-prev-user-message)
         ;; ("C-n" . eca-chat-go-to-next-user-message))
  :config
  (add-to-list
   'display-buffer-alist
   '("<eca-chat.*"
     (my/smart-display-buffer-function))))
;; Package:1 ends here

;; [[file:README.org::*Org GPG encryption][Org GPG encryption:1]]
(use-package org-crypt
  :defer t
  :ensure nil
  :custom
  (org-crypt-key nil)
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  :config
  (org-crypt-use-before-save-magic))
;; Org GPG encryption:1 ends here

;; [[file:README.org::*Org GPG encryption][Org GPG encryption:2]]
(use-package epa
  :defer t
  :ensure nil
  :custom 
  (epg-pinentry-mode 'loopback)
  (epa-file-encrypt-to my/user-email)
  :config
  (epa-file-enable))
;; Org GPG encryption:2 ends here

;; [[file:README.org::*async code][async code:1]]
(use-package ob-async
  :defer t
  :custom
  (ob-async-no-async-languages-alist '("ipython")))
;; async code:1 ends here

;; [[file:README.org::*HTTP requests][HTTP requests:1]]
(use-package restclient
  :defer t)
;; HTTP requests:1 ends here

;; [[file:README.org::*HTTP requests][HTTP requests:2]]
(use-package ob-restclient
  :defer t)
;; HTTP requests:2 ends here

;; [[file:README.org::*Dart][Dart:1]]
(use-package ob-dart
  :defer t
  :config
  (add-to-list 'org-babel-load-languages  '(dart . t)))
;; Dart:1 ends here

;; [[file:README.org::*Typescript][Typescript:1]]
(use-package ob-typescript
  :defer t)
;; Typescript:1 ends here

;; [[file:README.org::*Golang][Golang:1]]
(use-package ob-go :defer t)
;; Golang:1 ends here

;; [[file:README.org::*Rust][Rust:1]]
(use-package ob-rust :defer t)
;; Rust:1 ends here

;; [[file:README.org::*Faces][Faces:1]]
(defun my/setup-org-mode-faces ()
  "Setup faces for org mode"
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 2.5))))))
;; Faces:1 ends here

;; [[file:README.org::*Copy src block][Copy src block:1]]
(defun my/copy-src-block ()
  "Copy org src block contents (without #+begin/#+end) to clipboard. Handles Emacs 30+ deferred values."
  (interactive)
  (let ((src-block (org-element-at-point)))
    (when (eq (car src-block) 'src-block)
      (let ((val (org-element-property :value src-block)))
        (kill-new val)
        (message "Source block copied. %s" val)))))
;; Copy src block:1 ends here

;; [[file:README.org::*Tangle block under cursor][Tangle block under cursor:1]]
(defun my/org-babel-tangle-current-block ()
  "Tangle only the org-babel source block at point, if it has a :tangle header.
Automatically creates parent directories if needed."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'light))
         (lang (nth 0 info))
         (params (nth 2 info))
         (tangle (cdr (assoc :tangle params)))
         (body (nth 1 info)))
    (unless (and info tangle (not (string= tangle "no")))
      (user-error "No :tangle file specified for this block"))
    (let* ((file (expand-file-name tangle (file-name-directory (or (buffer-file-name) default-directory))))
           (dir (file-name-directory file))
           (tangled-code (org-babel-expand-body:generic body params)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (with-temp-file file
        (insert tangled-code))
      (message "Tangled block to %s" file))))
;; Tangle block under cursor:1 ends here

;; [[file:README.org::*Init org ligatures][Init org ligatures:1]]
(defun my/init-org-ligatures ()
  "Initialize org mode ligatures."
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
  (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
  (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
  (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
  (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
  (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
  (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
  (push '("#+end_src" . "⇤" ) prettify-symbols-alist)

  (push '("#+TITLE:" . "") prettify-symbols-alist)
  (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
  (push '("#+LANG:" . "") prettify-symbols-alist)
  (push '("#+ID:" . "") prettify-symbols-alist)
  (push '("#+FILETAGS:" . "") prettify-symbols-alist)
  (push '("#+STARTUP:" . "") prettify-symbols-alist)
  (push '("#+ACTIVE:" . "") prettify-symbols-alist)
  (push '("#+START_SPOILER" . "") prettify-symbols-alist)
  (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
  (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
  (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
  (prettify-symbols-mode))
;; Init org ligatures:1 ends here

;; [[file:README.org::*Org core package][Org core package:1]]
(use-package org
  :mode (("\\.org$" . org-mode))
  :after meow
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (corfu-mode -1)))
  (org-mode . (lambda () (setq truncate-lines nil)))
  (org-mode . my/init-org-ligatures)
  :bind
  (:map org-mode-map
        ("M-t" . org-todo)
        ("C-c l s" . org-store-link)
        ("M-l l" . org-insert-link)
        ("C-c l l" . org-insert-link)
        ("C-c d t" . org-time-stamp-inactive)
        ("M-l t" . org-toggle-link-display)
        ("C-c l d" . org-toggle-link-display)
        ("C-c t b" . my/org-babel-tangle-current-block)
        ("C-c C-b" . nil)
        ("C-c b ]" . org-babel-next-src-block)
        ("C-c b [" . org-babel-previous-src-block)
        ("C-c d t" . org-time-stamp-inactive)
        ("C-c s t" . org-set-tags-command)
        ("C-c x" . org-toggle-checkbox)
        ("C-c h ]" . org-next-visible-heading)
        ("C-c h [" . org-previous-visible-heading)
        ("C-h ]" . org-next-visible-heading)
        ("C-h [" . org-previous-visible-heading)
        ("C-h n" . org-babel-next-src-block)
        ("C-h p" . org-babel-previous-src-block)
        :map meow-normal-state-keymap
        ("\\o" . org-mode)
        :map org-read-date-minibuffer-local-map
        ("C-s" . org-goto-calendar)
        :map calendar-mode-map
        ("<return>" . org-calendar-select))
  :custom
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-latex-create-formula-image-program 'dvisvgm)
  (org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; https://www.reddit.com/r/orgmode/comments/jwf7ya/python_org_code_blocks_and_indentation/
  (org-adapt-indentation nil)
  (org-imenu-depth 8)
  (org-src-window-setup 'current-window)
  (org-todo-keywords
   '((sequence
      "TODO(t)"     ; A task that needs doing & is ready to do
      "PROJ(p)"     ; A project, which usually contains other tasks
      "IDEA(i)"     ; Idea
      "PROGRESS(s)" ; A task that is in progress
      "WAIT(w)"     ; Something external is holding up this task
      "TEST(c)"     ; In TEST statement
      "BLOCK(b)"    ; task blocked
      "REJECTED(x)" ; somebody rejected idea :(
      "FEEDBACK(f)" ; Feedback required
      "REVIEW(r)"   ; Somebody reviewed your feature
      "HOLD(h)"     ; This task is paused/on hold because of me
      "|"
      "DONE(d)"     ; Task successfully completed
      "KILL(k)")    ; Task was cancelled, aborted or is no longer applicable
     (sequence
      "[ ](T)"      ; A task that needs doing
      "[-](S)"      ; Task is in progress
      "[?](W)"      ; Task is being held up or paused
      "|"
      "[X](D)"))    ; Task was completed
   org-todo-keyword-faces
   '(("PROGRESS"   . org-warning)
     ("DONE"       . org-done)
     ("IDEA"       . org-todo)
     ("WAIT"       . org-todo)
     ("TEST"       . org-warning)
     ("FEEDBACK"   . org-todo)
     ("REVIEW"     . org-todo)
     ("HOLD"       . org-todo)
     ("PROJ"       . org-level-1)
     ("BLOCK"      . org-done)
     ("REJECTED"   . org-done)
     ("KILL"       . org-done)))

  (org-highest-priority ?A)
  (org-default-priority ?C)
  (org-lowest-priority ?E)
  (org-hide-emphasis-markers t)
  (org-use-property-inheritance t)
  :config
  (my/setup-org-mode-faces)
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu")))

  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))


  (setenv "NODE_PATH" "/opt/homebrew/lib/node_modules")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (typescript . t)
     (js . t)
     (python . t)
     (restclient . t)
     (shell . t)))

  (defun org-babel-execute:typescript (body params)
    (let ((org-babel-js-cmd "npx ts-node < "))
      (org-babel-execute:js body params))))
;; Org core package:1 ends here

;; [[file:README.org::*Org menu][Org menu:1]]
(use-package org-menu
  :defer t
  :ensure (:host github :repo "sheijk/org-menu")
  :bind (("C-c o a" . org-menu)))
;; Org menu:1 ends here

;; [[file:README.org::*Prettify org priority][Prettify org priority:1]]
(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '((?A . "🔥")
                                    (?B . "⬆")
                                    (?C . "❗")
                                    (?D . "⬇")
                                    (?E . "❓")
                                    (?1 . "🔥")
                                    (?2 . "⚡")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))
;; Prettify org priority:1 ends here

;; [[file:README.org::*Pretty org stars][Pretty org stars:1]]
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))
;; Pretty org stars:1 ends here

;; [[file:README.org::*Org node. Org roam alternative][Org node. Org roam alternative:1]]
(use-package org-mem
  :config
  (setq org-mem-do-sync-with-org-id t)
  (setq org-mem-watch-dirs
        (list my/org-roam-dir)) 
  (org-mem-updater-mode))

(use-package org-node
  :after org-mem
  :bind
  (("C-c n r f" . org-node-find)
   ("C-c n r i" . org-node-insert-link))
  :custom
  (org-node-alter-candidates t)
  (org-node-creation-fn #'org-node-new-via-roam-capture)
  (org-node-file-slug-fn #'org-node-slugify-like-roam-default)
  (org-node-file-timestamp-format "%Y%m%d%H%M%S-")
  :config
  (org-node-cache-mode)
  (org-node-roam-accelerator-mode)
  (setq org-node-affixation-fn
        (defun my/org-node-prefix-with-tag (node title)
          "Let NODE's tags prefix TITLE."
          (list title
                (when-let ((tags (org-node-get-tags node)))
                  (propertize (concat "(" (string-join tags ", ") ") ")
                              'face `(:foreground ,my/org-tag-color :weight bold :slant italic)))
                nil))))
;; Org node. Org roam alternative:1 ends here

;; [[file:README.org::*Image inserting to org documents][Image inserting to org documents:1]]
(use-package org-yt
  :defer t
  :ensure (:host github :repo "TobiasZawada/org-yt")
  :config
  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))
;; Image inserting to org documents:1 ends here

;; [[file:README.org::*Orgnote. Roam publisher for second brain project][Orgnote. Roam publisher for second brain project:1]]
(use-package orgnote
  :ensure (:host github :repo "Artawower/orgnote.el")
  :bind
  (("C-c n p" . orgnote-publish-file)
   ("C-c n F" . orgnote-force-sync)
   ("C-c n S" . orgnote-sync)
   :map org-mode-map
   ("C-c n P" . orgnote-publish-all)
   ("C-c n L" . orgnote-load))
  :custom
  (orgnote-debug-p t)
  (orgnote-execution-script "node /Users/darkawower/projects/pet/orgnote/orgnote-cli/dist/index.js"))
;; Orgnote. Roam publisher for second brain project:1 ends here

;; [[file:README.org::*Table of contents][Table of contents:1]]
(use-package org-make-toc
  :bind (:map org-mode-map
              ("C-c o g" . org-make-toc)))
;; Table of contents:1 ends here

;; [[file:README.org::*Org exporters][Org exporters:1]]
(use-package ox-gfm
  :defer t
  :ensure (ox-gfm :type git :host github :repo "larstvei/ox-gfm"))
;; Org exporters:1 ends here

;; [[file:README.org::*Better enter handling][Better enter handling:1]]
(use-package org-smart-enter
  :ensure (org-smart-enter :type git :host github :repo "artawower/org-smart-enter.el")
  :hook (org-mode . org-smart-enter-mode)
  :config
  (org-smart-enter-setup))
;; Better enter handling:1 ends here

;; [[file:README.org::*Org raindrop][Org raindrop:1]]
(use-package raindrop
  :bind (("C-c r s" . raindrop-search))
  :ensure (raindrop :host github :repo "artawower/raindrop.el"))
;; Org raindrop:1 ends here

;; [[file:README.org::*Org raindrop][Org raindrop:2]]
(use-package raindrop-search
  :ensure nil
  :after raindrop)
;; Org raindrop:2 ends here

;; [[file:README.org::*Org raindrop][Org raindrop:3]]
(use-package ob-raindrop
  :commands (org-babel-execute:raindrop)
  :ensure nil
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(raindrop . t) t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)))
;; Org raindrop:3 ends here

;; [[file:README.org::*Google translate][Google translate:1]]
(use-package google-translate
  :defer 10
  :bind (:map google-translate-minibuffer-keymap
              ("C-'" . google-translate-next-translation-direction)
              :map meow-normal-state-keymap
              ("\\ t" . google-translate-smooth-translate))
  :custom
  (google-translate-backend-method 'curl)
  (google-translate-pop-up-buffer-set-focus t)
  (google-translate-translation-directions-alist
   '(("en" . "ru") ("ru" . "en") ))

  :config
  (require 'google-translate-smooth-ui)
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))
;; Google translate:1 ends here

;; [[file:README.org::*Jinx. Spellchecker.][Jinx. Spellchecker.:1]]
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US ru_RU")
  (jinx-camel-modes '(prog-mode org-mode))
  :bind (("C-c s c" . jinx-correct)
         ("C-c s l" . jinx-languages)
         ("C-c s ]" . jinx-next)
         ("C-c s [" . jinx-previous))
  :config
  (add-to-list 'jinx-camel-modes 'html-ts-mode)
  (add-to-list 'jinx-camel-modes 'html-mode)
  (add-to-list 'jinx-camel-modes 'typescript-ts-mode)
  ;; Add function and variable names to jinx faces in prog-mode
  (cl-callf cl-union
      (alist-get 'prog-mode jinx-include-faces)
    '(font-lock-function-name-face
      font-lock-variable-name-face)
    :test #'eq))
;; Jinx. Spellchecker.:1 ends here

;; [[file:README.org::*Tramp][Tramp:1]]
(use-package tramp
  :ensure nil
  :custom
  (remote-file-name-inhibit-cache nil)
  (tramp-verbose 1)
  :config
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:user@host:")
                     "remote-shell" "/usr/bin/bash"))
  (add-to-list 'tramp-connection-properties
               (list ".*" "locale" "LC_ALL=C")))
;; Tramp:1 ends here

;; [[file:README.org::*Pdf tools][Pdf tools:1]]
(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install))
;; Pdf tools:1 ends here

;; [[file:README.org::*Settings][Settings:1]]
(add-to-list 'display-buffer-alist '("^\\*scratch\\*$" (display-buffer-below-selected) (window-height . 0.4)))
(add-to-list 'display-buffer-alist '("^\\*quicknote\\*$"
                                     (display-buffer-in-side-window)
                                     (inhibit-same-window . t)
                                     (side . bottom)
                                     (window-width . full)
                                     (window-height . 0.3)))
;; Settings:1 ends here

;; [[file:README.org::*Switch to scratch buffer][Switch to scratch buffer:1]]
(defun my/switch-to-scratch ()
  "Switch to scratch buffer"
  (interactive)
  (let* ((buffer-name "*quicknote*")
         (buffer (get-buffer buffer-name)))

    (unless buffer
      (persistent-scratch-setup-default)
      (setq buffer (get-buffer-create buffer-name))
      (persistent-scratch-restore))
    
    (message "current buffer: %s" buffer)
    (with-current-buffer buffer
      (pop-to-buffer buffer)
      (org-mode)
      (meow-insert))))
;; Switch to scratch buffer:1 ends here

;; [[file:README.org::*Persistent scratch][Persistent scratch:1]]
(defun create-persistent-scratch-file-if-not-exist ()
  "Create the persistent-scratch file if it does not already exist."
  (let ((scratch-file (expand-file-name "var/persistent-scratch.el" user-emacs-directory)))
    (unless (file-exists-p scratch-file)
      (make-directory (file-name-directory scratch-file) t)
      (with-temp-file scratch-file
        (insert ";; Persistent scratch file\n")))))

;; Run the function during startup
(create-persistent-scratch-file-if-not-exist)
;; Persistent scratch:1 ends here

;; [[file:README.org::*Persistent scratch][Persistent scratch:2]]
(defun my/persistent-scratch-savable-p ()
  "Return non-nil if the current buffer is savable."
  (when-let* ((current-buffer-scratch-p (string= (buffer-name) "*quicknote*"))
              (scratch-buffer (get-buffer "*quicknote*"))
              (scratch-buffer-content (with-current-buffer scratch-buffer
                                        (buffer-string)))
              (scratch-buffer-content-not-empty (not (equal scratch-buffer-content ""))))
    t))

(defun my/save-scratch-buffer-p ()
  "Return non-nil if the current buffer is the scratch buffer."
  (my/persistent-scratch-savable-p))

(setq persistent-scratch-scratch-buffer-p-function #'my/save-scratch-buffer-p)
;; Persistent scratch:2 ends here

;; [[file:README.org::*Persistent scratch][Persistent scratch:3]]
(defun my/preserve-persistent-scratch ()
  "Preserve the persistent scratch buffer. Make backup."
  (interactive)
  (persistent-scratch-save)
  (persistent-scratch-new-backup))
;; Persistent scratch:3 ends here

;; [[file:README.org::*Persistent scratch][Persistent scratch:5]]
(use-package persistent-scratch
  :demand t
  :bind (("C-c n n" . my/switch-to-scratch))
  :config
  (defun persistent-scratch-save (&optional file)
    "Save the current state of scratch buffers.
  When FILE is non-nil, the state is saved to FILE; when nil or when called
  interactively, the state is saved to `persistent-scratch-save-file'.
  What state exactly is saved is determined by `persistent-scratch-what-to-save'.
  
  When FILE is nil and `persistent-scratch-backup-directory' is non-nil, a copy of
  `persistent-scratch-save-file' is stored in that directory, with a name
  representing the time of the last `persistent-scratch-new-backup' call."
    (interactive)
    (let* ((actual-file (or file persistent-scratch-save-file))
           (tmp-file (concat actual-file ".new"))
           (saved-state (persistent-scratch--save-buffers-state)))
      (when (car saved-state)
        (let ((old-umask (default-file-modes)))
          (set-default-file-modes #o600)
          (unwind-protect
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (cdr saved-state) nil tmp-file nil 0))
            (set-default-file-modes old-umask)))
        (run-hook-with-args 'persistent-scratch-before-save-commit-functions tmp-file)
        (rename-file tmp-file actual-file t)
        (dolist (buffer (car saved-state))
          (with-current-buffer buffer
            (set-buffer-modified-p nil)))
        (when (called-interactively-p 'interactive)
          (message "Wrote persistent-scratch file %s" actual-file)))
      (unless file
        (persistent-scratch--update-backup)
        (persistent-scratch--cleanup-backups))))
  (setq persistent-scratch-backup-directory "~/apps/pure-emacs/backups")
  (persistent-scratch-autosave-mode)
  (setq persistent-scratch-autosave-interval 5))
;; Persistent scratch:5 ends here

;; [[file:README.org::*Husky][Husky:1]]
(use-package husky
  :defer t
  :ensure (:type git :host github :repo "artawower/husky" :files ("lisp/*.el"))
  :bind (("C-c t t" . husky-edit-insert-todo-by-current-git-branch)
         ("C-c t i" . husky-edit-insert-todo-by-current-git-branch)
         ("C-c t d" . husky-edit-insert-debug-by-current-git-branch)
         ("C-c t n" . husky-edit-insert-note-by-current-git-branch)
         ("C-c o l" . husky-buffers-open-messages)
         ("C-c o M" . husky-buffers-open-clear-messages)
         ("C-c f D" . husky-buffers-delete-this-file)
         ("s-<backspace>" . husky-edit-kill-word-backward)
         ("M-n" . husky-lsp-repeat-consult-search-forward)
         ("M-p" . husky-lsp-repeat-consult-search-backward)
         ("C-c b k i" . husky-buffers-kill-invisible-buffers)
         ("C-c S-SPC" . husky-buffers-side-project-find-file)
         ("C-c b B" . husky-buffers-side-consult-projectile-switch-to-buffer)
         ("C-c >" . husky-buffers-side-find-file)
         :map meow-normal-state-keymap
         ("gd" . husky-lsp-find-definition)
         ("%" . husky-navigation-bounce-paren)
         ("g F" . husky-lsp-avy-go-to-definition)
         ("g f" . husky-lsp-avy-go-to-definition)
         ("g D" . husky-buffers-side-husky-actions-find-definition)
         ("z r" . husky-fold-open)
         ("z R" . husky-fold-open-all)
         ("s-y" . husky-lsp-copy-to-register-1)
         ("s-p" . husky-lsp-paste-from-register-1)
         ("z A" . husky-fold-toggle-all)
         ("z a" . husky-fold-toggle)
         ("z j" . husky-fold-next)
         ("z M" . husky-fold-close-all)
         ("z k" . husky-fold-previous)
         ("\\ m" . husky-window-manager-toggle-maximize-buffer)
         :map meow-motion-state-keymap
         ("\\ m" . husky-window-manager-toggle-maximize-buffer)
         :map magit-status-mode-map
         ("M-n" . husky-lsp-repeat-consult-search-forward)
         ("M-p" . husky-lsp-repeat-consult-search-backward))))
;; Husky:1 ends here

;; [[file:README.org::*End config][End config:1]]
(elpaca-wait)
;; End config:1 ends here
