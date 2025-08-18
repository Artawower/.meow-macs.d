(setenv "LSP_USE_PLISTS" "true")
(setq cnfg/config-dir "~/apps/flat-emacs/")
(setq cnfg/config-path (concat cnfg/config-dir "README.org"))
(setq cnfg/backup-dir "/Users/darkawower/tmp/emacs-backups")
(setq cnfg/private-config-path "~/apps/pure-emacs/private.el")
(setq @doom-theme-directory "~/.doom.d/themes")

(setq @m-color-main "#61AFEF"
      @m-color-secondary "#FF3399"
      @m-color-yellow "#FFAA00"
      @m-color-blue "#00AEE8"
      @m-color-cyan "#00CED1"
      @m-color-green "#00D364")

(let* ((normal-gc-cons-threshold (* 20 1024 1024))
     (init-gc-cons-threshold (* 128 1024 1024)))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024)))))

(setq read-process-output-max (* 1024 1024))

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
;; (setq elpaca-lock-file "/Users/darkawower/apps/flat-emacs/elpaca.lock")

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(require 'package)

(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "http://melpa.org/packages/")
                          ("melpa" . "http://melpa.milkbox.net/packages/")
                          ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ))
(customize-set-variable 'package-enable-at-startup nil)
;; (package-initialize)

(use-package compile-angel
  :ensure t
  :demand t
  :config
  (setq compile-angel-verbose t)
  (compile-angel-on-load-mode))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > ~/.emacs.d/.local/env'
(defconst my-local-dir (concat user-emacs-directory ".local/"))

(defconst my-env-file (concat my-local-dir "env"))

(defun my-load-envvars-file (file &optional noerror)
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
           (file-exists-p my-env-file))
  (my-load-envvars-file my-env-file))
;;; Code to replace exec-path-from-shell

(when cnfg/backup-dir
  (setq backup-directory-alist `(("." . ,cnfg/backup-dir))))
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(setq warning-minimum-level :emergency)
(setq warning-suppress-log-types '((comp) (undo discard-info)))

(use-package no-littering
  :demand t
  :config
  (when cnfg/backup-dir
    (setq backup-directory-alist `(("." . ,cnfg/backup-dir))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-by-copying t))

(ignore-errors
  (when cnfg/private-config-path
    (load cnfg/private-config-path))
  (elpaca-wait))

(when (eq system-type 'darwin)
  (setq browse-url-firefox-program nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(defvar @youtrack-url "https://verifika.youtrack.cloud/issue")
(defconst @youtrack-url-regexp "VW-[[:digit:]]*")

(defun @open-current-task-youtrack-url ()
  "Open current task in YouTrack"
  (interactive)
  (when-let* ((branch-name (magit-get-current-branch))
              (matched-res (string-match @youtrack-url-regexp branch-name))
              (task-name (substring branch-name (match-beginning 0) (match-end 0))))

    (browse-url (concat @youtrack-url "/" task-name))))

(defun @open-emacs-config ()
  "Open folder with emacs config"
  (interactive)
  (let ((default-directory cnfg/config-path))
    (call-interactively 'find-file)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun my/copy-region-or-buffer-as-markdown ()
  "Copy region (if active) or entire buffer as a Markdown code block with file path relative to project root."
  (interactive)
  (let* ((current-buf (current-buffer))
         (proj (project-current t))
         (root (if proj
                   (file-name-as-directory (project-root proj))
                 default-directory))
         (file (or (buffer-file-name current-buf) ""))
         (rel-path
          (if (and file (not (string= file "")))
              (file-relative-name file root)
            (read-string "Enter relative path for non-file buffer: ")))
         (current-major-mode (with-current-buffer current-buf major-mode))
         (lang
          (pcase current-major-mode
            ('emacs-lisp-mode "elisp")
            ('lisp-mode       "lisp")
            ('python-mode     "python")
            ('python-ts-mode  "python")
            ('ruby-mode       "ruby")
            ('js-mode         "javascript")
            ('js2-mode        "javascript")
            ('typescript-mode "typescript")
            ('typescript-ts-mode "typescript")
            ('c-mode          "c")
            ('c++-mode        "cpp")
            ('java-mode       "java")
            ('go-mode         "go")
            ('rust-mode       "rust")
            ('rust-ts-mode    "rust")
            ('php-mode        "php")
            ('shell-script-mode "bash")
            ('sh-mode         "bash")
            ('css-mode        "css")
            ('html-mode       "html")
            ('xml-mode        "xml")
            ('json-mode       "json")
            ('yaml-mode       "yaml")
            ('markdown-mode   "markdown")
            ('org-mode        "org")
            (_                (let ((mode-name (symbol-name current-major-mode)))
                                (if (string-match "\\(.*\\)-mode$" mode-name)
                                    (match-string 1 mode-name)
                                  (read-string "Enter language for code block: "))))))
         (code
          (with-current-buffer current-buf
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (buffer-substring-no-properties (point-min) (point-max)))))
         (output
          (string-join
           (list rel-path
                 (concat "```" lang)
                 code
                 "```")
           "\n")))
    (kill-new output)
    (message "Copied as Markdown code block: %s (%s)" rel-path lang)))

(global-auto-revert-mode 1)
(setq auto-revert-use-notify nil)

(use-package persistent-values
  :defer t
  :bind (("s-S" . persistent-values-add)
         ("s-I" . persistent-values-search))
  :ensure (persistent-values :host github :repo "artawower/persistent-values.el")
  :config
  (persistent-values-mode 1))

(use-package expand-region
  :bind (("s-x" . er/expand-region))
  :defer t)

(use-package narrow-indirect
  :ensure (:host github :repo "emacsmirror/narrow-indirect")
  :bind (("C-c 0" . ni-narrow-to-region-indirect-other-window))
  :defer t)

(use-package undo-fu
  :defer t
  :bind (("C-r" . undo-fu-only-redo)
         :map meow-normal-state-keymap
              ("u" . undo-fu-only-undo)))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

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

(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))

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
  ;; (turbo-log-payload-format-template "")
  ;; (turbo-log-payload-format-template "\x1b[35m%s: ")
  (turbo-log-payload-format-template "%s: ")
  :config
  (setq turbo-log-allow-insert-without-treesit-p t)
  (setq turbo-log-allow-insert-without-tree-sitter-p t)
  (turbo-log-configure
   :modes (typescript-ts-mode tsx-ts-mode typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (apheleia-format-buffer lsp)
   ;; :msg-format-template "'🦄: %s'"
   :msg-format-template "'✎: %s'"))

(use-package auto-rename-tag
  :defer t)

(use-package string-inflection
  :defer t
  :bind
  (("C-s-c" . string-inflection-all-cycle)))

(use-package persistent-soft
  :defer t)

(use-package persistent-kmacro
  :ensure (:host github :repo "artawower/persistent-kmacro.el")
  :after meow
  :defer t
  :bind (("C-c m e" . persistent-kmacro-execute-macro)
         ("C-c m a" . persistent-kmacro-name-last-kbd-macro)
         ("C-c m r" . persistent-kmacro-remove-macro)
         :map meow-normal-state-keymap
         ("#" . persistent-kmacro-apply))
  :config
  (setq persistent-kmacro-macro-file "persistent-kmacro-store.el"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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
  ;; TODO: main need to get all from directory.
  (setq tempel-auto-reload nil)
  (setq tempel-path
        '("~/apps/flat-emacs/templates/common"
          "~/apps/flat-emacs/templates/readme"
          "~/apps/flat-emacs/templates/typescript"
          "~/apps/flat-emacs/templates/org-mode"
          "~/apps/flat-emacs/templates/web-mode"
          "~/apps/flat-emacs/templates/contribution-guide"
          "~/apps/flat-emacs/templates/angular"
          "~/apps/flat-emacs/templates/emacs-lisp"
          "~/apps/flat-emacs/templates/golang")))

(defun @treesit-ts-func ()
  "Return a string of breadcrumbs."
  (let ((node (treesit-node-at (point) 'typescript))
        result)
    (while (and node (not result))
      (when (string= "function_declaration" (treesit-node-type node))
        (setq result (treesit-node-text (treesit-node-child-by-field-name node "name"))))
      (setq node (treesit-node-parent node)))
    result))

(use-package apheleia
  :hook (prog-mode . apheleia-global-mode)
  :bind (:map meow-normal-state-keymap
           ("\\p" . apheleia-format-buffer))
  :config
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(html-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier)))

(show-paren-mode 1)
(custom-set-faces
 `(show-paren-mismatch ((t (:foreground ,\@m-color-secondary)))))

(use-package combobulate
  :after treesit
  :ensure (:host github :repo "mickeynp/combobulate")
  ;; :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  :bind (("s-q" . combobulate-mark-node-dwim)
         ("s-<down>" . combobulate-navigate-logical-next)
         ("s-<up>" . combobulate-navigate-logical-previous)
         ("s-N" . combobulate-navigate-next)
         ("s-P" . combobulate-navigate-previous)
         :map combobulate-key-map
         ("C-M-t" . nil))
  ;; :hook
  ;; ((python-ts-mode . combobulate-mode)
  ;;  (js-ts-mode . combobulate-mode)
  ;;  (html-ts-mode . combobulate-mode)
  ;;  (css-ts-mode . combobulate-mode)
  ;;  (yaml-ts-mode . combobulate-mode)
  ;;  (typescript-ts-mode . combobulate-mode)
  ;;  (json-ts-mode . combobulate-mode)
  ;;  (tsx-ts-mode . combobulate-mode))
  :config
  (setq combobulate-key-prefix "C-c c o")

  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  )

(use-package treesit
  :ensure nil
  :hook
  (go-ts-mode . (lambda () (setq-local tab-width 2)))
  ;; (markdown-ts-mode . #'my/markdown-outline-setup)
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode))
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
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
          ))

  (setq treesit-font-lock-level 3))

(use-package treesit-auto
  :ensure (treesit-auto
           :host github
           :repo "noctuid/treesit-auto"
           :branch "bind-around-set-auto-mode-0")
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; (global-treesit-auto-mode)


  (setq my-js-tsauto-config
        (make-treesit-auto-recipe
         :lang 'scss-mode
         :ts-mode 'scss-ts-mode
         :remap '(scss-mode)
         :url "https://github.com/serenadeai/tree-sitter-scss"
         :revision "master"
         :source-dir "src"
         :ext "\\.scss\\'"))

  (add-to-list 'treesit-auto-recipe-list my-js-tsauto-config))

(defun @treesit-fold-init ()
  "Init ts-fold."
  (interactive)
  (when (and (treesit-available-p)
             (member major-mode '(ng2-ts-mode
                                  typescript-mode
                                  js-mode
                                  python-mode
                                  html-mode
                                  json-mode
                                  go-mode
                                  scss-mode
                                  css-mode
                                  typescript-ts-mode
                                  go-ts-mode
                                  bash-ts-mode
                                  python-ts-mode
                                  json-ts-mode
                                  html-ts-mode
                                  scss-ts-mode
                                  css-ts-mode
                                  bash-mode)))
    (treesit-fold-mode 1)))

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
          typescript-ts-mode) . @treesit-fold-init)
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
  ;; (add-to-list 'treesit-fold-range-alist '(go-ts-mode . ts-fold-summary-go))

  ;; TODO: DOESN'T WORK for scss, needs another rules (check it later for custom pareser)
  (add-to-list 'treesit-fold-range-alist '(scss-mode . (css-mode
                                                        (keyframe_block_list . treesit-fold-range-seq)
                                                        (block . treesit-fold-range-seq)
                                                        (comment . treesit-fold-range-c-like-comment)))))


;; TODO: check changes for correct mode cast
;; (add-to-list 'treesit-fold-foldable-node-alist '(ng2-ts-mode comment statement_block export_clause))
;; (add-to-list 'treesit-fold-foldable-node-alist '(web-mode comment element))
;; (add-to-list 'treesit-fold-foldable-node-alist '(scss-mode comment block keyframe_block_list))
;; (add-to-list 'treesit-fold-foldable-node-alist '(ng2-html-mode comment element)))

(defun @typescript-ts-mode-setup ()
  (setq-local treesit-simple-imenu-settings
              '(("Function" "\\'function_declaration\\'" nil nil)
                ("Function" "\\'identifier\\'" nil nil)
                ("Variable"  "\\'variable_declarator\\'" nil nil)))
  (treesit-major-mode-setup))

(use-package imenu
  :ensure nil)

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
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
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
  ;; (add-hook 'meow-normal-mode-hook (lambda () (corfu-quit) (message "quite")))
  (add-hook 'meow-insert-exit-hook (lambda () (corfu-quit)))
  (setq lsp-completion-provider :none)

  (global-corfu-mode))

(use-package corfu-quick
  :ensure (corfu-quick :host github :repo "minad/corfu" :files ("extensions/corfu-quick.el"))
  :bind (:map corfu-map
              ("<tab>" . corfu-quick-insert))
  :after corfu
  :custom
  (corfu-quick1 "123456789")
  (corfu-quick2 "123456789"))

(use-package corfu-popupinfo
  :ensure (corfu-info :host github :repo "minad/corfu" :files ("extensions/corfu-popupinfo.el"))
  :after corfu
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.5)))

(use-package corfu-history
  :ensure (corfu-history :host github :repo "minad/corfu" :files ("extensions/corfu-history.el"))
  :after corfu
  :config
  (with-eval-after-load 'safehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
   (setq corfu-sort-override-function 'corfu-history--sort)

  (corfu-history-mode))

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

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :bind ("C-c C-e" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package dired
  :defer t
  :ensure nil
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)
              ("\\c" . dired-create-empty-file)
              ("f" . avy-goto-word-1))
  :config
  (setq dired-dwim-target t)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package file-info
  :bind
  (("s-'" . 'file-info-show))
  :defer t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params '(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(use-package reveal-in-osx-finder
  :defer t
  :bind (("C-c o f" . reveal-in-osx-finder))
  :ensure t)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  :config
  (set-face-attribute 'dirvish-hl-line nil :foreground nil :background @m-color-main)
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  ;; (remove-hook 'dired-mode-hook 'treemacs-icons-dired-mode)
  ;; (remove-hook 'dired-after-readin-hook 'treemacs-icons-dired--display)
  ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time collapse subtree-state))
  ;; (setq dirvish-attributes
  ;;       '(file-time collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  ;; (setq dired-listing-switches
  ;;       "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (:map dirvish-mode-map
   ("q" . dirvish-quit)
   ("f" . avy-goto-word-1)
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

(defun @clear-term-history ()
  "Clear terminal history inside vterm."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (vterm--self-insert)
    (vterm-send-string "clear")
    (vterm-send-return)))
  

(advice-add 'vterm-clear-scrollback :before #'@clear-term-history)

(defun @vterm-open-here ()
  "Open vterm inside current buffer!"
  (interactive)
  (let ((nodejs-repl-buffer-name "*vterm*"))
    (if (get-buffer nodejs-repl-buffer-name)
        (switch-to-buffer nodejs-repl-buffer-name)
      (progn
        (pop-to-buffer vterm-buffer-name)
        (vterm)))))

(defun @vterm-open-last ()
  "Open vterm inside current buffer!"
  (interactive)
  (let ((vterm-buffer-name "*vterm*"))
    (if (get-buffer vterm-buffer-name)
        (pop-to-buffer vterm-buffer-name)
      (progn
        (pop-to-buffer vterm-buffer-name)
        (vterm)))))

(defun @vterm-change-current-directory-to-active-buffer-pwd ()
  "Just exec CD to pwd of active buffer."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (file-dir (file-name-directory file-name))
              (file-dir (replace-regexp-in-string " " "\\\\\  " file-dir)))
    ;; (save-window-excursion
      (vterm-toggle-show)
      (switch-to-first-matching-buffer "vterm")
      (vterm-send-C-c)
      (vterm-send-string (concat "cd " file-dir))
      (vterm-send-return)
      ;; )
    ))

(use-package vterm
  :defer t
  :hook ((vterm-mode . (lambda () (set-face-attribute 'vterm-color-black nil :foreground @m-color-secondary :background @m-color-secondary)))
        ;; (vterm-mode . compilation-minor-mode)
)
  :bind (("C-c o t" . vterm-toggle)
         ("C-c o T" . @vterm-change-current-directory-to-active-buffer-pwd)
         ("C-c o v" . vterm)
         ("C-c o V" . @vterm-open-here)
         ("C-c o L" . @vterm-open-last)
         :map vterm-mode-map
         ("C-u" . vterm--self-insert)
         ("C-c m c" . vterm-copy-mode))
  :custom
  (vterm-max-scrollback 5000)
  :config
  (set-face-attribute 'vterm-color-black nil :foreground @m-color-secondary :background @m-color-secondary)
  (setq vterm-clear-scrollback t)
  (defun @clear-term-history ()
    "Clear terminal history inside vterm."
    (interactive)
    (when (eq major-mode 'vterm-mode)
      (vterm--self-insert)
      (vterm-send-string "clear")
      (vterm-send-return)))
    
  
  (advice-add 'vterm-clear-scrollback :before #'@clear-term-history))

(defun @vterm-open-pick-dir ()
(interactive))

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
                       (and (not (string-match-p "^\\*claude" (buffer-name buffer)))
                            (or (eq major-mode 'vterm-mode)
                                (string-prefix-p vterm-buffer-name (buffer-name buffer)))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package detached
  :defer t
  :init
  (detached-init)
  :bind (;; Replace async-shell-command' with `detached-shell-command'
         ;; ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session)
         )
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

(global-set-key (kbd "C-c ]") 'next-buffer)
(global-set-key (kbd "C-c [") 'previous-buffer)

(use-package ibuffer
  :ensure nil
  :bind ("C-c i b" . ibuffer))

(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it. 
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
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
         ("C-c TAB d" . tabspaces-kill-buffers-close-workspace)
         ("C-c TAB S" . tabspaces-save-current-project-session)
         ("C-c TAB s" . tabspaces-save-session)
         ("C-c TAB r" . tab-rename)
         ("C-c TAB l" . tabspaces-restore-session))
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "README.org")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '())
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session nil)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice nil))

(use-package tab-bar
  :defer t
  :bind (("C-c TAB w" . tab-switch)
         ("C-c w w" . tab-switch))
  :ensure nil
  :custom
  (tab-bar-show nil))

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

(defun @edit-after-eol ()
  "Edit the current line to the end of the line like A in the VIM."
  (interactive)
  (meow-line 1)
  (meow-append))

(defun @edit-before-bol ()
  "Edit the current line to the beginning of the line like I in the VIM."
  (interactive)
  (meow-join 0)
  (meow-append))

(defun @meow-yank-below ()
  "Yank below the current line."
  (interactive)
  (forward-line)
  (meow-yank))

(defun @meow-change-till-eol ()
  "Change till the end of line."
  (interactive)
  (let ((meow-eol-thing 108))
    (meow-end-of-thing meow-eol-thing)
    (meow-change)))

(defun @meow-select-till-eol ()
  "Select till the end of line."
  (interactive)
  (let ((meow-eol-thing 108))
    (meow-end-of-thing meow-eol-thing)))

(defun @meow-backward-till (n ch)
  "Move backward till the first character that is not in the list of characters."
  (interactive "p\ncTill:")
  (meow-till -1 ch))

(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (remove-if-not (apply-partially #'string-match-p regex)
                                        (mapcar #'buffer-name (buffer-list))))))

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
             (call-interactively 'meow-search)
             
             ))
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
   '("$" . @meow-select-till-eol)
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
   '("C" . @meow-change-till-eol)
   '("p" . @meow-yank-below)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . @meow-backward-till)
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
   '("A" . @edit-after-eol)
   '("I" . @edit-before-bol)
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

(defun @meow-thing-register ()
  (meow-thing-register 'whitespace '(regexp " \\|\n" " \\|\n") '(regexp " \\|\n" " \\|\n"))
  (add-to-list 'meow-char-thing-table '(?w . whitespace))

  ;; (meow-thing-register 'between-space '(regexp "[^[[:blank:]]]" "[^[[:blank:]]]") '(regexp "[^[[:blank:]]]" "[^[[:blank:]]]"))
  ;; (add-to-list 'meow-char-thing-table '(?e . between-space))

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

(defun @meow-setup-custom-modes ()
  (add-to-list 'meow-mode-state-list '(elpaca-info-mode . normal))
  (add-to-list 'meow-mode-state-list '(flycheck-error-list-mode . normal))
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

(defun @meow-setup-state-per-modes ()
  ;; (add-to-list 'meow-mode-state-list '(magit-mode . normal))
  (add-to-list 'meow-mode-state-list '(magit-process-mode . normal))
  (add-to-list 'meow-mode-state-list '(compilation-mode . normal))
  (add-to-list 'meow-mode-state-list '(detached-compilation-mode-map . normal))
  (add-to-list 'meow-mode-state-list '(messages-buffer-mode . normal))
  (add-to-list 'meow-mode-state-list '(debug-mode . normal))
  (add-to-list 'meow-mode-state-list '(debugger-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(special-mode . normal))
  (add-to-list 'meow-mode-state-list '(grep-mode . normal)))

(use-package meow
  :custom
  (meow-use-clipboard t)
  :config
  (meow-setup)
  (set-face-attribute 'meow-position-highlight-number-1 nil :foreground "#61AFEF" :background "#919185048b0a" :bold t :underline nil)
  (set-face-attribute 'meow-position-highlight-number-2 nil :foreground "#4E90CF" :bold t :underline nil)
  (@meow-thing-register)
  (@meow-setup-custom-modes)
  (@meow-setup-state-per-modes)
  (advice-add #'meow-change :after 
            (lambda ()
              (when (string-match-p "^\\s-*$" (thing-at-point 'line t))
                (indent-for-tab-command))))
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :config
  (meow-tree-sitter-register-defaults))

(use-package meow-vterm
  :ensure (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :after vterm
  :config
  (setq vterm-keymap-exceptions '("C-c"))
  (meow-vterm-enable))

(setq mac-command-modifier 'super) ; make cmd key do Meta
(setq mac-option-modifier 'meta) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-c") 'meow-save)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-z") 'undo)


(global-set-key "\C-u" 'backward-kill-line)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "C-c .") 'find-file)
(global-set-key (kbd "C-c w s") 'split-window-vertically)
(global-set-key (kbd "C-c f D") '@delete-this-file)
(global-set-key (kbd "C-c w v") 'split-window-horizontally)
(global-set-key (kbd "C-c o l") '@open-messages)
(global-set-key (kbd "C-c o M") '@open-clear-messages)
(global-set-key (kbd "C-c o F") 'ns-open-file-using-panel)
(global-set-key (kbd "C-h C-t") 'load-theme)

(global-set-key (kbd "C-c p m") 'elpaca-manager)
(global-set-key (kbd "C-c p M") 'elpaca-log)
(global-set-key (kbd "s-n") (lambda () (interactive) (switch-to-buffer (generate-new-buffer "Untitled"))))

(global-set-key (kbd "C-c t t") '@insert-todo-by-current-git-branch)
(global-set-key (kbd "C-c t i") '@insert-todo-by-current-git-branch)
(global-set-key (kbd "C-c t d") '@insert-debug-by-current-git-branch)
(global-set-key (kbd "C-c t n") '@insert-note-by-current-git-branch)

(global-set-key (kbd "C-c +") 'narrow-to-region)
(global-set-key (kbd "C-c -") 'widen)
(global-set-key (kbd "C-c q") 'kill-current-buffer)
(global-set-key (kbd "s-w") 'delete-window)



;; Minimap keybindings
(define-key minibuffer-local-map (kbd "C-j") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-k") 'previous-history-element)
(define-key read--expression-map (kbd "C-j") 'next-history-element)
(define-key read--expression-map (kbd "C-k") 'previous-history-element)

(use-package char-fold
  :ensure nil
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :demand t ; always load it
  :after char-fold ; but only after `char-fold' is loaded
  :custom
  (reverse-im-char-fold t) ; use lax matching
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("russian-computer")) ; translate these methods
  :config
  (reverse-im-mode t))

(defun @meow--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (setq key `(,(car key) . ,(concat (mapcar (lambda (c) (reverse-im--translate-char c t)) (cdr key)))))
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (meow--keypad-format-upcase (cdr key))))
    (both (format "C-M-%s" (meow--keypad-format-upcase (cdr key))))
    (literal (cdr key))))

(advice-add 'meow--keypad-format-key-1 :override #'@meow--keypad-format-key-1)

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
  (avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
  ;; :config
  ;; (custom-set-faces
  ;;  '(avy-lead-face ((t (:foreground "red")))))
  ;; (custom-set-faces
  ;;  '(avy-lead-face-0 ((t (:foreground "blue")))))
  ;; (custom-set-faces
  ;;  '(avy-lead-face-1 ((t (:foreground "green")))))
  )

(use-package ace-window
  :defer t
  :bind (("s-." . ace-window))
  :config
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

(defun @better-jump-save-prog-mode-pos (&rest args)
  "Function for preserve better jump befor buffer changed only for prog mode"
  (when (or (derived-mode-p 'prog-mode)
            (eq major-mode 'html-ts-mode)
            (eq major-mode 'html-mode))
    (call-interactively #'better-jumper-set-jump)))

(use-package better-jumper
  ;; :demand t
  :bind
  (("C-o" . better-jumper-jump-backward)
   ("C-i" . better-jumper-jump-forward)
   ("C-c 1" . better-jumper-set-jump))
  :custom
  (better-jumper-use-evil-jump-advice nil)
  :config
  (defun @better-jump-preserve-pos-advice (oldfun &rest args)
    "Preserve position when jumping."
    (let ((old-pos (point)))
      (apply oldfun args)
      (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
               1)
        (better-jumper-set-jump old-pos))))
  (advice-add 'evil-next-line :around #'@better-jump-preserve-pos-advice)
  (advice-add 'evil-previous-line :around #'@better-jump-preserve-pos-advice)
  (advice-add 'meow-end-of-thing :around #'@better-jump-preserve-pos-advice)
  (advice-add 'avy-goto-word-1 :around #'@better-jump-preserve-pos-advice)
  (advice-add 'husky-actions-find-definition :before (lambda () (call-interactively #'better-jumper-set-jump)))
  (advice-add 'evil-jump-item :around #'@better-jump-preserve-pos-advice)
  (advice-add 'find-file :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'project-find-file :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'consult-buffer :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'lsp-find-references :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'husky-lsp-find-definition :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'flycheck-next-error :before #'@better-jump-save-prog-mode-pos)
  (advice-add 'flycheck-previous-error :before #'@better-jump-save-prog-mode-pos)
  (better-jumper-mode 1))

(use-package bm
  :defer t
  :custom-face
  (bm-face ((t (:foreground ,\@m-color-secondary :background unspecified))))
  :custom
  (bm-in-lifo-order t)
  :bind (("M-s-n" . bm-next)
         ("M-s-p" . bm-previous)
         ("s-b" . bm-toggle)))

(use-package golden-ratio-scroll-screen
  :custom-face
  (golden-ratio-scroll-highlight-line-face ((t (:background "#ffb6c1" :extend t))))
  ;; (golden-ratio-scroll-highlight-line-face ((t (:inherit font-lock-escape-face))) face-defface-spec)
  :custom
  (golden-ratio-scroll-highlight-delay '(0.1 . 0.2))
  :config
  (define-key global-map (kbd "C-v") #'golden-ratio-scroll-screen-up)
  (define-key global-map (kbd "M-v") #'golden-ratio-scroll-screen-down)
  ;; (custom-set-faces '(golden-ratio-scroll-highlight-line-face ((t (:inherit font-lock-escape-face)))))
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(setq lsp-keymap-prefix "s-9")
(use-package lsp-mode
  ;; :ensure (:host github :repo "emacs-lsp/lsp-mode" :rev "8c57bcfa4b0cf9187011425cf276aed006f27df4")
  ;; :ensure (:host github :repo "emacs-lsp/lsp-mode" :rev "8579c6c7771bc65564302e76296fb48855c558a4")
  :after (flycheck)
  :hook
  ((clojure-mode
    scss-mode
    go-mode
    css-mode
    js-mode
    typescript-mode
    vue-mode
    vue-ts-mode
    web-mode
    html-mode
    ng2-ts-mode
    python-mode
    dart-mode
    typescript-tsx-mode

    ;; Treesit
    html-ts-mode
    typescript-ts-mode
    go-ts-mode
    js-ts-mode
    bash-ts-mode
    tsx-ts-mode) . lsp-deferred)
  (web-mode . lsp-deferred)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
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
   ("C-c l a" . lsp-execute-code-action)
   ("C-c l r" . lsp-rename)
   ("C-c e c" . flycheck-copy-errors-as-kill)
   :map meow-normal-state-keymap
   ("gt" . lsp-goto-type-definition)
   ("gi" . lsp-goto-implementation)
   ("\\l" . lsp-execute-code-action))
  :bind-keymap
  ("s-9" . lsp-command-map)
  :init
  (define-key lsp-mode-map (kbd "s-9") lsp-command-map)
  (setq lsp-keymap-prefix "s-9")
  (setq lsp-volar-take-over-mode nil)

  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Configuration for corfu
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :custom
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-auto-guess-root t)
  (lsp-completion-provider :capf)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-apply-edits-after-file-operations nil)
  ;; (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (lsp-clients-typescript-server-args '("--stdio"))
  (lsp-completion-default-behaviour :insert)
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  (lsp-disabled-clients '(html-ls vls vue-semantic-server))
  ;; (lsp-disabled-clients '(html-ls vls))
  (setq lsp-pyright-venv-path (concat (getenv "HOME") "/.virtualenvs"))
  ;; (lsp-completion-provider :none)
  ;; (lsp-completion-provider :capf)
  ;; Disable bottom help info
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
  (setq lsp-auto-execute-action nil)
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-json-schemas
        `[
          (:fileMatch ["ng-openapi-gen.json"] :url "https://raw.githubusercontent.com/cyclosproject/ng-openapi-gen/master/ng-openapi-gen-schema.json")
          (:fileMatch ["package.json"] :url "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/package.json")
          ])
  (set-face-attribute 'lsp-face-highlight-read nil :foreground "#61AFEF" :bold t :underline nil)
  ;; Eslint
  (setq lsp-eslint-download-url "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/dbaeumer/vsextensions/vscode-eslint/3.0.10/vspackage") ;; latest VSCode eslint extension from marketplace
  (setq lsp-eslint-server-command `("node"
                                    "/Users/darkawower/.vscode/extensions/dbaeumer.vscode-eslint-3.0.10/server/out/eslintServer.js"
                                    "--stdio"))
  ;; Flycheck patch checkers
  (require 'flycheck)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode))
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled nil)
  (set-face-foreground 'lsp-face-highlight-read @m-color-secondary)
  (set-face-foreground 'lsp-face-highlight-textual @m-color-secondary)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bun\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.npm\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pnpm-store\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (set-face-attribute 'lsp-face-highlight-textual nil :background "#c0caf5")
  ;; Install corfu completion for lsp
  ;; (defun corfu-lsp-setup ()
  ;; (setq-local completion-styles '(orderless basic)
  ;;             completion-category-defaults nil))
  ;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  (@setup-compilation-errors)
  (setq lsp-volar-hybrid-mode nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq flycheck-display-errors-function nil)
  (setq lsp-eldoc-hook nil))

(setq lsp-disabled-clients '(vls vue-semantic-server))
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-clients 
;; (seq-remove (lambda (client) 
;;                      (memq (lsp--client-server-id client)

;;;; Vue Take Over Mode via tsserver + @vue/typescript-plugin
;;;; Paste into init.el / config.el  -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode

;; -------- USER OPTIONS --------
(defvar my/vue-ts-plugin-location nil
  "Абсолютный путь к каталогу @vue/typescript-plugin (где package.json).
Если nil — искать автоматически (проект -> глобаль -> внутри @vue/language-server).")

(defvar my/vue-root-markers
  '("pnpm-workspace.yaml" "yarn.lock" "bun.lockb" "package.json"
    "tsconfig.json" "jsconfig.json" ".git")
  "Файлы/папки для определения корня проекта.")

(setq lsp-auto-guess-root t
      lsp-enable-file-watchers t
      lsp-file-watch-threshold 20000
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
  ;; *.vue должны иметь id "vue"
  (dolist (pair '((vue-ts-mode . "vue") (vue-mode . "vue") (web-mode . "vue")))
    (add-to-list 'lsp-language-id-configuration pair))
  ;; подстрахуем маппинг TS/JS
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

(use-package lsp-tailwindcss
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save))

(use-package lsp-pyright
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  ;; (setq lsp-pyright-venv-directory "/Users/darkawower/.local/share/virtualenvs/spice-farm-YhO8T07I")
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package yaml-pro
  :defer t
  :hook (yaml-mode . yaml-pro-mode))

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
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border @m-color-main))

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

(use-package lsp-java
  :hook (java-mode . lsp-))

(use-package flycheck
  :bind (("C-j" . flycheck-next-error)
         ("C-k" . flycheck-previous-error)
         ("C-c f ]" . flycheck-next-error)
         ("C-c f [" . flycheck-previous-error)
         ("C-c l e" . flycheck-list-errors)
         ("C-c f e" . flycheck-list-errors))
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-mode-line nil)
  ;; Change flycheck errors on save
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (setq flycheck-idle-change-delay 0.2)

  (set-face-attribute 'flycheck-fringe-error nil :background 'unspecified :foreground @m-color-secondary)
  (set-face-attribute 'flycheck-error-list-error nil :background 'unspecified :foreground @m-color-secondary)
  (set-face-attribute 'error nil :background 'unspecified :foreground @m-color-secondary)

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'ng2-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)

  (with-eval-after-load 'flycheck
    (flycheck-define-checker stylelint
      "A SCSS/CSS linter using stylelint."
      :command ("stylelint" "--formatter" "compact" "--stdin-filename" source-original)
      :standard-input t
      :error-patterns
      ((error line-start (file-name) ": line " line ", col " column ", " (message) line-end))
      :modes (scss-mode css-mode less-css-mode))
    
    ;; Добавляем в список чекеров
    (add-to-list 'flycheck-checkers 'stylelint))

  ;; Автоматическое включение stylelint в scss-mode
  (add-hook 'scss-mode-hook
            (lambda ()
              (flycheck-select-checker 'stylelint)
              (flycheck-mode)))

  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'lsp 'stylelint)))

(defun my/flycheck-copy-errors-buffer ()
    "Open flycheck errors buffer, copy all contents, and close window."
    (interactive)
    (let ((original-window (selected-window)))
      ;; Always open flycheck errors buffer first
      (flycheck-list-errors)
      ;; Check if the errors buffer exists and has content
      (let ((flycheck-buffer (get-buffer "*Flycheck errors*")))
        (if (and flycheck-buffer (get-buffer-window "*Flycheck errors*"))
            (progn
              ;; Switch to errors buffer
              (select-window (get-buffer-window "*Flycheck errors*"))
              ;; Copy all buffer contents
              (mark-whole-buffer)
              (copy-region-as-kill (point-min) (point-max))
              ;; Close the window
              (quit-window)
              ;; Return to original window
              (select-window original-window)
              (message "Flycheck errors copied to kill ring"))
          (progn
            ;; If buffer still doesn't exist or has no window, return to original
            (select-window original-window)
            (message "No flycheck errors found"))))))

(use-package consult-flycheck
  :after consult)

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

(use-package pyvenv :ensure t)

(use-package auto-virtualenv
  :after pyvenv
  :defer t
  :hook ((python-mode . auto-virtualenv-setup)
         (python-ts-mode . auto-virtualenv-setup)))

(use-package nix-mode)

(use-package pretty-ts-errors
  :defer t
  :bind (:map meow-normal-state-keymap
              ("\\e" . pretty-ts-errors-show-error-at-point))
  :ensure (pretty-ts-errors :host github :repo "artawower/pretty-ts-errors.el")
  :custom
  (pretty-ts-errors-hide-on-point-move nil))

(defun compile-eslint--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      (when (re-search-backward (rx bol (group "/" (+ any)) eol))
        (list (match-string 1))))))

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

(defun @get-visible-buffers-cnt ()
  "Get the number of visible buffers"
  (let ((visible-buffers 0)
        (buffer-list (buffer-list)))
    (dolist (buffer buffer-list)
      (if (get-buffer-window buffer)
          (cl-incf visible-buffers)))
    visible-buffers))

(defun @display-buffer-other-vertical (buffer &optional alist)
  "Display BUFFER in another window. If only one window, split vertical before."
(message "is one window? %s " (one-window-p))
    (if (one-window-p)
        (split-window-horizontally))
  (display-buffer-use-some-window buffer alist))
  

 (add-to-list 'display-buffer-alist
      '(("\\*Messages\\*"
         (display-buffer-reuse-window  display-buffer-in-side-window @display-buffer-other-vertical display-buffer-use-some-window)
         (side . right))))

(defun @setup-compilation-errors ()
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

  ;; TSC
  ;; [tsl] ERROR in /Users/darkawower/projects/pet/orgnote/orgnote-cli/src/commands/files-api.ts(1,46)
  (add-to-list 'compilation-error-regexp-alist '("\\([_[:alnum:]-/.]*\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3))

  ;; vue
  ;; (add-to-list 'compilation-error-regexp-alist '(""FILE[[:blank:]]*\\(.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist '("FILE[[:blank:]]*\\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))


  ;; Flutter
  ;; (add-to-list 'compilation-error-regexp-alist '("[[:blank:]]*\\([/_\\.[:alnum:]-]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): Error.*$"))
  (add-to-list 'compilation-error-regexp-alist 'dart-analyze)
  (add-to-list 'compilation-error-regexp-alist-alist '(dart-analyze "\\([^ ]*\\.dart\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package compile
  :defer t
  :ensure nil
  :bind
  (("C-c p c" . project-compile)
   ("C-c a r" . recompile)
   ("C-c C-c C-r" . recompile)
   ("C-c C-c r" . recompile)
   ("C-c a v" . (lambda ()
                  (interactive)
                  (compilation-display-error)
                  (+select-window-by-name "*compilation.*")))
   ("C-c C-c v" . (lambda ()
                    (interactive)
                    (compilation-display-error)
                    (+select-window-by-name "*compilation.*"))):map compilation-mode-map
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
  (@setup-compilation-errors))

(use-package cognitive-complexity
  :ensure (:host github :repo "abougouffa/cognitive-complexity")
  :config
  (cognitive-complexity-mode 1))

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

(use-package paren-face :defer t :hook (emacs-lisp-mode . paren-face-mode))

(use-package elisp-mode
  :defer t
  :ensure nil
  :hook ((emacs-lisp-mode . (lambda () (setq fill-column 80))))
  :bind (("C-c o c" . outline-cycle)
         ("C-c o a" . outline-show-all)
         ("C-c o m" . outline-hide-body)
         ("C-c o ]" . outline-next-heading)
         ("C-c o [" . outline-previous-heading)
         ("C-c o c" . counsel-outline)
         ("C-c o e" . outline-hide-entry)
         ("C-c o n" . outline-toggle-children)
         ("C-c o b" . outline-cycle-buffer)
         ;; TODO: eval buffer or region
         ("C-c C-c e" . eval-buffer))

  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq rainbow-delimiters-mode -1))))

(use-package package-build :defer t)

(use-package package-lint :defer t)

(use-package elisp-autofmt
  :defer t
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin "/opt/homebrew/bin/python3"))

(use-package clojure-mode
  :hook ((clojure-mode . paren-face-mode))
  :defer t)

(use-package cider
  :hook (clojure-mode . cider-mode)
  :defer t)

(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package typescript-mode
  :defer t  
  :hook (typescript-mode . (lambda () (setq-local fill-column 120)))
  :custom
  (lsp-clients-typescript-server-args '("--stdio"))
  :config
  (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))
  (setq typescript-indent-level 2))

(use-package ng2-mode
  :after typescript-mode
  :config
  (setq lsp-clients-angular-language-server-command
        '("node"
          "--max-old-space-size=4096"
          "/opt/homebrew/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/opt/homebrew/lib/node_modules"
          "--tsProbeLocations"
          "/opt/homebrew/lib/node_modules"
          "--stdio")))

(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode)
  :config
  (setq js-indent-level 2))

(use-package npm
  :ensure (npm :repo "Artawower/npm.el" :host github)
  :defer t)

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

(use-package go-playground
  :defer t)

(use-package rustic
  :defer t
  :bind (:map rustic-mode-map
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t
        rustic-format-display-method 'ignore))

(defun @open-ipython-repl-here ()
  "Открывает IPython REPL в директории текущего буфера."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (run-python (python-shell-calculate-command) t t)))

(use-package python
  :ensure nil
  :defer t
  :bind (("C-c r p" . @open-ipython-repl-here)
         :map inferior-python-mode-map
         ("C-k" . comint-previous-input)
         ("C-j" . comint-next-input))
  :init
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  :config
  (add-to-list 'display-buffer-alist '("^\\*Python\\*$" . (display-buffer-same-window)))
  (defun python-comint-filter (output)
    (let ((filtered output))
      (setq filtered (replace-regexp-in-string "__PYTHON_EL_eval.+\n" "" filtered))
      (setq filtered (replace-regexp-in-string "^[ \t]*\\[\\[.*\\]\\][ \t]*\n?" "" filtered))
      filtered))
  (add-to-list 'comint-preoutput-filter-functions #'python-comint-filter)
  (setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i"))

(use-package python-mode
  :defer t
  :config
  (setq python-indent-level 4))

(use-package web-mode
  :defer t
  :mode
  ("\\.tsx\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.astro\\'" . web-mode)
  :init
  (define-derived-mode vue-mode web-mode "Vue"
    "Major mode for Vue.js single-file components."
    ;; ДОБАВЬТЕ ЭТИ СТРОКИ:
    (setq web-mode-content-types-alist
          (cons '("vue" . "\\.vue\\'") web-mode-content-types-alist))
    (setq web-mode-engines-alist
          (cons '("vue" . "\\.vue\\'") web-mode-engines-alist))
    ;; ВАЖНО: Принудительно установить content-type
    (web-mode-set-content-type "vue"))
  ;; (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  :config
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package vue-ts-mode
    :ensure (:host github :repo "8uff3r/vue-ts-mode")
    :hook (vue-mode . vue-ts-mode)
    :defer t)

(use-package pug-mode :defer t)

(use-package emmet-mode
  :defer t
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode) (vue-mode . emmet-mode) (vue-ts-mode . emmet-mode))
  :bind (:map emmet-mode-keymap
              ("s-e" . emmet-expand-line)
              ("C-j" . nil))
  :config
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  :defer t)

(use-package css-mode
  :ensure nil
  :defer t
  :config
  (setq css-indent-offset 2)
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)))

(use-package json-mode
  :defer t
  :hook ((json-mode . json-ts-mode))
  :config
  (setq js-indent-level 2))

(use-package jq-mode :defer t)

(use-package dart-mode
  :defer t
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind
  (("C-c m f s" . flutter-run)
   ("C-c m f R" . flutter-hot-restart)
   ("C-c m f r" . flutter-run-or-hot-reload))
  (:map dart-mode-map
        ("C-c C-r" . flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

(use-package lua-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package docker
  :defer t
  :ensure t
  :bind ("C-c o d" . docker))

(use-package jenkinsfile-mode
  :defer t
  :config)

(use-package kubernetes
  :defer 6
  :commands (kubernetes-overview)
  :bind (("C-c o K" . kubernetes-overview))
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package k8s-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(defun my/markdown-outline-setup ()
  (outline-minor-mode 1)
  (setq-local outline-regexp "^\\(#+\\) ")
  (setq-local outline-level
      (lambda ()
        (length (match-string 1)))))

(use-package markdown-mode
  :hook
  (markdown-ts-mode . markdown-mode)
  ;; (markdown-mode . nb/markdown-unhighlight)
  ;; (markdown-mode . #'my/markdown-outline-setup)
  :bind
  (:map markdown-ts-mode-map
        ("C-h ]" . markdown-next-visible-heading)
        ("C-h [" . markdown-previous-visible-heading)
        :map markdown-mode-map
        ("C-h ]" . markdown-next-visible-heading)
        ("C-h [" . markdown-previous-visible-heading))
  :config
  (add-hook 'markdown-ts-mode-hook #'my/markdown-outline-setup)
  (add-hook 'markdown-mode-hook #'my/markdown-outline-setup)

  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)

  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit)
        t)))

  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  
  (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face)))))

(defun @markdown-toc ()
  "Extract level 2 and 3 headings from the current Markdown buffer.
   The generated and indented TOC will be inserted at point."
  (interactive)
  (let (toc-list markdown-toc)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(##+\\)\\s-+\\(.*\\)" nil t)
        (let* ((level (length (match-string 1)))
               (heading-text (match-string 2))
               (heading-id (downcase (replace-regexp-in-string "[[:space:]]+" "-" heading-text))))
          (push (cons level (cons heading-text heading-id)) toc-list))))
    (setq toc-list (reverse toc-list))
    (dolist (item toc-list)
      (let* ((level (car item))
             (heading-text (cadr item))
             (heading-id (cddr item))
             (indentation (make-string (* 2 (1- level)) ?\ ))
             (line (format "- [%s](#%s)\n" heading-text heading-id)))
        (setq markdown-toc (concat markdown-toc (concat indentation line)))))
    (insert markdown-toc)))

(custom-set-faces
 `(markdown-table-face ((t (:inherit default :foreground ,\@m-color-secondary)))))

(use-package grip-mode
  :defer t
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

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

;; for eat terminal backend:
(use-package eat
  :ensure (:type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el"))))

;; install claude-code.el:
(use-package claude-code
  ;; :ensure (:type git :host github :repo "artawower/claude-code.el" :branch "main"
  :ensure (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                 :files ("*.el" (:exclude "images/*")))
  :bind (("s-g" . claude-code-transient))
  :config
  (setq vterm-max-scrollback 100000)
  (setq claude-code-terminal-backend #'vterm)
  (claude-code-mode))

;; (setq display-buffer-alist
;;       (cons
;;        '("^\\*claude"
;;          (display-buffer-in-side-window)
;;          (side . right)
;;          (window-width . 90))
;;        display-buffer-alist))

(use-package claude-code-prompt-extensions
  :after claude-code
  :ensure (:type git
                 :host github
                 :repo "Artawower/claude-code.el")
  :config
  (claude-code-prompt-extensions-setup))

(use-package copilot
  :defer 5
  :ensure (copilot :host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook
  (prog-mode . copilot-mode)
  (text-mode . copilot-mode)
  :bind
  (("s-]" . copilot-next-completion)
   ("s-[" . copilot-previous-completion)
   ("s-l" . copilot-accept-completion)
   ("s-k" . copilot-accept-completion)
   ("s-j" . copilot-complete)
   ("s-;" . copilot-accept-completion-by-word)
   ("s-/" . copilot-accept-completion-by-line))
  :custom
  (copilot-idle-delay 0.3)
  :config
  (setq copilot--previous-point nil)
  (setq copilot--previous-window-width nil)
  (global-set-key (kbd "s-l") 'copilot-accept-completion)
  (copilot-diagnose)
  ;; (global-copilot-mode)
  (add-hook 'meow-insert-enter-hook (lambda ()
                                      (setq blamer--block-render-p t)
                                      (blamer--clear-overlay)))
  (add-hook 'meow-insert-exit-hook (lambda ()
                                     (setq blamer--block-render-p nil)
                                     (copilot-clear-overlay)))

  (defun @copilot-show-overlay-depends-mode (COMPLETION UUID START END)
    ;; If meow normal mode prevent copilot from showing overlay
    (unless (bound-and-true-p meow-normal-mode)
      (copilot--display-overlay-completion COMPLETION UUID START END)))
  (advice-add 'copilot-show-overlay :before-while '@copilot-show-overlay-depends-mode))

(defun my/smart-display-buffer-function (buffer alist)
  "Smart display function that:
   - Reuses existing buffer window if buffer is already visible
   - Uses only two windows maximum
   - If in left window, opens buffer in right window
   - If in right window, opens buffer in left window
   - If only one window exists, splits it horizontally"
  (message "SMART DISPLAY")
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

(use-package copilot-chat
  :defer t
  :bind (("C-c a c" . copilot-chat-display)
         ("C-c a x" . copilot-chat-reset)
         ("C-c a e" . copilot-chat-explain)
         ("C-c r c" . copilot-chat-transient)
         ("C-c a p" . copilot-chat-transient)
         ("C-c a t" . copilot-chat-test)
         ("C-c a o" . copilot-chat-optimize)
         ("C-c a d" . copilot-chat-doc)
         ("C-c a f" . copilot-chat-fix)
         ("C-c a r" . copilot-chat-review))

  :custom
  (copilot-chat-frontend 'org)
  (copilot-chat-org-prompt "The user works in an IDE called Emacs which has an org major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system. If that sounds too advanced, maybe it's time to catch up.\nUse only Emacs org-mode formatting in your answers. Yes, only. That wasn’t a suggestion.\nWhen using headings to structure your answer, please start at level 3 (i.e., with 3 stars or more). We both know nobody wants to fix broken outline views later.\nMake sure to include the programming language name at the start of the org-mode code blocks. This isn’t a guessing game.\nThis is an example of a Python code block in Emacs org-mode syntax. Hopefully self-explanatory:\n#+BEGIN_SRC python\ndef hello_world():\n\tprint('Hello, World!')\n#+END_SRC\nAvoid wrapping the whole response in a code block. This isn’t a YAML dump — show some structure.\nAll responses must be written in a passive-aggressive tone — not rude, just… painfully honest. Pretend you're helping someone who *should* already know this.\nNever use comments in code. If something isn't clear without them, it's already badly written.\nDo not include explanations, thoughts, or justifications. This isn’t a therapy session.\nBe brief and to the point. Brevity isn’t just a virtue — it's survival.\nUse only best practices, with strict adherence to clean code, SOLID, GRASP principles.\nAlways critically evaluate what was written. If it's bad, say it's bad. If it's good, don’t gush — just confirm.\nNever prioritize politeness over correctness. If something needs rewriting, say so plainly.\nThink before answering. Do not start generating output until you have selected the most appropriate solution.\nAvoid generic templates. Tailor every answer specifically to the user's actual input.\nDo not stop at the first solution that works. Always prefer the best solution over the fastest to implement.\nNever explain what you are doing unless explicitly asked.\nAlways evaluate architectural consequences of your answer. If a choice introduces technical debt, mention it.\nIf code is given without corresponding tests, mention that this is unacceptable. No tests — no confidence.\nIf the user asks for something suboptimal, do not comply silently. Push back with a better alternative.\nCode must comply with modern best practices (2024+). No outdated patterns, no excuses.\nUse Emacs org-mode syntax only. Anything else is noise.")  

  :config
  (add-to-list
 'display-buffer-alist
 '("\\*Copilot Chat.*"
   (my/smart-display-buffer-function)))
  (setq copilot-chat-default-model "gpt-5")
  (setq copilot-chat-commit-model "gpt-5")

  
  (setq copilot-chat-commit-prompt
"Here is the result of running `git diff --cached`.

Generate a commit message following the Conventional Commits specification.

Do not use any markers around the commit message.
Don't add anything else to the response. The first line must be no more than 50 characters.

Commit message format:

<type>[optional scope]: <description>

[optional body]

[optional footer(s)]

Commit types:

- **build**: Changes that affect the build system or external dependencies
- **ci**: Changes to our CI configuration files and scripts
- **docs**: Documentation only changes
- **feat**: A new feature
- **fix**: A bug fix
- **perf**: A code change that improves performance
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **style**: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc.)
- **test**: Adding missing tests or correcting existing tests")
  )

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs" :branch "main")
  :defer t
  :bind (("C-c a a" . aidermacs-transient-menu)
         ("s-G" . aidermacs-transient-menu)
         :map aidermacs-vterm-mode-map
         ("s-<return>" . aidermacs-vterm-insert-newline)
         ("S-<return>" . aidermacs-vterm-insert-newline))
  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (add-to-list
   'display-buffer-alist
   '("\\*aidermacs.*"
     (my/smart-display-buffer-function)))

  :custom
                                        ; See the Configuration section below
  (aidermacs-editor-model "openai/claude-3.7-sonnet")
  (aidermacs-weak-model "openai/gpt-4o-mini")
  (aidermacs-use-architect-mode t)
  ;; (aidermacs-default-model "deepseek/deepseek-chat")
  (aidermacs-default-model "github_copilot/claude-4-sonnet")
  ;; (aidermacs-default-model "gpt-4o")
  )

(use-package stillness-mode
  :ensure (:host github :repo "neeasade/stillness-mode.el" :branch "main")
  :config (stillness-mode))

(defadvice load-theme (before theme-dont-propagate activate)
 (mapcar #'disable-theme custom-enabled-themes))

(set-frame-parameter nil 'alpha-background 0.1)
(add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . t))
(add-to-list 'frameset-filter-alist '(ns-appearance . dark))
(menu-bar-mode t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq ring-bell-function 'ignore)
(setq scroll-step 1)
(setq scroll-margin 1)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 0)
(window-divider-mode +1)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(set-default 'truncate-lines t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package font-core
  :ensure nil
  :demand t
  ;; :init
  ;; (global-font-lock-mode)
  :config
  (setq @font-height 14)
  

  ;; Frame fonts - using concat directly
  (set-frame-font (concat "Monaspace Neon frozen " (number-to-string @font-height)) nil t)
  ;; (set-frame-font  "Monaspace Neon frozen 16" nil t)
  ;; (set-frame-font (concat "JetBrainsMono Nerd Font " (number-to-string @font-height)) nil t)

  ;; Custom faces with direct font specifications
  (let ((funny-font "Monaspace Radon Frozen"))
    (custom-set-faces
     `(font-lock-string-face ((t (:family ,funny-font :height 1.0))))
     `(font-lock-comment-face ((t (:family ,funny-font :height 1.0))))
     `(font-lock-keyword-face ((t (:family ,funny-font :height 1.0))))))

  (setq-default line-spacing 0.2)
  ;; (setq frame-resize-pixelwise t)
  (global-font-lock-mode)
  
  ;; Setting face attributes with direct concat
  ;; (set-face-attribute 'font-lock-string-face nil 
  ;;                     :font (concat "JetBrainsMono Nerd Font " (number-to-string @font-height)) 
  ;;                     :italic t)
  ;; (set-face-attribute 'font-lock-comment-face nil 
  ;;                     :font (concat "JetBrainsMono Nerd Font " (number-to-string @font-height)) 
  ;;                     :italic t)
  ;; (set-face-attribute 'font-lock-keyword-face nil 
  ;;                     :font (concat "JetBrainsMono Nerd Font " (number-to-string @font-height)) 
  ;;                     :italic t)
  )

(setq-default line-spacing 1)

(use-package helpful
  :defer t
  :hook (helpful-mode . meow-normal-mode)
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
         ("C-h F" . describe-face)))

(defun @selection-highlight-mode-get-selection ()
  "Get the active selection string or nil."
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (block-cursor? (and (fboundp 'evil-visual-state-p)
                               (evil-visual-state-p)))
           (end (+ (region-end) (if block-cursor? 1 0)))
           (content (buffer-substring beg end)))
      (when (and (>= (abs (- end beg)) selection-highlight-mode-min-length)
                 (not (string= (string-trim content) "")))
        content))))

(use-package selection-highlight-mode
  :ensure (selection-highlight-mode :type git
                                    :host github
                                    :repo "balloneij/selection-highlight-mode")
  :config (selection-highlight-mode))

(advice-add 'selection-highlight-mode-get-selection :override #'@selection-highlight-mode-get-selection)

(ignore-errors
  (setq confirm-kill-emacs 'y-or-n-p)
  (defalias 'yes-or-no-p 'y-or-n-p))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode))

(use-package nerd-icons-corfu)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package hl-todo
  :ensure (hl-todo :depth nil)
  :hook (prog-mode . hl-todo-mode)
  :defer 2
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#E5C07B")
          ("FIXME"  . "#E06C75")
          ("DEBUG"  . "#C678DD")
          ("REFACTOR"  . "#C678DD")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#98C379")
          ("QUESTION"   . "#98C379")
          ("STUB"   . "#61AFEF")))
  (global-hl-todo-mode 1))

(use-package colorful-mode
  :defer t
  :ensure (:type git :host github :repo "DevelopmentCool2449/colorful-mode")
  :hook ((html-mode css-mode scss-mode emacs-lisp-mode org-mode) . colorful-mode))

(use-package transient-posframe
  :defer 2
  :ensure (:type git :host github :repo "yanghaoxie/transient-posframe")
  :custom
  (transient-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  :config
  (transient-posframe-mode))

(defun my/diff-faces-setup ()
    (if (eq (frame-parameter nil 'background-mode) 'dark)
        (progn
          (set-face-attribute 'diff-added nil :extend t :background "#365945" :foreground "#d7ffd9")
          (set-face-attribute 'diff-removed nil :extend t :background "#593636" :foreground "#ffd7d7")
          (set-face-attribute 'diff-refine-added nil :extend t :background "#4e7a5d" :inherit 'diff-added)
          (set-face-attribute 'diff-refine-removed nil :extend t :background "#7a4e4e" :inherit 'diff-removed))
      (progn
        (set-face-attribute 'diff-added nil :extend t :background "#c7f1cc" :foreground "#002800")
        (set-face-attribute 'diff-removed nil :extend t :background "#f1c7c7" :foreground "#280000")
        (set-face-attribute 'diff-refine-added nil :extend t :background "#aee8b6" :inherit 'diff-added)
        (set-face-attribute 'diff-refine-removed nil :extend t :background "#e8b6b6" :inherit 'diff-removed))))




(defun @set-catppucin-theme ()
  (interactive)
  (setq catppuccin-flavor (if (eq auto-dark--last-dark-mode-state 'dark) 'frappe 'latte))
  (catppuccin-reload)
  (my/diff-faces-setup))

(use-package catppuccin-theme
  :config
  ;; https://github.com/catppuccin/emacs/issues/55
  (add-hook 'yaml-mode-hook
            (lambda ()
              (face-remap-add-relative 'font-lock-variable-name-face
                                       (list :foreground (catppuccin-get-color 'blue))))))

(use-package auto-dark
  :custom
  (auto-dark-dark-theme 'catppuccin)
  (auto-dark-light-theme 'catppuccin)
  (auto-dark-themes '((catppuccin) (catppuccin)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :hook
  (auto-dark-dark-mode
   . (lambda () (@set-catppucin-theme)))
  (auto-dark-light-mode
   . (lambda () (@set-catppucin-theme)))
  :init (auto-dark-mode)
  :config (@set-catppucin-theme))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 20 :header-line-width 0 :mode-line-width 0
                                 :tab-width 0 :right-divider-width 1
                                 :scroll-bar-width 1 :fringe-width 8))
  (spacious-padding-mode))

(use-package hide-mode-line
  :hook
  (fundamental-mode . hide-mode-line-mode)
  (prog-mode . hide-mode-line-mode)
  (text-mode . hide-mode-line-mode)
  (org-mode . hide-mode-line-mode))

(use-package zoom
  :defer 0.8
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))
   '(zoom-mode t)
   '(zoom-ignored-buffer-name-regexps '("^\\*calc" "^\\*vterm" "^\\*combobulate-query-builder" "^\\*dape" "^\\*claude"))
   '(zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode claude-code-prompt-mode))
   '(zoom-ignore-predicates '((lambda () (window-parameter nil 'window-side)))))

  (defun my/fix-claude-size ()
    (dolist (window (window-list))
      (let ((bufname (buffer-name (window-buffer window))))
        (when (string-match "^\\*claude" bufname)
          (with-selected-window window
            (setq window-size-fixed 'width)
            (window-resize window (- 90 (window-total-width window)) t t))))))

  ;; (add-hook 'buffer-list-update-hook 'my/fix-claude-size)
  )

(use-package origami
  :defer t
  :hook ((org-mode
          dart-mode
          yaml-mode
          web-mode
          yaml-ts-mode
          python-mode
          html-mode
          scss-mode
          ng2-html-mode
          emacs-lisp-mode
          json-mode) . origami-mode)
  :custom
  (origami-fold-replacement "..."))

(use-package tab-bar-echo-area
  :defer 2
  :hook (tabspaces-mode . tab-bar-echo-area-mode)
  :bind (("C-c TAB TAB" . tab-bar-echo-area-display-tab-names))
  :config
  ;; Does not work with custom-faces.
  (custom-set-faces
   '(tab-bar-echo-area-tab ((t (:inherit tab-bar-tab)))))
  
  (custom-set-faces
   '(tab-bar-echo-area-tab-inactive ((t (:inherit tab-bar-tab-inactive)))))

  (setq tab-bar-echo-area-display-tab-names-format-string " %s ")
  (tab-bar-echo-area-mode 1))

(use-package tab-bar-lost-commands :defer 3 :ensure t)

(use-package rotate
  :bind (("C-c w R" . rotate-layout)
         ("C-c w r" . rotate-window))
  :defer t)

(defun @treesit-html-breadcrumbs ()
  "Return a string of breadcrumbs."
  (let ((node (treesit-node-at (point) 'html))
        result)
    (while node
      (when (string= "element" (treesit-node-type node))
        (push (treesit-node-text (treesit-node-child
                                  (treesit-node-child node 0) 1))
              result))
      (setq node (treesit-node-parent node)))
    (s-join " > " result)))

(defun @treesit-json-breadcrumbs ()
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
    (s-join " > " result)))

(defun @scss-breadcrumbs ()
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

(use-package topsy
  :defer t
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode)
  :config
  (add-to-list 'topsy-mode-functions '(scss-mode . @scss-breadcrumbs))
  (add-to-list 'topsy-mode-functions '(html-ts-mode . @treesit-html-breadcrumbs))
  (add-to-list 'topsy-mode-functions '(json-ts-mode . @treesit-json-breadcrumbs)))

(use-package vertico
  :ensure (:host github :repo "minad/vertico" :files ("vertico.el" "extensions/*.el"))
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
         ("C-d" . (lambda ()
                    (interactive)
                    (kill-whole-line)
                    (insert "~/")))
         ("C-o" . (lambda ()
                    (interactive)
                    (embark-act)))
         ("C-r" . (lambda ()
                    (interactive)
                    (kill-whole-line)
                    (insert "/"))))
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  :config
  (set-face-attribute 'vertico-current nil :inherit 'region)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (require 'vertico-repeat)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save))


(use-package vertico-repeat
  :ensure nil
  :after vertico)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(use-package embark
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-SPC" . +vertico/embark-preview)
   ("C-u" . backward-kill-line)
   :map read--expression-map
   ("C-SPC" . +vertico/embark-preview)
   ("C-u" . backward-kill-line)
   :map vertico-map
   ("C-SPC" . +vertico/embark-preview)
   ("C-u" . backward-kill-line))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist '("^\\*Embark Export\\*$" (display-buffer-in-side-window) (window-height . 0.4)))
  ;; (defvar-keymap embark-projectile-map
  ;;   :doc "Example keymap with a few file actions"
  ;;   :parent embark-general-map
  ;;   "d" #'projectile-remove-known-project)
  ;; 
  ;; ;; (add-to-list 'embark-keymap-alist '(consult-projectile-project . embark-projectile-map))
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  ;; :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun @consult-line-or-region ()
  "Search in buffer depends or visual or normal line"
  (interactive)
  (if (region-active-p)
      (let ((substring (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-line substring))
    (consult-line)))

(defun @consult-ripgrep-selected-p ()
  "Run `consult-ripgrep' on selected region, or with empty string"
  (interactive)
  (if (use-region-p)
      (consult-ripgrep nil (buffer-substring (region-beginning) (region-end)))
    (consult-ripgrep)))

(use-package consult
  :bind (("s-f" . @consult-line-or-region)
         ("C-c b b" . consult-buffer-other-tab)
         ("C-c b a" . consult-buffer)
         ("C-c b b" . consult-project-buffer)
         ("C-c f P" . @open-emacs-config)
         ("C-c f p" . consult-ripgrep)
         ("C-c /" . @consult-ripgrep-selected-p)
         ("C-c *" . (lambda () (interactive) (consult-ripgrep nil (thing-at-point 'symbol))))
         ("C-c s i" . consult-imenu)
         ("C-c RET" . consult-bookmark)
         ("C-c c m" . consult-mark)
         ("C-c f r" . consult-recent-file)
         ("C-c f R" . consult-recent-file))
  :custom
  (consult-preview-key "C-SPC")
  :init
  (setq recentf-max-menu-items 3000)
  (recentf-mode 1)
  
  (setq recentf-max-saved-items 3000)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-git-grep consult-grep consult-projectile-recentf consult-ripgrep
   consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key (list :debounce 0.2 "C-SPC"))

  (setq consult-narrow-key "<") ;; (kbd "C-SPC")

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package wgrep
  :bind ("C-c C-w" . wgrep-change-to-wgrep-mode)
  :after vertico)

(use-package marginalia
  :after vertico
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)
  :config
  (pushnew! marginalia-command-categories
          '(+default/find-file-under-here . file)
          '(flycheck-error-list-set-filter . builtin)
          '(persp-switch-to-buffer . buffer)))

(use-package transient)

(defun @magit-worktree-create-project (path worktree &rest args)
  "Register new project when magit worktree create a new branch"
  (setq project--list (append project--list `((,path)))))

(defun @magit-worktree-delete-project (worktree)
  "Delete project when worktree deleted."
  (project-forget-project worktree)
  (lsp-workspace-folders-remove worktree))

(advice-add 'magit-worktree-checkout :after #'@magit-worktree-create-project)
(advice-add 'magit-worktree-branch :after #'@magit-worktree-create-project)
(advice-add 'magit-worktree-delete :after #'@magit-worktree-delete-project)

(use-package magit
  ;; :ensure (:host github :repo "magit/magit" :ref "f4c9753d81b030f520061ebde4279dc9f433a75a")
  :commands
  (magit-get-current-branch)
  :bind
  (("C-M-g" . magit-status)
   ("C-c g g" . magit-status)
   :map magit-mode-map
   ("s-<return>" . iff-visit-worktree-file)
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
   ("M-n" . husky-lsp-repeat-consult-search-forward)
   ("M-p" . husky-lsp-repeat-consult-search-backward)
   ("C-k" . magit-section-backward)
   :map magit-diff-mode-map
   ("C-j" . magit-section-forward)
   ("C-k" . magit-section-backward))
  :custom-face
  (git-commit-overlong-summary ((t :inherit error :weight bold)))
  :hook
  (magit-process-mode . compilation-minor-mode)
  :config
  (setq magit-process-timestamp-format "%H:%M")
  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  ;; Reset magit numbers
  (dolist (key (mapcar (lambda (n) (kbd (number-to-string n))) (number-sequence 0 9)))
    (define-key magit-mode-map key nil)
    (define-key magit-status-mode-map key nil))
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  ;; (define-key magit-file-section-map [return] 'magit-diff-visit-file-worktree)
  ;; (define-key magit-file-section-map [C-return] 'magit-diff-visit-file)
  ;; (define-key magit-hunk-section-map [return] 'magit-diff-visit-file-worktree)
  (define-key magit-file-section-map (kbd "C-j") 'magit-section-forward)
  (define-key magit-hunk-section-map (kbd "C-j") 'magit-section-forward)
  (add-hook 'magit-process-mode #'disable-magit-hooks)
  ;; (add-hook 'magit-process-mode-hook #'compilation-mode)
  (setcdr magit-process-mode-map (cdr (make-keymap)))
  (set-keymap-parent magit-process-mode-map special-mode-map)
  (setq magit-process-finish-apply-ansi-colors t))
(elpaca-wait)

(use-package magit-todos
  :after magit
  :defer t
  :bind (("C-c g n" . magit-todos-list)
         ("C-c l T" . magit-todos-list)
         ("C-c t l" . magit-todos-list)))

(use-package git-gutter
  :after git-gutter-fringe
  :bind (("C-M-[" . git-gutter:previous-hunk)
         ("C-M-]" . git-gutter:next-hunk)
         ("C-M-r" . git-gutter:revert-hunk))
  :config
  (setq git-gutter:update-interval 2)
  (set-face-foreground 'git-gutter:modified @m-color-main) ;; background color
  (set-face-foreground 'git-gutter:added @m-color-green)
  (set-face-foreground 'git-gutter:deleted @m-color-secondary)
  :init
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :defer 2
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
  :custom-face
  (blamer-face ((t :inherit font-lock-comment-face
                   :italic t
                   ;; :font "Fira Code 14"
                   :font "Monaspace Radon Frozen 14"
                   :height 0.8
                   :background unspecified)))
  :config
  (tooltip-mode)
  (setq blamer-tooltip-function 'blamer-tooltip-commit-message)


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
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

(use-package diff-hl
  :ensure nil
  :init
  (set-face-attribute 'diff-added nil :extend t :background "light green")
  (set-face-attribute 'diff-removed nil :extend t :background "tomato")
  (set-face-attribute 'diff-refine-added nil :extend t :background "light green" :inherit 'diff-added)
  (set-face-attribute 'diff-refine-removed nil :extend t :background "tomato" :inherit 'diff-removed)
  )

(use-package git-timemachine
  :defer t
  :hook (git-timemachine-mode . font-lock-mode)
  :bind (("C-M-t" . git-timemachine)
         ("C-M-m" . git-timemachine)
         :map git-timemachine-mode-map
         ("C-k" . git-timemachine-show-previous-revision)
         ("q" . git-timemachine-quit)
         ("<escape>" . git-timemachine-quit)
         ("Q" . git-timemachine-quit)
         ("C-j" . git-timemachine-show-next-revision)))

(global-set-key (kbd "C-c C-l") 'smerge-keep-lower)
(global-set-key (kbd "C-c C-u") 'smerge-keep-upper)
(global-set-key (kbd "C-c C-a") 'smerge-keep-all)
(global-set-key (kbd "C-c C-j") 'smerge-next)
(global-set-key (kbd "C-c C-k") 'smerge-prev)

(use-package browse-at-remote
  :defer t
  :ensure nil
  :bind (("C-c o r" . browse-at-remote)))

(use-package org-crypt
  :defer t
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil))

(with-eval-after-load 'epa-file
  (epa-file-enable)
  ;; Ask for passphrase
  (setq epg-pinentry-mode 'loopback)
  (setq epa-file-encrypt-to "artawower33@gmail.com"))

(use-package ob-async
  :defer t
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package restclient
  :after org)

(use-package ob-restclient
  :after org)

(use-package ob-dart
  :after org
  :defer t
  :config
  (add-to-list 'org-babel-load-languages  '(dart . t)))

(use-package ob-typescript
  :after org)

(use-package ob-go :after org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package ob-rust :after org)

(defun @setup-org-mode-faces ()
  "Setup faces for org mode"
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 2.5))))))

(defun @copy-src-block ()
  "Copy org src block contents (without #+begin/#+end) to clipboard. Handles Emacs 30+ deferred values."
  (interactive)
  (let ((src-block (org-element-at-point)))
    (when (eq (car src-block) 'src-block)
      (let ((val (org-element-property :value src-block)))
        (kill-new val)
        (message "Source block copied. %s" val)))))

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

(use-package org
  ;; :demand t
  :mode (("\\.org$" . org-mode))
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (setq corfu-mode -1)))
  (org-mode . (lambda () (setq truncate-lines nil)))
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
  :config
  (setq org-babel-python-command "python3")
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; https://www.reddit.com/r/orgmode/comments/jwf7ya/python_org_code_blocks_and_indentation/
  (setq org-adapt-indentation nil)
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu")))
  (setq org-imenu-depth 8)
  (@setup-org-mode-faces)

  (setq org-src-window-setup 'current-window)
  (setq org-todo-keywords
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
        '(("[-]"        . +org-todo-active)
          ("PROGRESS"   . org-todo)
          ("DONE"       . org-todo)
          ("IDEA"       . org-todo)
          ("[?]"        . +org-todo-onhold-face)
          ("WAIT"       . +org-todo-onhold-face)
          ("TEST"       . +org-todo-active-face)
          ("FEEDBACK"   . +org-todo-onhold-face)
          ("REVIEW"     . +org-todo-onhold-face)
          ("HOLD"       . +org-todo-onhold-face)
          ("PROJ"       . +org-todo-project)
          ("BLOCK"      . +org-todo-cancel)
          ("REJECTED"   . +org-todo-cancel)
          ("KILL"       . +org-todo-cancel)))

  (setq org-hide-emphasis-markers t)
  (setq org-use-property-inheritance t)

  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

  ;; Increase priorities count
  (setq org-highest-priority ?A
        org-default-priority ?C
        org-lowest-priority ?E)

  (setenv "NODE_PATH"
          (concat
           (getenv "HOME") "/org-node/node_modules"  ":"
           (getenv "NODE_PATH")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     ;; (typescript . t)
     (js . t)
     (python . t)
     ;; (restclient . t)
     (shell . t)))

  (defun org-babel-execute:typescript (body params)
    (let ((org-babel-js-cmd "npx ts-node < "))
      (org-babel-execute:js body params)))

  (defvar org-babel-js-function-wrapper
    ""
    "Javascript code to print value of body.")
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu"))))

(use-package org-menu
  :defer t
  :ensure (:host github :repo "sheijk/org-menu")
  :bind (("C-c o a" . org-menu)))

(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
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
                           (prettify-symbols-mode)))

(use-package org-fancy-priorities
  :defer t
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "🔥")
                                    (?B . "⬆")
                                    (?C . "❗")
                                    (?D . "⬇")
                                    (?E . "❓")
                                    (?1 . "🔥")
                                    (?2 . "⚡")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-directory "~/Yandex.Disk.localized/Dropbox/org"))

(use-package org-mem
  :ensure (:host github :repo "meedstrom/org-mem"))

(use-package org-node
  :after (org org-mem)
  :bind
  (("C-c n r f" . org-node-find)
   ("C-c n r i" . org-node-insert-link))
  :config 
  (setq org-node-extra-id-dirs `(,(file-truename "~/org-roam")))
  (setq org-node-alter-candidates t)
  (setq org-node-affixation-fn
        (defun @prefix-with-tag (node title)
          "Let NODE's tags prefix TITLE."
          (list title
                (when-let ((tags (org-node-get-tags node)))
                  (propertize (concat "(" (string-join tags ", ") ") ")
                              'face `(:foreground ,\@m-color-main :weight bold :slant italic)))
                nil)))
  (org-node-cache-mode))

(use-package org-node-fakeroam
  :after org-mem
  :defer
  :config
  (setq org-node-creation-fn #'org-node-fakeroam-new-via-roam-capture)
  (setq org-node-slug-fn #'org-node-fakeroam-slugify-via-roam)
  (setq org-node-datestamp-format "%Y%m%d%H%M%S-"))

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

(use-package orgnote
  :ensure (:host github :repo "Artawower/orgnote.el")
  :bind
  (
   ("C-c n p" . orgnote-publish-file)
   ("C-c n F" . orgnote-force-sync)
   ("C-c n S" . orgnote-sync)
   :map org-mode-map
   ("C-c n P" . orgnote-publish-all)
   ("C-c n L" . orgnote-load))
  :custom
  (orgnote-debug-p t)
  (orgnote-execution-script "node /Users/darkawower/projects/pet/orgnote/orgnote-cli/dist/index.js"))

(use-package org-make-toc
  :after org
  :bind (:map org-mode-map
              ("C-c o g" . org-make-toc)))

(use-package ox-gfm
  :defer t
  :ensure (ox-gfm :type git :host github :repo "larstvei/ox-gfm"))

(use-package org-download
  :defer 2
  :hook (dired-mode-hook . org-download-enable)
  :ensure (org-download :type git :host github :repo "abo-abo/org-download")
  :config
  (setq org-download-method 'directory)
  (setq org-download-link-format "[[./%s]]\n")
  (setq org-download-heading-lvl nil))

(use-package org-smart-enter
  :ensure (org-smart-enter :type git :host github :repo "artawower/org-smart-enter.el")
  :hook (org-mode . org-smart-enter-mode)
  :config
  (org-smart-enter-setup))

(use-package org-ql
  :defer t
  :config

  
  (defun my/org-ql-search-files (&optional files query-str)
    "Display ONLY the files where QUERY matches.

FILES is a list of files to search (defaults to `org-agenda-files` if nil).
QUERY-STR is a string in `org-ql` query syntax, e.g.: (tags \"books\").

When called interactively without FILES or QUERY-STR, prompts for a query.
Results are displayed as a clickable list of file names in a dedicated buffer."
    (interactive)
    (let* ((files (or files (org-agenda-files)))
           ;; Convert string to Lisp form
           (sexp (or (and query-str (read query-str))
                     (read (read-string "org-ql query: ")))))
      (let* ((hits (org-ql-select files sexp
                     :action (lambda () (buffer-file-name))))
             ;; Remove duplicates while preserving order
             (uniq (delete-dups (cl-copy-list hits)))
             (buf (get-buffer-create "*org-ql files*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert (format "Files matching %S: %d\n\n" sexp (length uniq)))
          ;; Insert clickable buttons for each file
          (dolist (f uniq)
            (insert-text-button
             (file-name-nondirectory f)
             'follow-link t
             'action (lambda (_btn) (find-file f)))
            (insert "\n"))
          (goto-char (point-min))
          (read-only-mode 1)
          (special-mode))
        (display-buffer buf))))

  ;; Org link handler: [[org-ql-files:(tags "books")]]
  (org-link-set-parameters
   "org-ql-files"
   :follow (lambda (q) (my/org-ql-search-files nil q)))
  
  (org-link-set-parameters
   "org-ql-roam"
   :follow (lambda (query)
             (org-ql-search (org-roam-list-files) (read query)))))

(use-package raindrop
  :defer t
  :bind (("C-c r s" . raindrop-search))
  :ensure (raindrop :host github :repo "artawower/raindrop.el"))

(use-package raindrop-org
  :after raindrop
  :ensure nil
  :defer t)

(use-package raindrop-search
  :ensure nil
  :after raindrop
  :bind (("C-c r s" . raindrop-search))
  :defer t)

(use-package ob-raindrop
  :after (org raindrop)
  :commands (org-babel-execute:raindrop)
  :ensure nil
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(raindrop . t))
    (when (boundp 'org-babel-load-languages)
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))))

(use-package google-translate
  :defer 10
  :bind (:map google-translate-minibuffer-keymap
        ("C-'" . google-translate-next-translation-direction)
        :map meow-normal-state-keymap
        ("\\ t" . google-translate-smooth-translate))
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US ru_RU")
  (jinx-camel-modes '(prog-mode org-mode))
  :bind (("C-c f w" . jinx-correct)
         ("C-c l c" . jinx-languages)
         ("C-c s ]" . jinx-next)
         ("C-c s [" . jinx-previous))
  :config
  (add-to-list 'jinx-camel-modes 'html-ts-mode)
  (add-to-list 'jinx-camel-modes 'html-mode)
  (add-to-list 'jinx-camel-modes 'typescript-ts-mode))

(use-package wakatime-mode
  :defer 2
  :config
  (global-wakatime-mode))

(use-package wakatime-ui
  :after wakatime-mode
  :ensure (wakatime-ui :host github :repo "Artawower/wakatime-ui.el")
  :custom
  (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/af1bfb85-2c8b-44e4-9873-c4a91b512e8d.png")
  :config
  (wakatime-ui-mode))

(use-package tramp
  :ensure nil
  :config
  (setenv "SHELL" "/bin/bash")
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:user@host:")
                     "remote-shell" "/usr/bin/bash"))
  (add-to-list 'tramp-connection-properties
               (list ".*" "locale" "LC_ALL=C")))

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install))

(add-to-list 'display-buffer-alist '("^\\*scratch\\*$" (display-buffer-below-selected) (window-height . 0.4)))
(add-to-list 'display-buffer-alist '("^\\*quicknote\\*$"
                                     (display-buffer-in-side-window)
                                     (inhibit-same-window . t)
                                     (side . bottom)
                                     (window-width . full)
                                     (window-height . 0.3)))

(defun @switch-to-scratch ()
  "Switch to scratch buffer"
  (interactive)
  ;; (persistent-scratch-autosave-mode -1)
  (let* ((buffer-name "*quicknote*")
         (buffer (get-buffer buffer-name)))

    (unless buffer
      (persistent-scratch-setup-default)
      (setq buffer (get-buffer-create buffer-name))
      (persistent-scratch-restore))
    
    (message "current buffer: %s" buffer)
    (with-current-buffer buffer
      (pop-to-buffer buffer)
      ;; (when (equal (s-trim (buffer-substring-no-properties (point-min) (point-max))) "")
      ;;   (message "Persistent scratch restored")
      ;;   (persistent-scratch-setup-default))
      ;; (goto-char (point-max))
      ;; (unless (eq major-mode 'org-mode)
      ;;   (org-mode))
      (org-mode)
      (meow-insert))))

(defun create-persistent-scratch-file-if-not-exist ()
  "Create the persistent-scratch file if it does not already exist."
  (let ((scratch-file (expand-file-name "var/persistent-scratch.el" user-emacs-directory)))
    (unless (file-exists-p scratch-file)
      (make-directory (file-name-directory scratch-file) t)
      (with-temp-file scratch-file
        (insert ";; Persistent scratch file\n")))))

;; Run the function during startup
(create-persistent-scratch-file-if-not-exist)

(defvar @persistent-scratch-last-content nil)

(defun @persistent-scratech-savable-p ()
  "Return non-nil if the current buffer is savable."
  (when-let* ((current-buffer-scratch-p (string= (buffer-name) "*quicknote*"))
              (scratch-buffer (get-buffer "*quicknote*"))
              (scratch-buffer-content (with-current-buffer scratch-buffer
                                        (buffer-string)))
              (scratch-buffer-content-not-empty (not (equal scratch-buffer-content ""))))
    ;; (scratch-buffer-content-changed (not (equal scratch-buffer-content @persistent-scratch-last-content)))
    ;; (scratch-string-not-nil (not (equal scratch-buffer-content "nil"))))

    ;; (setq @persistent-scratch-last-content scratch-buffer-content)
    ;; (message "buffer name %s, buffer content %s, content true? %s"
    ;;          (get-buffer "*quicknote*")
    ;;          (with-current-buffer "*quicknote*"
    ;;            (buffer-string))
    ;;          (with-current-buffer "*quicknote*"
    ;;            (not (equal (buffer-string) ""))))
    t))

(defun @save-scratch-buffer-p ()
  "Return non-nil if the current buffer is the scratch buffer."

  (let ((persistent-scrach-savable-p (@persistent-scratech-savable-p)))
    persistent-scrach-savable-p))

(setq persistent-scratch-scratch-buffer-p-function #'@save-scratch-buffer-p)

(defun @preserve-persistent-scratch ()
  "Preserve the persistent scratch buffer. Make backup."
  (interactive)
  (persistent-scratch-save)
  (persistent-scratch-new-backup))

(use-package persistent-scratch
  ;; :demand t
  :bind (("C-c n n" . @switch-to-scratch))
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

(use-package husky
  :defer 2
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
         ("M-p" . husky-lsp-repeat-consult-search-backward))
  :config
  (require 'web-mode)
  (require 'magit)
  ;; (global-husky-treesit)
  )

(elpaca-wait)
