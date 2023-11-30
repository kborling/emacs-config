;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.2.0
;; Keywords: configuration
;; URL: https://github.com/kborling/emacs-config
;; Homepage: https://github.com/kborling/emacs-config
;; Package-Requires: ((emacs "29"))

;;; Commentary:

;; Copyright (C) 2023 Kevin Borling
;; My personal Emacs config.

;;; Code:

;; Backups ========================================== ;;

;; Auto-revert mode
(setopt
 auto-revert-interval 0.5
 auto-revert-verbose t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Backup stored in /tmp
(setq
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms
 `((".*" , temporary-file-directory t))

 backup-by-copying t
 version-control t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 create-lockfiles nil)

;; User ============================================= ;;
(setq user-full-name "Kevin Borling"
      user-mail-address "kborling@protonmail.com")

;; Defaults ========================================= ;;
(setq
 frame-title-format '("%b")
 use-short-answers t
 ring-bell-function 'ignore
 visible-bell nil
 sentence-end-double-space nil
 fill-column 100
 column-number-mode t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 set-mark-command-repeat-pop t
 global-mark-ring-max 50000
 bookmark-save-flag 1)

;; (setq-default cursor-type 'hollow)

;; Remember cursor place
(setq
 save-place-file (locate-user-emacs-file "saveplace")
 save-place-forget-unreadable-files t)

(save-place-mode 1)

;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil
              tab-stop-list    ()
              tab-width        2)

(show-paren-mode 1)
;; Treat Camelcase as words
(global-subword-mode 1)

;; Delete marked region when input
(add-hook 'after-init-hook #'delete-selection-mode)

;; Smooth scrolling
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Improve LSP performance
(fset #'jsonrpc--log-event #'ignore)
(add-hook 'focus-out-hook 'garbage-collect)

;; Elpaca ========================================== ;;

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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

(elpaca use-package :elpaca t)
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Install by copying on Windows
(if (eq system-type 'windows-nt)
    (elpaca-no-symlink-mode)
    nil)

(elpaca-wait)

;; Electric Pairs ====================================== ;;

;; make electric-pair-mode work on more brackets
(use-package elec-pair
  :elpaca nil
  :config
  (setq
   electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
   electric-pair-preserve-balance t
   electric-pair-pair
   '((8216 . 8217)
     (8220 . 8221)
     (171 . 187))
   electric-pair-skip-self 'electric-pair-default-skip-self
   electric-pair-skip-whitespace nil
   electric-pair-skip-whitespace-chars '(9 10 32)))
;; (electric-pair-mode -1)

(use-package electric
  :elpaca nil
  :config
  (setq
   electric-quote-context-sensitive t
   electric-quote-paragraph t
   electric-quote-string nil
   electric-quote-replace-double t)

  ;; (electric-quote-mode -1)
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;; Hooks ============================================= ;;

;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Keybindings ======================================= ;;

(let ((map global-map))
  ;; Remove suspend
  (define-key map (kbd "C-z") nil)
  (define-key map (kbd "C-x C-z") nil)

  (define-key map (kbd "C-h C-r") #'restart-emacs)
  (define-key map (kbd "C-c C-r") #'query-replace)

  (define-key map (kbd "C-;") #'comment-line)
  (define-key map (kbd "C-x f") #'project-find-file)
  (define-key map (kbd "C-x /") #'set-fill-column)

  (define-key map (kbd "M-(") #'insert-pair)
  (define-key map (kbd "M-\"") #'insert-pair)
  (define-key map (kbd "M-{") #'insert-pair)
  (define-key map (kbd "M-[") #'insert-pair)

  (define-key map (kbd "C-c C-p") #'previous-buffer)
  (define-key map (kbd "C-c C-n") #'next-buffer)
  (define-key map (kbd "C-o") #'other-window)
  ;; Misc
  (define-key map (kbd "C-x C-b") #'ibuffer)
  (define-key map (kbd "M-z") #'zap-up-to-char)
  (define-key map (kbd "C-z") #'zap-to-char)
  ;; isearch
  (define-key map (kbd "C-s") #'isearch-forward-regexp)
  (define-key map (kbd "C-r") #'isearch-backward-regexp)
  (define-key map (kbd "C-M-s") #'isearch-forward)
  (define-key map (kbd "C-M-r") #'isearch-backward)
  ;; Open stuff
  (define-key map (kbd "C-c o e") #'eshell)
  (define-key map (kbd "C-c o t") #'vterm)
  (define-key map (kbd "C-c o d") #'dired-jump-other-window)
  (define-key map (kbd "C-c o s") #'speedbar)
  ;; Toggle stuff
  (define-key map (kbd "C-c o f") #'toggle-frame-fullscreen))

;; Theming ================================================ ;;

(defvar default-font-size
  (cond
   ((eq system-type 'windows-nt) 100)
   ((eq system-type 'gnu/linux) 110)
   ((eq system-type 'darwin) 130)))

(let ((font "Comic Code"))
  (set-face-attribute 'default nil
                      :family font :weight 'regular :height default-font-size)
  (set-face-attribute 'bold nil
                      :family font :weight 'medium)
  (set-face-attribute 'italic nil
                      :family font :weight 'regular :slant 'italic))
(set-face-attribute 'variable-pitch nil
                    :family "Berkeley Mono Variable" :height (+ default-font-size 20) :weight 'regular)
(set-fontset-font t 'unicode
                  (font-spec :name "Inconsolata Light" :size 16) nil)

;; Custom themes
(use-package doom-themes)

(use-package haki-theme)

(use-package myron-themes
  :elpaca (myron-themes :host github :repo "neeasade/myron-themes" :files ("*.el" "themes/*.el")))

(use-package uwu-theme
  :elpaca (uwu-theme :host github :repo "kborling/uwu-theme" :files ("*.el"))
  :config
  (setq
   uwu-distinct-line-numbers 'nil
   uwu-scale-org-headlines t
   uwu-use-variable-pitch t)
  (load-theme 'uwu t))

;; Recent Files ====================================== ;;

(use-package recentf
  :elpaca nil
  :config
  (setq
   recentf-save-file (locate-user-emacs-file "recentf")
   recentf-max-saved-items 50
   recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))

  (add-hook 'after-init-hook #'recentf-mode))

;; Exec Path ========================================= ;;

(use-package exec-path-from-shell
  :elpaca t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Diminish ========================================== ;;

(use-package diminish
  :config
  (diminish 'subword-mode)
  (diminish 'eldoc-mode))

;; Which Key ========================================= ;;

(use-package which-key
  :defer 0
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Crux ============================================== ;;

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("M-<return>" . crux-smart-open-line-above)
         ("C-x w" . crux-rename-file-and-buffer)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-^" . crux-top-join-line)

         ([(shift return)] . crux-smart-open-line)))

(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

;; Highlight Todo ====================================== ;;

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Rainbow Mode ======================================== ;;

(use-package rainbow-mode
  :diminish
  :hook ((prog-mode) . rainbow-mode))

;; Magit =============================================== ;;

(use-package magit
  :bind (
         ("C-c g g" . magit-status)
         ("C-c g s" . magit-status)
         ("C-c g i" . magit-init)
         ("C-c g c" . magit-clone)
         ("C-c g l" . magit-pull)
         ("C-c g p" . magit-push)
         ("C-c g f" . magit-fetch-all)
         ("C-c g b" . magit-branch)
         ("C-c g d" . magit-diff)
         ("C-c g r" . magit-remote)
         ("C-c g z" . magit-stash)
         ("C-c g Z" . magit-apply)
         ("C-c g t" . #'magit-quick-status)))

(defun magit-status-refresh-buffer-quick ()
  "Refresh the current `magit-status' buffer."
  (magit-insert-section (status)
    (magit-insert-heading "Quick status")
    (insert "\n")
    (magit-insert-error-header)
    (magit-insert-head-branch-header)
    (insert "\n")
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defun magit-quick-status ()
  "Toggle quick magit status."
  (interactive)
  (if (advice-member-p 'magit-status-refresh-buffer-quick 'magit-status-refresh-buffer)
      (progn
        (advice-remove 'magit-status-refresh-buffer 'magit-status-refresh-buffer-quick)
        (message "Quick magit status turned off"))
    (advice-add 'magit-status-refresh-buffer :override 'magit-status-refresh-buffer-quick)
    (message "Quick magit status turned on"))
  (magit-refresh-all))

;; Marginalia ======================================== ;;

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Corfu ============================================= ;;

(use-package corfu
  :elpaca (:files (:defaults "extensions/*"))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)                  ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  (corfu-separator ?\s)            ;; Orderless field separator
  (corfu-quit-no-match 'separator) ;; Don't quit if there is `corfu-separator' inserted
  (corfu-echo-documentation nil)   ;; Disable documentation in the echo area
  (corfu-scroll-margin 4)

  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.2)

  (corfu-history-mode 1)

  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  (defun corfu-move-to-minibuffer ()
    "Use corfu for completions in the minibuffer."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)

  ;; https://github.com/minad/corfu/wiki#continuously-update-the-candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Templates ================================================

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("C-<tab>" . tempel-complete) ;; Alternative tempel-expand
         ("M-+" . tempel-insert)
         ("C-1" . tempel-previous)
         ("C-2" . tempel-next))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package tempel-collection
  :after tempel)

;; TAGS ============================================== ;;

(setq path-to-ctags (locate-file "ctags" exec-path))
(defun create-tags (dir-name)
  "Create tags file using 'DIR-NAME'."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

;; Multiple Cursors ================================== ;;

(use-package multiple-cursors
  :diminish
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-z" . mc/mark-next-like-this)
         ("M-C-z" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)))

;; So Long =========================================== ;;

(use-package so-long
  :config
  (global-so-long-mode 1))

;; Dabbrev =========================================== ;;

(use-package dabbrev
  :elpaca nil
  :config
  (setq
   dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
   dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
   dabbrev-backward-only nil
   dabbrev-case-distinction 'case-replace
   dabbrev-case-fold-search nil
   dabbrev-case-replace 'case-replace
   dabbrev-check-other-buffers t
   dabbrev-eliminate-newlines t
   dabbrev-upcase-means-case-search t)
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "M-\\") #'hippie-expand)
    (define-key map (kbd "C-M-/") #'dabbrev-completion)))

;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Completion Styles ================================= ;;

(use-package minibuffer
  :elpaca nil
  :config
  (setq
   completion-category-defaults nil
   completion-category-overrides nil
   completion-cycle-threshold 2
   completion-flex-nospace nil
   completion-pcm-complete-word-inserts-delimiters nil
   completion-pcm-word-delimiters "-_./:| "
   completion-ignore-case t
   completions-detailed t
   completion-show-inline-help nil)

  ;; Grouping of completions for Emacs 28
  (setq
   completions-group t
   completions-group-sort nil
   completions-group-format
   (concat
    (propertize "    " 'face 'completions-group-separator)
    (propertize " %s " 'face 'completions-group-title)
    (propertize " " 'face 'completions-group-separator
                'display '(space :align-to right)))

   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   ;; enable-recursive-minibuffers t
   read-answer-short t
   resize-mini-windows t
   minibuffer-eldef-shorten-default t
   echo-keystrokes 0.25
   minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq-default case-fold-search t)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;; Modeline ============================================= ;;
(setq
 mode-line-compact nil
 mode-line-percent-position '(-3 "%p")
 mode-line-position-column-line-format '(" %l,%c")
 mode-line-defining-kbd-macr
 (propertize " Macro" 'face 'mode-line-emphasis))

(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))


(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

;; Ibuffer ============================================== ;;

(use-package ibuffer
  :elpaca nil
  :config
  (setq
   ibuffer-expert t
   ibuffer-display-summary nil
   ibuffer-use-other-window nil
   ibuffer-show-empty-filter-groups nil
   ibuffer-movement-cycle nil
   ibuffer-default-sorting-mode 'filename/process
   ibuffer-use-header-line t
   ibuffer-default-shrink-to-minimum-size nil
   ibuffer-format
   '((mark modified read-only locked " "
           (name 40 40 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))
   ibuffer-saved-filter-groups nil
   ibuffer-old-time 48)

  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

;; Winner =============================================== ;;

(use-package winner
  :elpaca nil
  :config
  (add-hook 'after-init-hook #'winner-mode)

  (let ((map global-map))
    (define-key map (kbd "C-c <") #'winner-redo)
    (define-key map (kbd "C-c >") #'winner-undo)))

;; EAT ================================================== ;;

(use-package eat
  :elpaca (eat
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

;; Fussy =============================================== ;;

(use-package fussy
  :config
  (setq fussy-use-cache t)
  (setq fussy-filter-fn 'fussy-filter-default)
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)
  (push 'fussy completion-styles)

  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil)))

  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

;; Fido ================================================ ;;

(use-package icomplete
  :elpaca nil
  :init
  (fido-mode)
  :config
  (setq icomplete-tidy-shadowed-file-names t
        icomplete-show-matches-on-no-input t
        icomplete-compute-delay 0
        icomplete-delay-completions-threshold 50)
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda () (setq-local completion-styles '(fussy basic))))
  (global-set-key (kbd "C-=") 'fido-vertical-mode))

;; IDO =============================================== ;;

(use-package ido
  :elpaca nil
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq
   ido-enable-flex-matching t
   ido-use-filename-at-point 'guess
   ido-use-virtual-buffers t
   ido-use-faces t
   ido-create-new-buffer 'always
   ido-max-prospects 10
   ;; ido-max-directory-size 20
   ido-file-extensions-order '(".org" ".ts" ".js" ".html" ".json" ".scss" ".txt" ".el" ".ini" ".cfg" ".cnf")
   ;; (add-to-list 'ido-ignore-files "\.bak" "")
   ;; (add-to-list 'completion-ignored-extensions ".bak" ".o" ".a" ".so" ".git" "node_modules")
   magit-completing-read-function 'magit-ido-completing-read))

;; Dired ============================================= ;;

(use-package dired
  :elpaca nil
  :config
  (setq
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   delete-by-moving-to-trash t
   dired-dwim-target t
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-make-directory-clickable t
   dired-free-space nil
   dired-isearch-filenames 'dwim
   dired-create-destination-dirs 'ask
   dired-vc-rename-file t
   dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))
   dired-kill-when-opening-new-dired-buffer t
   dired-clean-up-buffers-too t
   dired-clean-confirm-killing-deleted-buffers t
   dired-x-hands-off-my-keys t
   dired-bind-man nil
   dired-bind-info nil
   ;; wdired-allow-to-change-permissions t
   ;; wdired-create-parent-directories t
   image-dired-thumb-size 80
   image-dired-thumb-margin 2
   image-dired-thumb-relief 0
   image-dired-thumbs-per-row 4)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  )

;; Xref ============================================== ;;

(use-package xref
  :elpaca nil
  :config
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-file-name-display 'project-relative
   xref-search-program 'ripgrep))

;; ISearch =========================================== ;;

(use-package isearch
  :elpaca nil
  :config
  (setq
   search-highlight t
   search-whitespace-regexp ".*?"
   isearch-lax-whitespace t
   isearch-regexp-lax-whitespace nil
   isearch-lazy-highlight t
   isearch-lazy-count t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " (%s/%s)"
   isearch-yank-on-move 'shift
   isearch-allow-scroll 'unlimited
   isearch-repeat-on-direction-change t
   lazy-highlight-initial-delay 0.5
   lazy-highlight-no-delay-length 3
   isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

;; SaveHist ========================================== ;;

(use-package savehist
  :elpaca nil
  :config
  (setq
   savehist-file (locate-user-emacs-file "savehist")
   history-length 10000
   history-delete-duplicates t
   savehist-save-minibuffer-history t)

  (add-hook 'after-init-hook #'savehist-mode))

;; SmartScan ========================================== ;;

(use-package smartscan
  :config
  (global-smartscan-mode 1))

;; Consult ============================================ ;;

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("M-X" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-h t" . consult-theme)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x C-r" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ("M-R" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-r" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-fd)
         ;; ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Enable consult previews
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Use `consult-completion-in-region' if fido is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  ;; (setq completion-in-region-function
  ;;       (lambda (&rest args)
  ;;         (apply (if fido-mode
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region)
  ;;                args)))

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)
   consult-line :prompt "Search: "
   :preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Ignore certain directories when using 'consult-find'
  (setq consult-find-args "find . -not ( -wholename */.* -prune -o -name node_modules -prune )")

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Embark ============================================ ;;

(use-package embark
  :bind
  (("C-," . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Eldoc Box ========================================= ;;

(use-package eldoc
  :elpaca nil
  :config
  (global-eldoc-mode 1)
  (setq eldoc-echo-area-use-multiline-p t
        eldoc-idle-delay 0.75))

;; Project ============================================ ;;

(use-package project
  :elpaca nil
  :config
  (setq vc-directory-exclusion-list
        (nconc vc-directory-exclusion-list
               '("node_modules"
                 ".sl")))
  (setq project-vc-extra-root-markers '(".envrc" "package.json" ".project" ".sl")))

;; Eglot ============================================== ;;

(use-package eglot
  :elpaca nil
  :config
  (setq eglot-sync-connect 0
        eglot-send-changes-idle-time 0
        eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  ;; Keybindings
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-.") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c c q") 'eglot-code-action-quickfix)
  (define-key eglot-mode-map (kbd "C-c c e") 'eglot-code-action-extract)
  (define-key eglot-mode-map (kbd "C-c c j") 'eglot-code-action-inline)
  (define-key eglot-mode-map (kbd "C-c c k") 'eglot-code-action-rewrite)

  (define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c c d") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c c t") 'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c c h") 'eldoc)
  ;; (define-key eglot-mode-map (kbd "C-c c d") 'xref-find-definitions))

  ;; Language Servers
  ;; (add-to-list 'eglot-server-programs '(gdscript-mode . ("localhost" 6008)))
  (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp" "-lsp")))
  (add-to-list 'eglot-server-programs '(js-mode . ("quick-lint-js" "--lsp-server")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls" "--stdio")))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rls" "--stdio")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode)
                                        . ("clangd"
                                           "-j=8"
                                           "--log=error"
                                           "--malloc-trim"
                                           "--background-index"
                                           "--clang-tidy"
                                           "--cross-file-rename"
                                           "--completion-style=detailed"
                                           "--pch-storage=memory"
                                           "--header-insertion=never"
                                           "--header-insertion-decorators=0")))

  (defvar node-modules-path
    (let* ((global-prefix (string-trim (shell-command-to-string "npm config get --global prefix")))
           (modules-path (if (eq system-type 'windows-nt)
                             "node_modules"
                           "lib/node_modules")))
      (expand-file-name modules-path global-prefix)))

  ;; FIXME: The LSP starts, but doesn't connect to buffer events.
  (add-to-list 'eglot-server-programs
               `(angular-template-mode . ("ngserver"
                                          "--stdio"
                                          "--ngProbeLocations"
                                          ,node-modules-path
                                          "--tsProbeLocations"
                                          ,node-modules-path
                                          )))

  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Make sure Eldoc will show us all of the feedback at point."
              (setq-local eldoc-documentation-strategy
                          #'eldoc-documentation-compose)))

  (dolist (mode '(css-mode
                  html-mode
                  angular-template-mode
                  js-mode
                  typescript-ts-mode
                  gdscript-mode
                  c-mode
                  c++-mode
                  rust-mode
                  csharp-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'eglot-ensure))
  )

(use-package consult-eglot
  :after (consult eglot))

;; Flymake ========================================= ;;
(use-package flymake
  :elpaca nil
  :config
  (setq
   flymake-fringe-indicator-position 'left-fringe
   flymake-suppress-zero-counters t
   flymake-start-on-flymake-mode t
   flymake-no-changes-timeout 0
   ;; flymake-start-on-save-buffer t
   flymake-proc-compilation-prevents-syntax-check t
   flymake-wrap-around nil
   flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters)
   flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ""))

  (define-key ctl-x-x-map "m" #'flymake-mode)
  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c f s") #'flymake-start)
    (define-key map (kbd "C-c f d") #'flymake-show-buffer-diagnostics)
    (define-key map (kbd "C-c f D") #'flymake-show-project-diagnostics)
    (define-key map (kbd "C-c f n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c f p") #'flymake-goto-prev-error))
  :init
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode)
  (add-hook 'flymake-mode-hook
            (lambda ()
              (setq eldoc-documentation-functions
                    (cons 'flymake-eldoc-function
                          (delq 'flymake-eldoc-function eldoc-documentation-functions))))))

;; Emmet Mode ====================================== ;;

(use-package emmet-mode
  :mode ("\\.html\\'" "\\.cshtml\\'" "\\.tsx\\'")
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :bind (:map emmet-mode-keymap
              ("<C-tab>" . emmet-expand-line))
  :hook ((html-mode . emmet-mode)
         (web-mode . emmet-mode)))

;; Web Mode ======================================== ;;

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'" "\\.cshtml\\'" "\\.astro\\'")
  :config
  (push '(css-mode . css-ts-mode) major-mode-remap-alist))

;; Node Modules ================================= ;;

(use-package add-node-modules-path
  :config
  (dolist (mode '(typescript-ts-mode js-mode))
    (add-hook mode 'add-node-modules-path)))

;; Typescript Mode ================================= ;;

(use-package ob-typescript
  :after org)

(use-package typescript-ts-mode
  :elpaca nil
  :mode ("\\.ts\\'")
  :custom (typescript-ts-mode-indent-offset 4)
  :config
  ;; Add intdentation support
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (setq-local indent-line-function 'js-indent-line))))

;; Treesitter ========================================== ;;

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :elpaca (combobulate :host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c r")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; Angular ============================================= ;;

(use-package angular-mode
  :elpaca (angular-mode :host github :repo "kborling/angular-mode" :files ("*.el"))
  :config
  (defun angular-open-interface ()
    "Open an Angular interface file in the project."
    (interactive)
    (angular-open-file "interface"))
  (global-set-key (kbd "C-c a o f") 'angular-open-interface))

(define-derived-mode angular-template-mode web-mode "Angular"
  "A major mode derived from 'web-mode', for editing angular template files with LSP support.")
;; TODO Mode must manually be set
(add-to-list 'auto-mode-alist '("\\.component.html\\'" . angular-template-mode))

;; Quick-lint =========================================== ;;

;;https://github.com/quick-lint/quick-lint-js/tree/master/plugin/emacs
(use-package flymake-quicklintjs
  :elpaca (flymake-quicklintjs :host github :repo "quick-lint/quick-lint-js" :files ("plugin/emacs/flymake-quicklintjs.el"))
  :config
  (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t))
;; (setq flymake-quicklintjs-experimental-typescript t))

;; EditorConfig ======================================== ;;

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; Rust ========================================== ;;

(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot
        rustic-format-on-save t)

  (add-hook 'rustic-mode-hook 'kdb-rustic-mode-hook))

(defun kdb-rustic-mode-hook ()
  "Run `C-c` `C-c` `C-r` works without having to confirm."
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; C++ =========================================== ;;

(with-eval-after-load 'cc-mode
  (defun c-indent-then-complete ()
    "Make completion work again."
    (interactive)
    (if (= 0 (c-indent-line-or-region))
        (completion-at-point)))
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key map (kbd "<tab>") #'c-indent-then-complete)))

;; Elisp ========================================= ;;

(use-package sly
  :hook ((lisp-mode . sly-symbol-completion-mode))
  :custom (inferior-lisp-program (locate-file "sbcl" exec-path))
  :bind (:map sly-mode-map
              ("M-h" . sly-documentation-lookup))
  :config
  (defun kdb-sly-mrepl (arg)
    "Find or create the first useful REPL for the default connection in a side window."
    (interactive "P")
    (save-excursion
      (sly-mrepl nil))
    (let ((buf (sly-mrepl--find-create (sly-current-connection))))
      (if arg
          (switch-to-buffer buf)
        (pop-to-buffer buf)))))

(use-package package-lint)


;; Godot ========================================= ;;

(use-package gdscript-mode
  :elpaca (gdscript-mode
           :type git
           :host github
           :repo "godotengine/emacs-gdscript-mode"))

;; YAML ========================================== ;;

(use-package yaml-mode
  :mode ("\\.yaml\\'"))

;; JSON ========================================== ;;

(use-package json-mode
  :mode ("\\.json\\'"))

;; Restclient ==================================== ;;

(use-package restclient)
(use-package ob-restclient)

;; Copilot ======================================== ;;

(use-package copilot
  :elpaca (copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (global-set-key (kbd "C-c c p") 'copilot-mode)
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  :bind (:map copilot-completion-map
              ("C-g" . 'copilot-clear-overlay)
              ("<right>" . 'copilot-accept-completion)
              ("C-f" . 'copilot-accept-completion)
              ("M-<right>" . 'copilot-accept-completion-by-word)
              ("M-f" . 'copilot-accept-completion-by-word)
              ("C-e" . 'copilot-accept-completion-by-line)
              ("<end>" . 'copilot-accept-completion-by-line)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion)))

;; Dev Docs ======================================= ;;

(use-package devdocs
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))

;; (use-package devdocs-browser
;;   :after devdocs)

;; Ansi-term ====================================== ;;

(setq explicit-shell-file-name (locate-file "fish" exec-path))
(defadvice kdb-ansi-term (before force-bash)
  "Set the default shell to bash."
  (interactive (list explicit-shell-file-name)))
(ad-activate 'kdb-ansi-term)

(defun kdb-term-exec-hook ()
  "Kill the terminal after exit."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'kdb-term-exec-hook)

;; Paste into term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Ediff ======================================== ;;

(use-package ediff
  :elpaca nil
  :config
  (setq
   ediff-keep-variants nil
   ediff-make-buffers-readonly-at-startup nil
   ediff-merge-revisions-with-ancestor t
   ediff-show-clashes-only t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain))

;; Jinx Spellcheck ============================== ;;

;; See https://github.com/minad/jinx for installing enchant
(use-package jinx
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :config
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook #'jinx-mode)))

;; Org Mode ===================================== ;;

;;; Org Mode
(defun kdb-org-mode-setup ()
  "Setup org mode."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (electric-indent-local-mode -1)
  (setq cursor-type 'bar)
  ;; (auto-fill-mode 1)
  )

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package org
  :elpaca nil
  :commands (org-capture org-agenda)
  :hook (org-mode . kdb-org-mode-setup)
  :config

  (setq
   org-ellipsis "…"
   org-pretty-entities t
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-hide-emphasis-markers t
   org-confirm-babel-evaluate nil
   org-export-with-smart-quotes t
   org-src-window-setup 'current-window
   org-directory "~/org/"
   org-todo-keyword
   '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED" "BLOCKED" "FIXME"))
   org-structure-template-alist
   '(("s" . "src")
     ("E" . "src emacs-lisp")
     ("e" . "example")
     ("q" . "quote")
     ("v" . "verse")
     ("V" . "verbatim")
     ("c" . "center")
     ("C" . "comment"))

   ;; code blocks
   org-confirm-babel-evaluate nil
   org-src-window-setup 'current-window
   org-edit-src-persistent-message nil
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 0
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-export-with-toc t
   org-export-headline-levels 8
   org-export-dispatch-use-expert-ui nil
   org-html-htmlize-output-type nil
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (restclient . t)
     (python . t))))

;; Org Files
(defun kdb-todo-visit ()
  "Load ~/.org/todo.org for editing."
  (interactive)
  (find-file (concat org-directory "todo.org")))
(global-set-key (kbd "C-c e t") 'kdb-todo-visit)

(defun kdb-org-find-file ()
  "Find file for org mode directory."
  (interactive)
  (find-file org-directory))
(global-set-key (kbd "C-c e f") 'kdb-org-find-file)

;; Org Roam ===================================== ;;

(use-package org-roam
  ;; :if (or (eq system-type 'gnu/linux)
  ;;         (eq system-type 'darwin))
  :custom
  (org-roam-directory (file-truename "~/roam"))
  ;; (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c d c" . org-roam-dailies-capture-today)
         ("C-c d t" . org-roam-dailies-goto-today)
         ("C-c d p" . org-roam-dailies-goto-previous-note)
         ("C-c d n" . org-roam-dailies-goto-next-note)
         ("C-c d C" . org-roam-dailies-capture-date)
         ("C-c d d" . org-roam-dailies-goto-date))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  ;; :if (or (eq system-type 'gnu/linux)
  ;;         (eq system-type 'darwin))
  :elpaca (org-roam-ui :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Config ======================================= ;;

(defun config-visit ()
  "Load ~/.emacs.d/init.el for editing."
  (interactive)
  (find-file (expand-file-name (locate-user-emacs-file "init.el"))))
(global-set-key (kbd "C-c e v") 'config-visit)

(defun config-reload ()
  "Reload ~/.emacs.d/init.el at runtime."
  (interactive)
  (load-file (expand-file-name (locate-user-emacs-file "init.el"))))
(global-set-key (kbd "C-c e r") 'config-reload)

;; Utilities ====================================== ;;

(defun choose-and-set-font ()
  "Choose a font and font size interactively and set them as the default font."
  (interactive)
  (let* ((font-list (split-string (shell-command-to-string "fc-list : family") "\n" t))
         (chosen-font (completing-read "Select a font: " font-list nil t))
         (font-size (* (string-to-number (read-string "Enter a font size (e.g., 10, 12, 14, ...): ")) 10)))
    (if (and (member chosen-font font-list) (>= font-size 10))
        (progn
          (let ((font-height font-size))
            (set-face-attribute 'default nil
                                :family chosen-font :weight 'regular :height font-height)
            (set-face-attribute 'bold nil
                                :family chosen-font :weight 'medium)
            (set-face-attribute 'italic nil
                                :family chosen-font :weight 'regular :slant 'italic))
          (message "Font set to %s" chosen-font))
      (message "Invalid font choice: %s" chosen-font))))

(global-set-key (kbd "C-c t f") 'choose-and-set-font)

;; Buffers ======================================== ;;

;; Hide useless buffers
;; (require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")

(use-package uniquify
  :elpaca nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun kill-buffer-other-window ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1))
(global-set-key (kbd "C-x M-k") 'kill-buffer-other-window)

(defun format-current-buffer ()
  "Format the current buffer while maintaining cursor position."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))
(global-set-key (kbd "<backtab>") 'format-current-buffer)

(defun copy-whole-buffer ()
  "Copy the current buffer while maintaining cursor position."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (push-mark (point-max) nil t)
    (copy-region-as-kill 1 (buffer-size))))

(global-set-key (kbd "C-x c") 'copy-whole-buffer)

;; Popups ========================================= ;;

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Compile-Log\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "^\\*cargo"
          "^\\*rust"
          "^\\*lsp"
          "\\*npm\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Window Management ================================== ;;

(use-package windmove
  :elpaca nil
  :config
  (global-set-key (kbd "C-S-h") 'windmove-left)
  (global-set-key (kbd "C-S-s") 'windmove-right)
  (global-set-key (kbd "C-S-t") 'windmove-up)
  (global-set-key (kbd "C-S-n") 'windmove-down)
  (global-set-key (kbd "C-M-S-h") 'windmove-swap-states-left)
  (global-set-key (kbd "C-M-S-s") 'windmove-swap-states-right)
  (global-set-key (kbd "C-M-S-t") 'windmove-swap-states-up)
  (global-set-key (kbd "C-M-S-n") 'windmove-swap-states-down))

;; (defun split-and-follow-horizontally ()
;;   "Focus follows horizontal split."
;;   (interactive)
;;   (split-window-below)
;;   (balance-windows)
;;   (other-window 1))
;; (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

;; (defun split-and-follow-vertically ()
;;   "Focus follows vertical split."
;;   (interactive)
;;   (split-window-right)
;;   (balance-windows)
;;   (other-window 1))
;; (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Emacs ============================================= ;;

(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (advice-add #'consult-completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  ;; (setq enable-recursive-minibuffers t)
  :config
  (setq
   window-divider-default-right-width 1
   window-divider-default-bottom-width 1
   window-divider-default-places t)
  (window-divider-mode -1))

;; EWW =============================================== ;;

(use-package eww
  :elpaca nil
  :config
  (setq
   browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
   shr-use-fonts  nil                          ; No special fonts
   shr-indentation 2                           ; Left-side margin
   shr-width 120                                ; Fold text to 70 columns
   shr-use-colors nil             ; t is bad for accessibility
   shr-max-image-proportion 0.6
   shr-image-animate nil          ; No GIFs, thank you!
   shr-discard-aria-hidden t
   shr-cookie-policy nil
   eww-restore-desktop t
   eww-desktop-remove-duplicates t
   eww-header-line-format nil
   eww-retrieve-command nil
   eww-search-prefix "https://duckduckgo.com/html/?q="
   eww-download-directory (expand-file-name "~/Downloads")
   eww-suggest-uris
   '(eww-links-at-point
     thing-at-point-url-at-point)
   eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/")
   eww-history-limit 150
   eww-use-external-browser-for-content-type
   "\\`\\(video/\\|audio\\)" ; On GNU/Linux check your mimeapps.list
   eww-browse-url-new-window-is-tab nil
   eww-form-checkbox-selected-symbol "[X]"
   eww-form-checkbox-symbol "[ ]"))

;; Olivetti ========================================== ;;
(defvar-local old-mode-line-format nil)

(use-package olivetti
  :ensure
  :diminish
  :config
  (setq
   olivetti-body-width 0.65
   olivetti-minimum-body-width 120
   olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode kdb-olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters."
    :init-value nil
    :global nil
    (if kdb-olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (unless (derived-mode-p 'prog-mode)
            (setq mode-line-format nil)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (unless (derived-mode-p 'prog-mode)
        (setq mode-line-format old-mode-line-format))))

  :bind ("C-c t m" . kdb-olivetti-mode))

;; Macos ========================================== ;;

(when (equal system-type 'darwin)
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'none)
  (setq ns-pop-up-frames nil)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-_") 'ns-do-hide-others))

;; Custom ========================================= ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-popupinfo ((t (:inherit corfu-default :height 1.0)))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
