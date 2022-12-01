;;; init.el --- My personal emacs config  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Version: 1.1.0
;; Keywords: configuration
;; URL: https://gist.github.com/kborling/13f2300e60ae4878d5d96f5f4d041664
;; Homepage: https://gist.github.com/kborling/13f2300e60ae4878d5d96f5f4d041664
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Copyright (C) 2022 Kevin Borling
;; My personal Emacs config

;;; Code:

;; Backups ========================================== ;;

;; Auto-revert mode
(setopt auto-revert-interval 0.5)
(setopt auto-revert-verbose t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Backup stored in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" , temporary-file-directory t)))

(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

;; User ============================================= ;;
(setq user-full-name "Kevin Borling"
      user-mail-address "kborling@protonmail.com")

;; Defaults ========================================= ;;
(setq frame-title-format '("%b"))

(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(setq fill-column 100)

(show-paren-mode 1)
;; Treat Camelcase as words
(global-subword-mode 1)
;; Remember cursor place
(setq save-place-file (locate-user-emacs-file "saveplace"))
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)
;; Remember my bookmarks
(setq bookmark-save-flag 1)
;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil
              tab-stop-list    ()
              tab-width        2)
(setq sentence-end-double-space nil)
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

(setq ; save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t)

;; Global mark ring
(setq global-mark-ring-max 50000)

;; Delete marked region when input
(add-hook 'after-init-hook #'delete-selection-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Electric Pairs ====================================== ;;

;; make electric-pair-mode work on more brackets
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq electric-pair-preserve-balance t)
(setq electric-pair-pairs
      '((8216 . 8217)
        (8220 . 8221)
        (171 . 187)))
(setq electric-pair-skip-self 'electric-pair-default-skip-self)
(setq electric-pair-skip-whitespace nil)
(setq electric-pair-skip-whitespace-chars '(9 10 32))
(setq electric-quote-context-sensitive t)
(setq electric-quote-paragraph t)
(setq electric-quote-string nil)
(setq electric-quote-replace-double t)
(electric-pair-mode -1)
(electric-quote-mode -1)
(electric-indent-mode -1)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)

;; Hooks ============================================= ;;

;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Package ========================================== ;;

;; Initialize package sources
(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

(setq package-pinned-packages
      '((corfu . "elpa-devel")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Keybindings ======================================= ;;

(let ((map global-map))
  ;; Remove suspend
  (define-key map (kbd "C-z") nil)
  (define-key map (kbd "C-x C-z") nil)

  (define-key map (kbd "C-;") #'comment-line)

  ;; (define-key map (kbd "C-t") #'other-window)
  ;; (define-key map (kbd "C-c C-<tab>") #'next-window)
  (define-key map (kbd "C-c C-p") #'previous-buffer)
  (define-key map (kbd "C-c C-n") #'next-buffer)
  ;; Misc
  (define-key map (kbd "C-x C-b") #'ibuffer)
  (define-key map (kbd "M-z") #'zap-up-to-char)
  ;; Isearch
  (define-key map (kbd "C-s") #'isearch-forward-regexp)
  (define-key map (kbd "C-r") #'isearch-backward-regexp)
  (define-key map (kbd "C-M-s") #'isearch-forward)
  (define-key map (kbd "C-M-r") #'isearch-backward)
  ;; Open applications
  (define-key map (kbd "C-c o e") #'eshell)
  (define-key map (kbd "C-c o d") #'dired)
  (define-key map (kbd "C-c o f") #'treemacs)
  )

;; Theming ================================================ ;;

(set-face-attribute 'default nil
                    :family "Comic Code" :weight 'regular :height 140)
(set-face-attribute 'bold nil
                    :family "Comic Code" :weight 'bold)
(set-face-attribute 'italic nil
                    :family "Comic Code" :weight 'regular :slant 'italic)
(set-face-attribute 'variable-pitch nil
                    :family "Cantarell" :height 130 :weight 'regular)
(set-fontset-font t 'unicode
                  (font-spec :name "Inconsolata Light" :size 16) nil)
;; (set-fontset-font t '(#xe000 . #xffdd)
;;     (font-spec :name "LiberationMono Nerd Font" :size 12) nil)

;; Custom themes
(use-package uwu-theme
  :config
  (setq uwu-distinct-line-numbers 'nil))
(use-package ef-themes)
(load-theme 'uwu t)

;; Frame ============================================== ;;

;; Make frame transparency overridable
(defvar kdb-frame-transparency '(96 . 96))
;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha kdb-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,kdb-frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Recent Files ======================================= ;;

(require 'recentf)
;; ;; enable recent files mode.
(setq recentf-save-file (locate-user-emacs-file "recentf"))
(setq recentf-max-saved-items 50)
(setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
(add-hook 'after-init-hook #'recentf-mode)

;; Exec Path ====================================== ;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Diminish ======================================= ;;

(use-package diminish
  :ensure t
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

;; Expand Region ====================================== ;;

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

;; Wrap Region ======================================== ;;

(use-package wrap-region
  :diminish
  :config
  (wrap-region-mode t))

;; Crux ============================================== ;;

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ;; ("C-k" . crux-smart-kill-line)
         ("M-<return>" . crux-smart-open-line-above)
         ("C-x w" . crux-rename-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ([(shift return)] . crux-smart-open-line)))

(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

;; Highlight Todo ====================================== ;;

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Magit ============================================== ;;

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
         ;; ("C-c g r" . magit-remote)
         ("C-c g z" . magit-stash)
         ("C-c g Z" . magit-apply)
         ))

;; Marginalia ======================================== ;;

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Corfu ============================================= ;;

(use-package corfu
  :init
  (global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 4)

  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.0)
  ;; (corfu-indexed-mode 1)
  (corfu-history-mode 1)

  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

;;   (defun contrib/corfu-enable-always-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if Vertico is not active.
;; Useful for prompts such as `eval-expression' and `shell-command'."
;;     (unless (bound-and-true-p vertico--input)
;;       (corfu-mode 1)))

;;   (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

;; Add extensions
(use-package cape
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

;; Templates ================================================

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

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
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Snippets =================================================

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)

;; (yas-global-mode 1)

;; TAGS ============================================== ;;

(setq path-to-ctags "/opt/homebrew/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file using 'DIR-NAME'."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;; Multiple Cursors ================================== ;;

(use-package multiple-cursors
  :ensure t
  :diminish
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-z" . mc/mark-next-like-this)
         ("M-C-z" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/mark-edit-lines)
         ))

;; Repeat ============================================ ;;

(use-package repeat
  :ensure t
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t))

;; So Long =========================================== ;;

(use-package so-long
  :ensure t
  :config
  (global-so-long-mode 1))

;; Dabbrev =========================================== ;;

(require 'dabbrev)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines t)
(setq dabbrev-upcase-means-case-search t)
(let ((map global-map))
  (define-key map (kbd "M-/") #'dabbrev-expand)
  (define-key map (kbd "C-M-/") #'dabbrev-completion))

;; Orderless ========================================= ;;

(use-package orderless
  :ensure t
  :config
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles
        '( orderless-prefixes orderless-initialism
           orderless-flex orderless-regexp))
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

;; Completion Styles ================================= ;;

;; Set completion styles
(setq completion-styles
      '(basic orderless))
;; '(basic substring initials flex partial-completion orderless))
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles . (basic substring partial-completion orderless)))
        (project-file (styles . (basic substring partial-completion orderless)))
        (imenu (styles . (basic substring orderless)))
        (kill-ring (styles . (basic substring orderless)))
        (consult-location (styles . (basic substring orderless)))))
(setq completion-cycle-threshold 2)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters nil)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-ignore-case t)
(setq completions-detailed t)
(setq-default case-fold-search t)   ; For general regexp

;; Grouping of completions for Emacs 28
(setq completions-group t)
(setq completions-group-sort nil)
(setq completions-group-format
      (concat
       (propertize "    " 'face 'completions-group-separator)
       (propertize " %s " 'face 'completions-group-title)
       (propertize " " 'face 'completions-group-separator
                   'display '(space :align-to right))))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq completion-show-inline-help nil)

(setq completions-detailed t)

;; Ibuffer ============================================== ;;

(setq ibuffer-expert t)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-other-window nil)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-movement-cycle nil)
(setq ibuffer-default-sorting-mode 'filename/process)
(setq ibuffer-use-header-line t)
(setq ibuffer-default-shrink-to-minimum-size nil)
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 40 40 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
(setq ibuffer-saved-filter-groups nil)
(setq ibuffer-old-time 48)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)

;; Minibuffer =========================================== ;;
(setq completion-cycle-threshold 2)
(setq completion-flex-nospace nil) ; though I don't use the built-in `flex' style...
(setq completion-pcm-complete-word-inserts-delimiters nil)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-ignore-case t)
(setq completions-detailed t)
(setq-default case-fold-search t)   ; For general regexp

;; Grouping of completions for Emacs 28
(setq completions-group t)
(setq completions-group-sort nil)
(setq completions-group-format
      (concat
       (propertize "    " 'face 'completions-group-separator)
       (propertize " %s " 'face 'completions-group-title)
       (propertize " " 'face 'completions-group-separator
                   'display '(space :align-to right))))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq enable-recursive-minibuffers t)
(setq read-answer-short t) ; also check `use-short-answers' for Emacs28
(setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)

(setq echo-keystrokes 0.25)           ; from the C source code

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; Modeline ============================================= ;;

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)            ; Emacs 28
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

;; Winner =============================================== ;;

(add-hook 'after-init-hook #'winner-mode)

(let ((map global-map))
  (define-key map (kbd "C-c <") #'winner-redo)
  (define-key map (kbd "C-c >") #'winner-undo))

;; Vterm ================================================ ;;
(use-package vterm
  :ensure t)

;; FIDO/IComplete ===================================== ;;

;; (fido-mode)
;; (fido-vertical-mode 1)
;; (setq icomplete-compute-delay 0)

;; ;; (setq icomplete-in-buffer 1)
;; ;; (define-key map (kbd "RET") 'icomplete-vertical-goto-last)

;; (global-set-key (kbd "C-=") 'fido-vertical-mode)

;; Vertico =========================================== ;;
(use-package vertico
  :init
  (vertico-mode)
  ;; (vertico-unobtrusive-mode)
  (vertico-reverse-mode)
  ;; different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  ;; :bind
  ;; ("C-s" . vertico-next)
  ;; ("C-r" . vertico-previous)

  (define-minor-mode kdb/vertico-reverse-mode
    "Toggle between `vertico-reverse-mode' and 'vertico-flat-mode'."
    :init-value nil
    :global nil
    :require 'vectico-mode
    :diminish kdb/vertico-reverse-mode
    (if kdb/vertico-reverse-mode
        (progn
          (vertico-reverse-mode)
          (vertico-flat-mode -1)
          )
      (vertico-reverse-mode -1)
      (vertico-flat-mode)
      )
    (message " "))

  :bind ("C-=" . kdb/vertico-reverse-mode)
  )

;; Dired ============================================= ;;

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches
      "-AGFhlv --group-directories-first --time-style=long-iso")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
(setq dired-make-directory-clickable t) ; Emacs 29.1
(setq dired-free-space nil) ; Emacs 29.1

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(setq dired-isearch-filenames 'dwim)
;; The following variables were introduced in Emacs 27.1
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)
;; And this is for Emacs 28
(setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
(setq dired-bind-man nil)
(setq dired-bind-info nil)

(setq wdired-allow-to-change-permissions t)
(setq wdired-create-parent-directories t)

(setq image-dired-thumb-size 80)
(setq image-dired-thumb-margin 2)
(setq image-dired-thumb-relief 0)
(setq image-dired-thumbs-per-row 4)

;; Xref ============================================== ;;

(setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
(setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
(setq xref-file-name-display 'project-relative)
(setq xref-search-program 'ripgrep)

;; ISearch =========================================== ;;
(setq search-highlight t)
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-highlight t)
;; All of the following variables were introduced in Emacs 27.1.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll 'unlimited)
;; These variables are from Emacs 28
(setq isearch-repeat-on-direction-change t)
(setq lazy-highlight-initial-delay 0.5)
(setq lazy-highlight-no-delay-length 3)
(setq isearch-wrap-pause t)

(define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
(let ((map isearch-mode-map))
  (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
  (define-key map (kbd "M-/") #'isearch-complete))

;; SaveHist ========================================== ;;

(require 'savehist)
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

;; Consult ============================================ ;;

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-h t" . consult-theme)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x C-r" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
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

  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Enable consult previews
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; Embark ============================================ ;;

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Eldoc Box ========================================= ;;

(global-eldoc-mode 1)
(setq eldoc-echo-area-use-multiline-p t)

(use-package eldoc-box
  :after eldoc
  :diminish
  :config
  (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)
  )

;; LSP Mode =========================================== ;;

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c c")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-headerline-breadcrumb-enable nil)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  ;; Fix for completions with corfu
  (setq lsp-completion-provider :none)
  (defun lsp-mode-use-orderless ()
    "Set LSP mode to use orderless."
    (setf (alist-get 'styles
		                 (alist-get 'lsp-capf completion-category-defaults))
	        '(orderless)))

  ;; (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  (setq lsp-eslint-auto-fix-on-save t)

  :custom
  ;; Rust settings
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)

  :hook
  (lsp-completion-mode . lsp-mode-use-orderless)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)

  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package consult-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

;; Flycheck ========================================= ;;

(use-package consult-flycheck)

(use-package flycheck
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (setq flycheck-javascript-eslint-executable "eslint_d")

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disgabled-checkers '(haskell-stack-ghc)))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

;; Emmet Mode ====================================== ;;

(use-package emmet-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.cshtml\\'" "\\.tsx\\'")
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :bind (:map emmet-mode-keymap
              ("<C-tab>" . emmet-expand-line))
  :hook ((css-mode web-mode html-mode vue-mode) . emmet-mode))

;; Eslint ========================================== ;;

;; (use-package eslintd-fix
;;   :config
;;   ;; (setq eslintd-fix-executable "~/Projects/angular-tests/node_modules/.bin/eslint_d")
;;   (add-hook 'js2-mode-hook 'eslintd-fix-mode)
;;   (add-hook 'typescript-mode-hook 'eslintd-fix-mode))

;; Lua Mode ======================================== ;;

(use-package lua-mode
  :mode ("\\.lua\\'"))

;; Web Mode ======================================== ;;

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'" "\\.cshtml\\'" "\\.astro\\'"))

;; Javascript Mode ================================= ;;

(use-package js2-mode
  :mode ("\\.js\\'")
  ;; :hook (js2-mode . lsp-deferred)
  :config
  ;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js2-mode-map (kbd "C-\\") #'js2r-log-this)
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook (lambda ()
                             (setup-tide-mode)))
  )

(use-package js2-refactor)
(use-package xref-js2
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  )

;; Typescript Mode ================================= ;;

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))
;; :config
;; (setq typescript-indent-level 4))

(use-package tide
  :ensure t
  :bind (("C-c C-." . tide-documentation-at-point))
  :diminish)

(defun setup-tide-mode ()
  "Setup 'tide-mode' for js/ts."
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode 1))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package ob-typescript
  :after org)


;; EditorConfig ======================================== ;;

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

;; TreeSitter ========================================== ;;

(use-package tree-sitter
  :ensure t
  :diminish
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  )
(use-package tree-sitter-indent
  :ensure t
  :after tree-sitter
  )

;; Csharp ========================================== ;;

;; (use-package csharp-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

;; Rust ========================================== ;;

(use-package rust-mode
  :mode ("\\.rs\\'")
  ;; :hook (rust-mode . lsp-deferred)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  )

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'kdb/rustic-mode-hook))

(defun kdb/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Go ============================================ ;;

(use-package go-mode
  :mode ("\\.go\\'"))

;; Elisp ========================================= ;;

;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(use-package slime
  :ensure t
  :config
  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package package-lint)

;; YAML ========================================== ;;

(use-package yaml-mode)

;; JSON ========================================== ;;

(use-package json-mode)

;; Restclient ==================================== ;;

(use-package restclient)
(use-package ob-restclient)

;; Dev Docs ======================================= ;;

(use-package devdocs
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))

;; (use-package devdocs-browser
;;   :after devdocs)

;; Dash Docs ======================================= ;;

;; (use-package dash-docs)
;; (use-package consult-dash
;;   :bind (("C-h D" . consult-dash))
;;   :config
;;   ;; Use the symbol at point as initial search term
;;   (consult-customize consult-dash :initial (thing-at-point 'symbol)))

;; Emacs Application Framework ===================== ;;

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   :config
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-browser)
;;   (setq browse-url-browser-function 'eaf-open-browser)
;;   (setq eaf-browse-blank-page-url "https://duckduckgo.com")
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

;; Ansi-term ====================================== ;;

(setq explicit-shell-file-name "/bin/zsh")
(defadvice ansi-term (before force-bash)
  "Set the default shell to bash."
  (interactive (list explicit-shell-file-name)))
(ad-activate 'ansi-term)

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

(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Org Mode ===================================== ;;

;;; Org Mode
(defun kdb/org-mode-setup ()
  "Setup org mode."
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1)
  (electric-indent-local-mode -1)
  ;; (auto-fill-mode 1)
  )

;; (add-hook 'org-mode-hook 'org-indent-mode)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . kdb/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-with-smart-quotes t)
  (setq org-src-window-setup 'current-window)
  (setq org-directory "~/org/")
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED" "BLOCKED" "FIXME")))
  (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  ;; (setq org-todo-keyword-faces
  ;;     '(("TODO" . org-warning) ("IN PROGRESS" . "yellow")
  ;;       ("BLOCKED" . (:foreground "blue" :weight bold))))

  ;; code blocks
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

  ;; export
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)
  (setq org-export-backends '(html texinfo md))
  )

;; Org Files

(defun todo-visit ()
  "Load ~/.org/todo.org for editing."
  (interactive)
  (find-file "~/org/todo.org"))
(global-set-key (kbd "C-c e t") 'todo-visit)

;; PDF Tools ===================================== ;;

;; (use-package pdf-tools
;;   :config
;;   ;; initialise
;;   (pdf-tools-install)
;;   ;; open pdfs scaled to fit page
;;   (setq-default pdf-view-display-size 'fit-page)
;;   ;; automatically annotate highlights
;;   (setq pdf-annot-activate-created-annotations t)
;;   ;; use normal isearch
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; Config ======================================= ;;

(defun config-visit ()
  "Load ~/.emacs.d/init.el for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e v") 'config-visit)

(defun config-reload ()
  "Reload ~/.emacs.custom/init.el at runtime."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e r") 'config-reload)

;; Buffers ======================================== ;;

;; Hide useless buffers
;; (require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")

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

(defun indent-current-buffer ()
  "Indent the current buffer while maintaining cursor position."
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "<backtab>") 'indent-current-buffer)

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
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Compile-Log\\*"
          "\\*rustfmt\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Window Splits ================================== ;;

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

;; Screenshots ==================================== ;;

;; Take screenshot of entire screen
(defun kdb/take-screenshot ()
  "Take a fullscreen screenshot of the current workspace."
  (interactive)
  (when window-system
    (cl-loop for i downfrom 3 to 1 do
             (progn
               (message (concat (number-to-string i) "..."))
               (sit-for 1)))
    (message "Cheese!")
    (sit-for 1)
    (start-process "screenshot" nil "import" "-window" "root"
                   (concat (getenv "HOME") "/" (subseq (number-to-string (float-time)) 0 10) ".png"))
    (message "Screenshot taken!")))

;; Keybindings
(global-set-key (kbd "<print>") 'kdb/take-screenshot)

;; Take screenshot of region
(defun kdb/take-screenshot-region ()
  "Takes a screenshot of a region selected by the user."
  (interactive)
  (when window-system
    (call-process "import" nil nil nil ".newScreen.png")
    (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1"
                  (concat (getenv "HOME") "/" (subseq (number-to-string (float-time)) 0 10) ".png"))
    (call-process "rm" nil nil nil ".newScreen.png")))

;; Keybindings
(global-set-key (kbd "<Scroll_Lock>") 'kdb/take-screenshot-region)


;; Emacs ============================================= ;;

(use-package emacs
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
  (setq enable-recursive-minibuffers t)
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places t)
  (window-divider-mode -1))


;; EWW =============================================== ;;

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
 eww-form-checkbox-symbol "[ ]")

;; Olivetti ========================================== ;;
(use-package olivetti
  :ensure
  :diminish
  :config
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 120)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode kdb/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `kdb/hidden-mode-line-mode').  The
default typeface is set to a proportionately-spaced family,
except for programming modes (see `kdb/variable-pitch-mode').
The cursor becomes a blinking bar, per `kdb/cursor-type-mode'."
    :init-value nil
    :global nil
    (if kdb/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          ;; (kdb/variable-pitch-mode 1)
          ;; (kdb/cursor-type-mode 1)
          (unless (derived-mode-p 'prog-mode)
            (kdb/hidden-mode-line-mode 1))
          (window-divider-mode 1))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      ;; (kdb/variable-pitch-mode -1)
      ;; (kdb/cursor-type-mode -1)
      (unless (derived-mode-p 'prog-mode)
        (kdb/hidden-mode-line-mode -1))
      (window-divider-mode -1)))

  :bind ("C-c m" . kdb/olivetti-mode))

;; Custom ========================================= ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(devdocs popper package-lint uwu-theme rustic go-mode ob-typescript flycheck-inline corfu consult-flycheck consult-lsp lsp-ui lsp-mode lua-mode wrap-region exec-path-from-shell desktop-environment eldoc-box editorconfig tempel yasnippet-snippets yas-snippet consult-dash dash-docs cape embark-consult embark tree-sitter-indent tree-sitter-langs tree-sitter ef-themes vterm json-mode yaml-mode orderless marginalia olivetti vertico ob-restclient restclient slime rust-mode tide typescript-mode xref-js2 js2-refactor js2-mode web-mode emmet-mode consult multiple-cursors magit hl-todo crux expand-region which-key diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-popupinfo ((t (:inherit corfu-default :height 1.0)))))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
