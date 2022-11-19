;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2020-2022  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Kevin Borling <kborling@protonmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.
;;
;; See my dotfiles: https://git.sr.ht/~protesilaos/dotfiles

;;; Code:

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

(setq large-file-warning-threshold 200000000)

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(set-default-coding-systems 'utf-8)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq frame-resize-pixelwise t)

(setq package-enable-at-startup t)

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;; (setq exec-path (append exec-path '("/opt/homebrew/bin")))
;; (setq exec-path (append exec-path '("~/env/bin")))
;;; early-init.el ends here
