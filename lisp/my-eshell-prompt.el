;;; my-eshell-prompt.el --- My eshell prompt -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lars Rustand.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Lars Rustand
;; URL: https://github.com/lrustand/emacs-config
;; Version: 0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;;; Change Log:

;;; Code:


(require 'eshell-prompt-extras)
(require 'my-text-property-helpers)


(defvar my-eshell-prompt-regexp "^ -> ")
(setq eshell-prompt-regexp my-eshell-prompt-regexp)


(defun my/git-status ()
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process "git" nil t nil "status" "--porcelain"))))

(defun my/git-status--dirty-p ()
  (not (string-blank-p (my/git-status))))


;;;###autoload
(defun my-eshell-prompt ()
  "Function for generating my eshell prompt.
Should be used as the function for `eshell-prompt-function'"
  (let* ((green (face-foreground 'term-color-green))
         (red (face-foreground 'term-color-red))
         (black (face-foreground 'term-color-black))
         (blue (face-foreground 'term-color-blue))
         (bright-black (face-foreground 'term-color-bright-black))
         (prompt-bg black)
         (username-fg (if (= (user-uid) 0) red green))
         (username-face `(:foreground ,username-fg
                                      :background ,prompt-bg
                                      :weight bold))
         (hostname-face `(:foreground ,green
                                      :background ,prompt-bg
                                      :weight bold))
         (timedate-face `(:foreground ,green
                                      :background ,prompt-bg
                                      :weight bold))
         (git-branch-face `(:foreground ,blue
                                        :background ,prompt-bg))
         (default-prompt-face `(:foreground unspecified
                                            :background ,black
                                            :weight bold)))
    (concat
     "\n"
     (my/eshell-prompt-venv)
     (propertize (format-time-string "[%H:%M, %d/%m/%y]" (current-time)) 'face timedate-face)
     "\n"
     ;;(propertize (user-login-name) 'face username-face)
     ;;(propertize "@" 'face default-prompt-face)
     ;;(propertize (system-name) 'face hostname-face)
     (my/eshell-prompt-user-and-host)
     (propertize (format " [%s]" (f-abbrev default-directory)) 'face default-prompt-face)
     (when (magit-get-current-branch)
       (concat
        (propertize (format " [%s" (magit-get-current-branch)) 'face git-branch-face)
        (when (my/git-status--dirty-p)
          (propertize "*" 'face `(:foreground "red" :background ,prompt-bg :bold)))
        (propertize "]" 'face git-branch-face)))
     "\n"
     (propertize "\n" 'face default-prompt-face)
     (propertize " ->" 'face `(:foreground ,blue :background ,prompt-bg :bold))
     (propertize " " 'face default-prompt-face))))

(defun my/eshell-prompt-user-and-host ()
  (my/with-bold
  (my/with-background 'term-color-black
  (my/with-foreground 'term-color-green
    (if (epe-remote-p)
        (concat (epe-remote-user) "@" (epe-remote-host))
      (concat (user-login-name) "@" system-name))))))

(defun my/eshell-prompt-venv ()
  (when venv-current-dir
    (let* ((venv-parent-dir (thread-last venv-current-dir
                                         (file-name-parent-directory)
                                         (string-remove-suffix "/")
                                         (file-name-nondirectory)))
           (venv-name (if (string-empty-p venv-current-name)
                          venv-parent-dir
                        venv-current-name)))
      (concat
       (thread-last (format "(%s)" venv-name)
                    (my/with-bold)
                    (my/with-background 'term-color-black)
                    (my/with-foreground 'term-color-green))
       "\n"))))

(provide 'my-eshell-prompt)
;;; my-eshell-prompt.el ends here
