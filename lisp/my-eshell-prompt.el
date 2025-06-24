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

;;;; Prompt faces

;; TODO: This breaks the foreground color of the below faces for
;; themes that set a foreground color for the `highlight' face.
;;
;; Currently, it is not possible to un-inherit a face attribute:
;; https://lists.gnu.org/archive/html/emacs-devel/2019-10/msg00394.html
(defface my-prompt-default-face
  '((t :foreground nil
       :inherit highlight
       :weight bold))
  "Default face for my prompt.")

(defface my-prompt-user-face
  '((t :inherit (my-prompt-default-face term-color-green)))
  "Face used to highlight a non-root user.")

(defface my-prompt-root-face
  '((t :inherit (my-prompt-default-face term-color-red)))
  "Face used to highlight the root user.")

(defface my-prompt-local-hostname-face
  '((t :inherit (my-prompt-default-face term-color-green)))
  "Face used to highlight the local hostname.")

(defface my-prompt-remote-hostname-face
  '((t :inherit (my-prompt-default-face term-color-blue)))
  "Face used to highlight a remote hostname.")

(defface my-prompt-timedate-face
  '((t :inherit (my-prompt-default-face term-color-green)))
  "Face used to highlight the time and date.")

(defface my-prompt-directory-face
  '((t :inherit (my-prompt-default-face)))
  "Face used to highlight the current directory.")

(defface my-prompt-git-branch-face
  '((t :inherit (my-prompt-default-face term-color-blue)))
  "Face used to highlight the git branch.")

(defface my-prompt-venv-face
  '((t :inherit (my-prompt-default-face term-color-green)))
  "Face used to highlight the python venv.")

(defface my-prompt-arrow-face
  '((t :inherit (my-prompt-default-face term-color-blue)))
  "Face used to highlight the arrow right before to input field.")


;;;; Helper functions

(defun my/git-status ()
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process "git" nil t nil "status" "--porcelain"))))

(defun my/git-status--dirty-p ()
  (not (string-blank-p (my/git-status))))


;;;; Prompt segment functions

(defun my/eshell-prompt-user-and-host ()
  "Return the formatted prompt segment for user and host info."
  (let (user host userface hostface)
    (if (epe-remote-p)
        (progn
          (setq user (epe-remote-user))
          (setq host (epe-remote-host))
          (setq hostface 'my-prompt-remote-hostname-face))
      (setq user (user-login-name))
      (setq host system-name)
      (setq hostface 'my-prompt-local-hostname-face))
    (if (string= user "root")
        (setq userface 'my-prompt-root-face)
      (setq userface 'my-prompt-user-face))
    (concat (propertize user 'face userface)
            (propertize "@" 'face 'my-prompt-default-face)
            (propertize host 'face hostface))))

(defun my/eshell-prompt-datetime ()
  "Return the formatted prompt segment for the time and date."
  (propertize (format-time-string "[%H:%M, %d/%m/%y]" (current-time)) 'face 'my-prompt-timedate-face))


(defun my/eshell-prompt-directory ()
  "Return the formatted prompt segment for the current directory."
  (propertize (format " [%s]" (f-abbrev (directory-file-name default-directory))) 'face 'my-prompt-directory-face))

(defun my/eshell-prompt-venv ()
  "Return the formatted prompt segment for venv info."
  (when venv-current-dir
    (let* ((venv-parent-dir (thread-last venv-current-dir
                                         (file-name-parent-directory)
                                         (string-remove-suffix "/")
                                         (file-name-nondirectory)))
           (venv-name (if (string-empty-p venv-current-name)
                          venv-parent-dir
                        venv-current-name)))
      (concat
       (propertize (format "(%s)" venv-name) 'face 'my-prompt-venv-face) "\n"))))

(defun my/eshell-prompt-git ()
  "Return the formatted prompt segment for git info."
  (when (and (not (file-remote-p default-directory))
             (magit-git-repo-p default-directory))
    (let ((branch (or (magit-get-shortname "HEAD"))))
      (concat
       (propertize (format " [%s" branch) 'face 'my-prompt-git-branch-face)
       (when (my/git-status--dirty-p)
         (propertize "*" 'face '(:foreground "red" :inherit my-prompt-default-face)))
       (propertize "]" 'face 'my-prompt-git-branch-face)))))


;;;; The actual prompt function

;;;###autoload
(defun my-eshell-prompt ()
  "Function for generating my eshell prompt.
Should be used as the function for `eshell-prompt-function'"
  (concat
    "\n"
    (my/eshell-prompt-venv)
    (my/eshell-prompt-datetime)
    "\n"
    (my/eshell-prompt-user-and-host)
    (my/eshell-prompt-directory)
    (my/eshell-prompt-git)
    "\n"
    (propertize "\n" 'face 'my-prompt-default-face)
    (propertize " ->" 'face 'my-prompt-arrow-face)
    (propertize " " 'face 'my-prompt-default-face)))

(provide 'my-eshell-prompt)
;;; my-eshell-prompt.el ends here
