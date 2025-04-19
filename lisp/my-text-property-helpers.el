;;; my-text-property-helpers.el --- Text property helper functions -*- lexical-binding: t; -*-

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
;; URL: https://github.com/lrustand/emacs-config.el
;; Version: 0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;;; Change Log:

;;; Code:



(defun my/with-foreground (face str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:foreground ,(face-foreground face)) nil str)
  str)

(defun my/with-background (face str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:background ,(face-background face)) nil str)
  str)

(defun my/with-bold (str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:weight bold) nil str)
  str)


(provide 'my-text-property-helpers)
;;; my-text-property-helpers.el ends here
