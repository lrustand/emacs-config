;;; early-init.el --- My Emacs config     -*- lexical-binding: t; -*-

;; Disable junk
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(tab-bar-lines . 1) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(push '(background-color . "#002b36") (alist-get 'x window-system-default-frame-alist))
(push '(foreground-color . "#839496") (alist-get 'x window-system-default-frame-alist))

(setq gc-cons-threshold 500000000) ; Set to 500MB
(setq package-enable-at-startup nil)
