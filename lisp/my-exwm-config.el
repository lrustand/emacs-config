;;; my-exwm-config.el --- My EXWM config -*- lexical-binding: t -*-

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

;;; Code:



(defvar my-fullscreen-window-configuration nil
  "Stores the window configuration before entering fullscreen.")

(defun my-toggle-fullscreen ()
  "Toggle fullscreen for the current buffer.
Automatically exits fullscreen if any window-changing command is executed."
  (interactive)
  (if (= 1 (length (window-list)))
      (when my-fullscreen-window-configuration
        (set-window-configuration my-fullscreen-window-configuration)
        (setq my-fullscreen-window-configuration nil)
        (advice-remove 'split-window #'my-exit-fullscreen-advice))
    (setq my-fullscreen-window-configuration (current-window-configuration))
    (delete-other-windows)
    (advice-add 'split-window :before #'my-exit-fullscreen-advice)))

(defun my-exit-fullscreen-advice (&rest _)
  "Advice to exit fullscreen before executing window-changing commands."
  (when (and my-fullscreen-window-configuration
             (eq (selected-frame)
                 (window-configuration-frame my-fullscreen-window-configuration)))
    (my-toggle-fullscreen)))

;;(advice-add 'delete-window :before #'my-exit-fullscreen-advice)
;;(advice-add 'delete-other-windows :before #'my-exit-fullscreen-advice)
;;(advice-add 'switch-to-buffer-other-window :before #'my-exit-fullscreen-advice)

(use-package exwm
  :ensure (:wait t)
  :demand t
  :config
  (require 'exwm-randr)
  (exwm-randr-mode 1)
  (defun efs/exwm-update-class ()
    (exwm-workspace-rename-buffer (truncate-string-to-width exwm-title 100)))

  (defun lr/resize-left ()
    (interactive)
    (if (window-at-side-p nil 'right)
        (enlarge-window-horizontally 3)
      (shrink-window-horizontally 3)))
  (defun lr/resize-right ()
    (interactive)
    (if (window-at-side-p nil 'right)
        (shrink-window-horizontally 3)
      (enlarge-window-horizontally 3)))
  (defun lr/resize-up ()
    (interactive)
    (if (window-at-side-p nil 'bottom)
        (enlarge-window 3)
      (shrink-window 3)))
  (defun lr/resize-down ()
    (interactive)
    (if (window-at-side-p nil 'bottom)
        (shrink-window 3)
      (enlarge-window 3)))

  (defun my/exwm-randr-get-monitors ()
    (mapcar #'car (cadr (exwm-randr--get-monitors))))
  (defun my/exwm-configure-monitors ()
    (interactive)
    (let* ((monitors (my/exwm-randr-get-monitors))
           (workspaces (number-sequence 1 (length monitors))))
      (exwm-randr-refresh)
      (setq exwm-randr-workspace-monitor-plist
            (flatten-list (cl-mapcar #'cons workspaces monitors)))
      ;; Wait until monitors are done un/re-connecting
      (run-with-timer 5 nil #'exwm-randr-refresh)
      (exwm-randr-refresh)))
  (defun i3lock ()
    (interactive)
    (start-process-shell-command "i3lock" nil "i3lock -c 000000 -n"))
  (defun xmodmap ()
    (interactive)
    (start-process-shell-command "xmodmap" nil "xmodmap ~/.Xmodmap"))
  (xmodmap)
  (defun xinput-finger-disable ()
    (interactive)
    (start-process-shell-command "xinput" nil
                                 "xinput disable \"Wacom HID 5256 Finger\""))
  (xinput-finger-disable)
  :custom
  ;; Set the default number of workspaces
  (exwm-workspace-number 5)
  (exwm-layout-show-all-buffers t)
  (exwm-workspace-show-all-buffers t)
  ;; These keys should always pass through to Emacs
  (exwm-input-prefix-keys
   '(?\C-x
     ?\C-u
     ?\C-h
     ?\M-x
     ?\M-`
     ?\M-&
     ?\M-:
     ?\C-\ ))  ;; Ctrl+Space
  ;; Set up global key bindings.  These always work, no matter the input state!
  (exwm-input-global-keys
   `(
     ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
     ([?\s-r] . exwm-reset)

     ([?\H-d] . app-launcher-run-app)
     ([s-backspace] . kill-current-buffer)
     ([S-s-backspace] . delete-window)
     ([s-return] . eshell-toggle)

     ;; Move focus between windows
     ([s-left] . windmove-left)
     ([s-right] . windmove-right)
     ([s-up] . windmove-up)
     ([s-down] . windmove-down)
     ([?\s-h] . windmove-left)
     ([?\s-l] . windmove-right)
     ([?\s-k] . windmove-up)
     ([?\s-j] . windmove-down)

     ;; Next/prev buffer in window
     ([?\s-n] . next-buffer)
     ([?\s-p] . previous-buffer)

     ;; Next/prev tabs
     ([?\H-j] . tab-next)
     ([?\H-k] . tab-previous)

     ;; Move buffers
     ([S-s-left] . buf-move-left)
     ([S-s-right] . buf-move-right)
     ([S-s-up] . buf-move-up)
     ([S-s-down] . buf-move-down)
     ([?\s-H] . buf-move-left)
     ([?\s-L] . buf-move-right)
     ([?\s-K] . buf-move-up)
     ([?\s-J] . buf-move-down)

     ;; Swap windows
     ([M-s-left] . windmove-swap-states-left)
     ([M-s-right] . windmove-swap-states-right)
     ([M-s-up] . windmove-swap-states-up)
     ([M-s-down] . windmove-swap-states-down)
     ([?\M-\s-h] . windmove-swap-states-left)
     ([?\M-\s-l] . windmove-swap-states-right)
     ([?\M-\s-k] . windmove-swap-states-up)
     ([?\M-\s-j] . windmove-swap-states-down)

     ;; Resize window
     ([C-s-left] . lr/resize-left)
     ([C-s-down] . lr/resize-down)
     ([C-s-up] . lr/resize-up)
     ([C-s-right] . lr/resize-right)
     ([?\C-\s-h] . lr/resize-left)
     ([?\C-\s-j] . lr/resize-down)
     ([?\C-\s-k] . lr/resize-up)
     ([?\C-\s-l] . lr/resize-right)

     ;; Toggle fullscreen
     ([?\s-f] . my-toggle-fullscreen)

     ;; Launch applications via shell command
     ([?\s-&] . (lambda (command)
                  (interactive (list (read-shell-command "$ ")))
                  (start-process-shell-command command nil command)))

     ;; Switch workspace
     ([?\s-w] . exwm-workspace-switch)

     ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch-create ,i))))
               (number-sequence 0 9))))
  :hook
  (exwm-randr-screen-change . my/exwm-configure-monitors)
  (exwm-init . my/posframe-enable-exwm-advice)
  :config
  ;; When window "class" updates, use it to set the buffer name
  ;;(add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-class)

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (defun my/posframe-refposhandler (&optional frame)
    "Get a correct reference position for posframe under EXWM."
    (let* ((monitor-geometry (get-focused-monitor-geometry frame))
           (monitor-x (nth 0 monitor-geometry))
           (monitor-y (nth 1 monitor-geometry)))
      (cons monitor-x monitor-y)))

  (defun my/posframe-set-refposhandler (args)
    "Fix position reference for all posframes."
    (let ((props (cdr args))
          (posframe (car args)))
      (plist-put props :refposhandler #'my/posframe-refposhandler)
      (cons posframe props)))

  (defun my/posframe-enable-exwm-advice ()
    "Enable advice which corrects the posframe position under EXWM."
    (advice-add 'posframe-show :filter-args #'my/posframe-set-refposhandler))

  ;; Make posframe appear in front of X11 windows
  (with-eval-after-load 'posframe
    (define-advice posframe-show (:filter-return (frame) exwm-deparent)
      (set-frame-parameter frame 'parent-frame nil)
      frame))

  (exwm-enable))

(use-package ednc
  :ensure t
  :after exwm
  :preface
  (defun my-ednc-notifier (old notification)
  "Show TEXT in a posframe in the upper right corner of the main frame."
  (let* ((main-frame (selected-frame))
         (frame-width (frame-width main-frame))
         (frame-height (frame-height main-frame))
         (app-name (ednc-notification-app-name notification))
         (app-icon (ednc-notification-app-icon notification))
         (summary (ednc-notification-summary notification))
         (body (ednc-notification-body notification))
         (icon-image (if (f-file-p app-icon)
                         (create-image app-icon nil nil :width 32 :height 32)
                       ""))
         (icon-string (propertize "" 'display icon-image))
         (summary-text (propertize summary 'face 'bold))
         (body-text (string-trim (string-fill body 40)))
         (formatted-text
          (format "%s%s\n%s\n%s"
                  icon-string app-name summary-text (or body-text ""))))
    (posframe-show
     "*my-posframe-buffer*"
     :string formatted-text
     :poshandler (lambda (info) '(-1 . 16))
     :background-color "black"
     :border-color "red"
     :border-width 2
     :accept-focus nil
     :timeout 10)))

  :config
  (ednc-mode 1)
  (add-hook 'ednc-notification-presentation-functions 'my-ednc-notifier))


(defun wait-for-exwm-window (window-name)
  "Wait for an EXWM window with WINDOW-NAME to appear."
  (interactive "sEnter window name: ")
  (let ((window-exists nil))
    (while (not window-exists)
      (setq window-exists
            (cl-some (lambda (win)
                       (string-match-p window-name (exwm--get-window-title win)))
                     (exwm--list-windows)))
      (unless window-exists
        (sit-for 1)))  ; Wait for 1 second before checking again
    (message "The window '%s' has appeared!" window-name)))


;; Rofi application launcher alternative
(use-package app-launcher
  :after exwm
  :defer t
  :ensure (:fetcher github :repo "SebastienWae/app-launcher"))



;;;; Statusbar
;;;;-----------


(use-package lemon
  :ensure (:fetcher codeberg :repo "emacs-weirdware/lemon")
  :demand t
  :autoload
  lemon-battery-present?
  my/lemon-def-monitor
  :config
  (defun my/advice:lemon-battery--face (args)
    "Replace 0:00 time-left with N/A, to avoid error face when battery is full."
    (let* ((charging (nth 0 args))
           (percent (nth 1 args))
           (time-left (nth 2 args))
           (time-left (cond
                       ((and (string= "0:00" time-left)
                             (>= percent 90))
                        "N/A")
                       (t time-left))))
      (list charging percent time-left)))
  (advice-add 'lemon-battery--face :filter-args #'my/advice:lemon-battery--face)
  :preface
  (defmacro my/lemon-def-monitor (name interval &rest args)
    (declare (indent defun))
    (let ((monitor-name (intern (format "my/lemon-%s-monitor" (symbol-name name))))
          (lemon-name (intern (format "lemon-%s" (symbol-name name))))
          (function-name (intern (format "my/lemon-update-%s-monitor" (symbol-name name))))
          (display-name (intern (format "my/lemon-%s" (symbol-name name))))
          (timer-name (intern (format "my/lemon-%s-timer" (symbol-name name)))))
      `(progn
         (if (and (boundp ',timer-name)
                  (timerp ',timer-name))
             (cancel-timer ,timer-name)
           (defvar ,timer-name nil))
         (defvar ,monitor-name nil)
         (defun ,function-name ()
           (lemon-monitor-update ,monitor-name))
         (defun ,display-name ()
           (lemon-monitor-display ,monitor-name))
         (setq ,monitor-name (,lemon-name ,@args))
         (setq ,timer-name (run-with-timer 0 ,interval #',function-name))))))

(use-package tab-bar
  :ensure nil
  :hook (exwm-init . tab-bar-mode)
  :after lemon
  :defines battery cpu-linux memory-linux linux-network-rx linux-network-tx
  :config
  (my/lemon-def-monitor battery 30
    :display-opts '(:charging-indicator "+" :discharging-indicator "-"))
  (my/lemon-def-monitor cpu-linux 1
    :display-opts '(:index "CPU: " :unit "%"))
  (my/lemon-def-monitor memory-linux 30
    :display-opts '(:index "MEM: " :unit "%"))
  (my/lemon-def-monitor linux-network-rx 1)
  (my/lemon-def-monitor linux-network-tx 1)
  (defun lr/tab-bar-time-and-date ()
    (let* ((tab-bar-time-face '(:weight bold))
           (tab-bar-time-format  "%a %-d %b, %H:%M "))
      `((menu-bar menu-item
                  ,(propertize (format-time-string tab-bar-time-format)
                               'font-lock-face
                               tab-bar-time-face)
                  nil ;; <- Function to run when clicked
                  :help "Current date and time"))))
  (defface lr/tab-bar-face
    '((t :inherit mode-line-active)) ;; Inherit attributes from mode-line-active
    "Face for the tab bar.")
  ;; Set the tab-bar face to use the custom face
  (set-face-attribute 'tab-bar nil :inherit 'lr/tab-bar-face)
  :custom
  (my/tab-bar-refresh-timer (run-with-timer 0 1 'force-mode-line-update))
  ;; Disable stupid "This window displayed buffer" in deleted buffers
  (tab-bar-select-restore-windows nil)
  (tab-bar-format `(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    my/lemon-linux-network-tx
                    my/lemon-linux-network-rx
                    my/lemon-memory-linux
                    my/lemon-cpu
                    ,(when (lemon-battery-present?) 'my/lemon-battery)
                    tab-bar-separator
                    tab-bar-separator
                    lr/tab-bar-time-and-date)))


(provide 'my-exwm-config)
;;; my-exwm-config.el ends here
