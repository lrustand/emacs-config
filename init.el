;;; init.el --- My Emacs config     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Bootstrap
;;;---------------

;; Load early-init.el in older emacs.
(when (> 27 emacs-major-version)
  (load "~/.emacs.d/early-init.el"))

;; Needs to be set EARLY
(setq use-package-enable-imenu-support t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enabled/disable various groups of packages
;; Use :when (memq 'feature my/enabled-features) in use-package
(setq my/enabled-features
      '(chat
        mail
        web
        eglot
        exwm))


;;;; Elpaca

;; TODO: Fix this
(setq elpaca-core-date 20250307)

;; Example Elpaca configuration
(defvar elpaca-installer-version 0.10)
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
    (when (< emacs-major-version 28) (require 'subr-x))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;;;; Use-package setup
;;;;-------------------

;; Set up package.el to work with MELPA
;;(require 'package)
;;(add-to-list 'package-archives
;;         '("melpa" . "https://melpa.org/packages/"))

;;(async-bytecomp-package-mode 1)
;;(setq package-quickstart t)

;; Bootstrap `use-package' for emacs < 29.1
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))

;;(use-package auto-package-update
;;  :ensure t
;;  :custom
;;  ;; Auto-update Emacs packages every 30 days
;;  (auto-package-update-interval 30)
;;  ;; Exclude packages installed from Guix
;;  (auto-package-update-excluded-packages
;;   '(geiser geiser-guile vterm multi-vterm mu4e pdf-tools))
;;  :config
;;  ;; Do it before loading any other packages, to avoid having to restart
;;  (auto-package-update-maybe))

(eval-when-compile
  (require 'use-package))

;; Used for installing packages from git
;;(use-package quelpa-use-package
;;  :ensure t
;;  :config
;;  ;; Avoid loading quelpa if not necessary
;;  (setq quelpa-use-package-inhibit-loading-quelpa t)
;;  :custom
;;  ;; We don't use quelpa to checkout things from melpa.
;;  ;; This avoids the extremely annoying "contacting melpa"
;;  ;; when starting emacs.
;;  (quelpa-update-melpa-p nil)
;;  (quelpa-checkout-melpa-p nil)
;;  (quelpa-async-p t)
;;  (quelpa-verbose nil)
;;  (quelpa-build-verbose nil))
;;
;;(defun dont-upgrade-external (orig-fun name)
;;  (if (package--user-installed-p name)
;;      (apply orig-fun (list name))
;;    (message "Package %s is external, not deleting" name)))
;;
;;(advice-add 'package-upgrade :around #'dont-upgrade-external)

(use-package general
  :ensure (:wait t))

;;; Theme
;;;-------


(use-package solarized-theme
  :ensure t
  :after auto-dim-other-buffers
  :preface
  ;; Using this to change the auto-dim-other-bufers-face for solarized
  ;; Taken from alphapapa unpackaged scripts
  (defun unpackaged/customize-theme-faces (theme &rest faces)
    "Customize THEME with FACES.
  Advises `enable-theme' with a function that customizes FACES when
  THEME is enabled.  If THEME is already enabled, also applies
  faces immediately.  Calls `custom-theme-set-faces', which see."
    (declare (indent defun))
    (when (member theme custom-enabled-themes)
      ;; Theme already enabled: apply faces now.
      (let ((custom--inhibit-theme-enable nil))
        (apply #'custom-theme-set-faces theme faces)))
    (let ((fn-name (intern (concat "unpackaged/enable-theme-advice-for-" (symbol-name theme)))))
      ;; Apply advice for next time theme is enabled.
      (fset fn-name
            (lambda (enabled-theme)
              (when (eq enabled-theme theme)
                (let ((custom--inhibit-theme-enable nil))
                  (apply #'custom-theme-set-faces theme faces)))))
      (advice-remove #'enable-theme fn-name)
      (advice-add #'enable-theme :after fn-name)))
  :preface
  ;; Fix load-theme
  (defun disable-all-themes-before-load (&rest _)
    "Disable all themes before loading a new one."
    (mapcar #'disable-theme custom-enabled-themes))
  :config
  (window-divider-mode 1)
  (advice-add 'load-theme :before #'disable-all-themes-before-load)
  ;; Fix the unfocused backgrounds of solarized
  (unpackaged/customize-theme-faces 'solarized-dark
    '(auto-dim-other-buffers-face ((t (:background "#041f27")))))
  (unpackaged/customize-theme-faces 'solarized-light
    '(auto-dim-other-buffers-face ((t (:background
                                     "#eee8d5")))))
  (load-theme 'solarized-dark t)
  (set-face-attribute 'window-divider nil :foreground (face-attribute 'mode-line :background))
  (set-face-attribute 'window-divider-first-pixel nil :foreground (face-attribute 'mode-line :background))
  (set-face-attribute 'window-divider-last-pixel nil :foreground (face-attribute 'mode-line :background)))

(use-package doom-modeline
  :ensure t
  :demand t
  ;; Fix flymake error
  :functions doom-modeline-mode
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-percent-position nil)
  (doom-modeline-workspace-name nil)
  :config
  (unless (display-graphic-p)
    (setq doom-modeline-icon nil))
  (doom-modeline-mode 1))

;; Set default font
(set-frame-font "DeJavu Sans Mono 10" nil t)

;; Automatically dim the background color of unfocused buffers
(use-package auto-dim-other-buffers
  :ensure t
  :when (display-graphic-p)
  ;; Fix flymake error
  :functions auto-dim-other-buffers-mode
  :custom
  (auto-dim-other-buffers-affected-faces
   '((default . (auto-dim-other-buffers-face . nil))
     (org-block . (auto-dim-other-buffers-face . nil))
     (org-hide . (auto-dim-other-buffers-hide-face . nil))
     (fringe . (auto-dim-other-buffers-face . nil))
     (line-number . (auto-dim-other-buffers-face . nil))
     (line-number-current-line . (auto-dim-other-buffers-face . nil))
     (magit-diff-context-highlight . (auto-dim-other-buffers-face . nil))
     (magit-section-highlight . (auto-dim-other-buffers-face . nil))))
  :config
  (auto-dim-other-buffers-mode 1))


;;;; Theme modifications
;;;;----------------------


;; Swap the modeline bg colors for oksolar-dark
;;(unpackaged/customize-theme-faces 'doom-oksolar-dark
;;  `(mode-line-active
;;    ((t (:background ,(face-attribute 'mode-line-inactive
;;                                      :background)))))
;;  `(mode-line-inactive
;;    ((t (:background ,(face-attribute 'mode-line-active
;;                                      :background))))))

;;; EXWM
;;;--------

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
                 (window-configuration-frame my-fullscreen-window-configuration))
    (my-toggle-fullscreen))))

;;(advice-add 'delete-window :before #'my-exit-fullscreen-advice)
;;(advice-add 'delete-other-windows :before #'my-exit-fullscreen-advice)
;;(advice-add 'switch-to-buffer-other-window :before #'my-exit-fullscreen-advice)

(use-package exwm
  :ensure t
  :when (and (eq window-system 'x)
             (memq 'exwm my/enabled-features))
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

(use-package tab-bar
  :after exwm
  :hook
  (exwm-init . tab-bar-mode)
  :config
  (require 'lemon)
  (defun lr/tab-bar-time-and-date ()
    (let* ((tab-bar-time-face '(:weight bold))
           (tab-bar-time-format  "%a %-d %b, %H:%M "))
      `((menu-bar menu-item
                  ,(propertize (format-time-string tab-bar-time-format)
                               'font-lock-face
                               tab-bar-time-face)
                  nil ;; <- Function to run when clicked
                  :help "My heltp"))))
  (defun lr/tab-bar-separator () " | ")
  (defun ram ()
    (lemon-monitor-display my/memory-monitor))
  (defun cpu ()
    (lemon-monitor-display my/cpu-monitor))
  (defun bat ()
    (lemon-monitor-display my/battery-monitor))
  (defun net ()
    (concat
     (lemon-monitor-display my/network-rx-monitor)
     (lemon-monitor-display my/network-tx-monitor)))

  (defface my-tab-bar-face
    '((t :inherit mode-line-active))  ;; Inherit attributes from mode-line-active
    "Face for the tab bar.")
  ;; Set the tab-bar face to use the custom face
  (set-face-attribute 'tab-bar nil :inherit 'my-tab-bar-face)
  :custom
  ;; Disable stupid "This window displayed buffer" in deleted buffers
  (tab-bar-select-restore-windows nil)
  (tab-bar-format `(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    net
                    ram
                    cpu
                    ,(when (lemon-battery-present?) 'bat)
                    lr/tab-bar-separator
                    lr/tab-bar-time-and-date)))

(defvar my/battery-monitor)
(defvar my/battery-monitor-timer)
(defvar my/cpu-monitor)
(defvar my/cpu-monitor-timer)
(defvar my/memory-monitor)
(defvar my/memory-monitor-timer)
(defvar my/network-rx-monitor)
(defvar my/network-tx-monitor)
(defvar my/timer-network-monitor)
(defvar my/tab-bar-refresh-timer)

(use-package lemon
  :ensure (:fetcher codeberg :repo "emacs-weirdware/lemon")
  :after exwm
  :autoload
  lemon-monitor-display
  :preface
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
  :config
  (advice-add 'lemon-battery--face :filter-args #'my/advice:lemon-battery--face)
  (setq my/battery-monitor
   (lemon-battery :display-opts '(:charging-indicator "+"
                                  :discharging-indicator "-")))
  (setq my/battery-monitor-timer
   (run-with-timer 0 30
                   (lambda ()
                     (lemon-monitor-update
                      my/battery-monitor))))
  (setq my/cpu-monitor
   (lemon-cpu-linux :display-opts '(:index "CPU: "
                                    :unit "%")))
  (setq my/cpu-monitor-timer
   (run-with-timer 0 1
                   (lambda ()
                     (lemon-monitor-update
                      my/cpu-monitor))))
  (setq my/memory-monitor
   (lemon-memory-linux :display-opts '(:index "MEM: "
                                       :unit "%")))
  (setq my/memory-monitor-timer
   (run-with-timer 0 30
                   (lambda ()
                     (lemon-monitor-update
                      my/memory-monitor))))
  (setq my/network-rx-monitor (lemon-linux-network-rx))
  (setq my/network-tx-monitor (lemon-linux-network-tx))
  (setq my/network-monitor-timer
   (run-with-timer 0 1
                   (lambda ()
                     (lemon-monitor-update
                      my/network-rx-monitor)
                     (lemon-monitor-update
                      my/network-tx-monitor))))
  (setq my/tab-bar-refresh-timer
   (run-with-timer 0 1 'force-mode-line-update)))



;;; Evil
;;;------

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  ;; Silence flymake errors
  :functions
  evil-global-set-key
  evil-mode
  evil-set-undo-system
  :custom
  (evil-want-fine-undo t)
  :config
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (define-key evil-motion-state-map (kbd "RET") nil) ;; Disable to avoid overriding org-mode follow links
  (define-key evil-motion-state-map (kbd "TAB") nil) ;; Disable to avoid overriding outline folding
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  ;; Silence flymake errors
  :functions
  evil-collection-init
  :autoload
  evil-collection-define-key
  :config
  (evil-collection-init))

(use-package evil-terminal-cursor-changer
  :ensure t
  :after evil
  :unless (display-graphic-p)
  ;; Silence flymake errors
  :functions
  evil-terminal-cursor-changer-activate
  :config
  (evil-terminal-cursor-changer-activate))


;;; Emacs
;;;-------

(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  :custom
  (recentf-auto-cleanup "11:00pm")
  (recentf-max-menu-items 500)
  (recentf-max-saved-items 500))

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package man
  :custom
  ;; Make man behave.  Default value is "friendly", which is the exact
  ;; opposite of its name. It always creates a new window, and it
  ;; somehow always manages to do it in the worst possible place
  ;; too...
  (Man-notify-method 'pushy))

;; Make C-i and Tab separate. Without this hack it is impossible to
;; distinguish between Tab and C-i
(defun my-translate-C-i (_prompt)
  (if (and (= (length (this-single-command-raw-keys)) 1)
           (eql (aref (this-single-command-raw-keys) 0) ?\C-i)
           (bound-and-true-p evil-mode)
           (eq evil-state 'normal))
      (kbd "<C-i>")
    (kbd "TAB")))

(define-key key-translation-map (kbd "TAB") 'my-translate-C-i)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "<C-i>") 'evil-jump-forward))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)) ;; Fix tramp for Guix

(use-package emacs
  :config
  (xterm-mouse-mode 1)
  (savehist-mode 1)
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)

  ;; change all prompts to y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; TODO Do this same way as for auto dim, see above
  ;;(defun set-line-number-background ()
  ;;  (set-face-background 'line-number (face-attribute 'mode-line :background)))

  :custom
  (vc-follow-symlinks t)
  ;; Don't save faces to custom file
  (custom-file-save-faces nil)
  (backup-directory-alist '((".*" . "~/.emacs.d/backup")))
  (create-lockfiles nil)
  (x-underline-at-descent-line t)
  (x-select-enable-clipboard t)
  (ring-bell-function #'ignore)
  (indent-tabs-mode nil)
  (auto-revert-use-notify nil)
  (native-comp-async-report-warnings-errors 'silent)
  ;; Length of any type of history
  ;; Stored in ~/.emacs.d/history
  (history-length 1000)

  (window-restore-killed-buffer-windows 'dedicated)
  ;; Avoid accidentally invoking `zap-char' when pressing Alt.
  :bind (:map global-map
              ("M-z" . nil))
  :hook
  (prog-mode . (lambda ()
                 (setq-local show-trailing-whitespace t)
                 (display-line-numbers-mode))))
  ;;(server-after-make-frame . set-line-number-background)
  ;;(window-setup . set-line-number-background)

;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  :ensure t
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs)
  ;; Let's define an alias so there's no need to remember the order.
  :config (defalias 'emacs-restart #'restart-emacs))


;;; Interface
;;;-----------


;;;; GPG

(use-package epg
  :demand t
  :custom
  (epg-gpg-program "gpg2")
  (epg-pinentry-mode 'loopback))

(use-package password-store
  :ensure t)

(use-package password-store-otp
  :ensure t
  :after password-store)

;;;; Workspace/project management
;;;;--------------------------------

(use-package perspective-tabs
  :ensure (:fetcher sourcehut :repo "woozong/perspective-tabs"))

;; Fix posframes in persp-mode
;(add-hook
; 'persp-restore-window-conf-filter-functions
;   #'(lambda (f _ _)
;        (with-selected-frame f
;          (or (eq f posframe--frame) (window-dedicated-p)))))

;;;; Navigation
;;;;-----------

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-x O" . ace-swap-window))
  ;; Silence flymake errors
  :functions
  exwm-workspace--active-p
  ace-window-posframe-mode
  :preface
  (defun my/aw-window-list-advice (orig-fun &rest args)
    "Advice to use EXWM-aware frame visibility check in aw-window-list."
    (cl-letf (((symbol-function 'frame-visible-p) #'exwm-workspace--active-p))
      (apply orig-fun args)))
  :config
  (advice-add 'aw-window-list :around #'my/aw-window-list-advice)
  (set-face-attribute 'aw-leading-char-face nil :height 400)
  (ace-window-posframe-mode 1)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; Let windmove move to other frames as well
;; TODO Send PR. I have modified framemove to get correct frame coords in EXWM
;; Lines 45-48 in framemove.el. Use exwm-workspace--get-geometry.
(use-package framemove
  :when (display-graphic-p)
  :ensure (:fetcher github :repo "jsilve24/framemove")
  :init
  (setq framemove-hook-into-windmove t)
  :preface
  (defun my-fm-frame-bbox (frame)
    (let* ((geometry (exwm-workspace--get-geometry frame))
           (yl (slot-value geometry 'y))
           (xl (slot-value geometry 'x)))
      (list xl
            yl
            (+ xl (frame-pixel-width frame))
            (+ yl (frame-pixel-height frame)))))
  :config
  (advice-add 'fm-frame-bbox :override #'my-fm-frame-bbox))

;; Move a buffer to a different window without swapping
;; TODO: Integrate with framemove
(use-package buffer-move
  :ensure t
  :custom
  (buffer-move-behavior 'move))

(use-package ibuffer
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  :bind
  ("C-x C-b" . ibuffer))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;(use-package ibuffer-tramp
;;  :ensure t)
;;
;;(use-package ibuffer-project
;;  :ensure t)

(use-package emacs
  :unless (display-graphic-p)
  :demand t
  :bind*
  (("M-h" . windmove-left)
   ("M-j" . windmove-down)
   ("M-k" . windmove-up)
   ("M-l" . windmove-right)
   ("M-S-h" . bufmove-left)
   ("M-S-j" . bufmove-down)
   ("M-S-k" . bufmove-up)
   ("M-S-l" . bufmove-right)
   ("M-DEL" . delete-window)
   ("M-RET" . eshell-toggle)))

;;;; Help
;;;;------

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; TODO: Make package of this
;; Show function signatures in a popup instead of echo area
(defun my-eldoc-posframe-show (&rest args)
  "Show eldoc posframe containing ARGS."
  (when (car args)
    (posframe-show "*eldoc-posframe*"
                   :string (apply 'format args)
                   :position (point)
                   :max-width 100
                   :background-color "#333333"
                   :foreground-color "#eeeeee"
                   :internal-border-width 1
                   :internal-border-color "#777777")
    (add-hook 'post-command-hook #'my-eldoc-posframe-hide)))
(defun my-eldoc-posframe-hide ()
  "Hide eldoc posframe."
  (remove-hook 'post-command-hook #'my-eldoc-posframe-hide)
  (posframe-hide "*eldoc-posframe*"))
(setq eldoc-message-function #'my-eldoc-posframe-show)
(setq eldoc-idle-delay 1)
;; Only trigger after any editing
(setq eldoc-print-after-edit t)


;;;; Window layout and positioning
;;;;-------------------------------

;; Trying to tame emacs window placement (taken from perspective.el readme)
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window display-buffer-use-some-window)
    (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


;;;; Posframe
;;;;-----------

(use-package posframe
  :ensure t
  :config
  ;; Set border color of posframes. The supposed option of
  ;; make-posframe doesn't work, we have to do this instead
  (set-face-background 'internal-border "gray50"))


(use-package vertico-posframe
  :ensure t
  :after vertico
  :functions
  vertico-posframe-mode
  :preface
  (vertico-posframe-mode 1))

(use-package which-key-posframe
  :ensure t
  :after which-key
  :custom
  (which-key-posframe-border-width 2)
  :functions
  which-key-posframe-mode
  :preface
  (defun my-which-key-posframe--max-dimensions (_)
    "Return max-dimensions of posframe.
The returned value has the form (HEIGHT . WIDTH) in lines and
characters respectably."
    (cons (- (frame-height) 2) ; account for mode-line and minibuffer
          (min 300 (frame-width))))
  :config
  (which-key-posframe-mode 1)
  (advice-add 'which-key-posframe--max-dimensions :override #'my-which-key-posframe--max-dimensions))

;; Show magit popups etc in posframe
(use-package transient-posframe
  :ensure t
  :config
  (transient-posframe-mode))

;;;; Other stuff
;;;;-------------

(use-package drag-stuff
  :ensure t
  :functions
  drag-stuff-global-mode
  drag-stuff-define-keys
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package xclip
  :ensure t
  :when (eq window-system 'x)
  :functions
  xclip-mode
  :config
  (xclip-mode 1))

(use-package hl-todo
  :ensure t
  :functions
  global-hl-todo-mode
  :config
  (global-hl-todo-mode 1))

(use-package undo-tree
  :ensure t
  :functions
  global-undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package git-gutter-fringe
  :ensure t
  :functions
  global-git-gutter-mode
  :config
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (global-git-gutter-mode))

(use-package goggles
  :ensure t
  :functions
  goggles-mode
  :config
  (goggles-mode))

(use-package evil-goggles
  :ensure t
  :after (evil goggles)
  :functions
  evil-goggles-mode
  :config
  (evil-goggles-mode))

(use-package outshine
  :ensure t
  ;; Avoid this overriding terminal mode keybindings
  :when (display-graphic-p)
  :hook
  (emacs-lisp-mode . (lambda ()
                       (outshine-mode)
                       (add-to-list 'imenu-generic-expression
                                    '("Headings" "^;;; \\([^\n]+\\)" 1)
                                    t))))

(use-package olivetti
  :ensure t
  :defer t)

;; Highlight outline headings
;;(use-package outline-minor-faces
;;  :ensure t
;;  :after outline
;;  :hook
;;  (outline-minor-mode-hook . outline-minor-faces-mode))


;;;; Minibuffer
;;;;------------

;; Enable opening another minibuffer while in minibuffer
;; Usually recursive, but see below
(setq enable-recursive-minibuffers t)

;; NOTE: Disabled for using consult-history
;;(defun my-minibuffer-unrecursion ()
;;  "Replace running minibuffer."
;;  (when (> (minibuffer-depth) 1)
;;    (run-with-timer 0 nil 'my-interactive-command
;;                    this-command current-prefix-arg)
;;    (abort-recursive-edit)))
;;
;;(defun my-interactive-command (cmd arg)
;;  "Call new minibuffer CMD with ARG."
;;  (let ((current-prefix-arg arg))
;;    (call-interactively cmd)))
;;
;;;;Replace current minibuffer with a new one
;;(add-hook 'minibuffer-setup-hook 'my-minibuffer-unrecursion)

(use-package vertico
  :ensure t
  :demand t
  :functions
  vertico-mode
  :bind (:map vertico-map
              ("M-RET" . vertico-exit-input))
  :custom
  (vertico-multiform-commands
   '((consult-imenu buffer (:not posframe))
     (consult-grep buffer (:not posframe))
     (consult-outline buffer (:not posframe))
     (consult-xref buffer (:not posframe)
                   (vertico-buffer-display-action . (display-buffer-below-selected)))
     ;; consult-history in eshell
     (eshell-send-input buffer (:not posframe)
               (vertico-buffer-display-action . (display-buffer-below-selected)))
     (my/consult-org-headings buffer (:not posframe))))
  (vertico-sort-function #'vertico-sort-history-alpha)
  (vertico-buffer-display-action '(display-buffer-same-window))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package prescient
  :ensure t
  :custom
  (prescient-history-length 5000)
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable t)
  (prescient-tiebreaker nil)
  (prescient-aggressive-file-save t)
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))


(use-package marginalia
  :ensure t
  :functions
  marginalia-mode
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :functions
  consult-org-heading
  :preface
  (defun my/consult-org-headings ()
    "Switch to any top-level org heading"
    (interactive)
    (consult-org-heading "LEVEL=1"))
  (defun my/consult-buffer (&optional sources)
    (interactive)
    (let ((selected (consult--multi (or sources consult-buffer-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Switch to: "
                                    :history nil
                                    :sort nil)))
      ;; For non-matching candidates, fall back to buffer creation.
      (unless (plist-get (cdr selected) :match)
        (consult--buffer-action (car selected)))))
  (defun my/advice-consult-command-with-preview (orig-fun &rest args)
    "Advice any consult command to enable preview."
    (let ((consult-preview-key 'any))
      (apply orig-fun args)))
  :config
  (advice-add 'consult-buffer :override #'my/consult-buffer)
  (advice-add 'consult-xref :around #'my/advice-consult-command-with-preview)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     consult--source-file-register
     consult--source-bookmark
     consult--source-project-buffer-hidden
     consult--source-project-recent-file
     consult--source-project-root))
  (confirm-nonexistent-file-or-buffer t)
  ;; Disable autmatic previewing
  (consult-preview-key nil)
  (consult-imenu-config
   `((emacs-lisp-mode :toplevel "Functions" :types
                      ((?h "Headings" ((t :inherit outline-1
                                          :height ,(face-attribute 'default :height)
                                          :weight bold)))
                       (?f "Functions" font-lock-function-name-face)
                       (?m "Macros" font-lock-function-name-face)
                       (?p "Packages" font-lock-constant-face)
                       (?t "Types" font-lock-type-face)
                       (?v "Variables" font-lock-variable-name-face)))))
  :bind (("C-x b" . consult-buffer)
         ("C-h i" . consult-info))
  :bind (:map minibuffer-mode-map
              ("C-r" . consult-history)))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind (("C-c p p" . consult-projectile-switch-project)
         ("C-c p f" . consult-projectile)
         ("C-c p b" . consult-projectile-switch-to-buffer)))

(use-package consult-dir
  :ensure t
  :after consult
  :autoload
  eshell/j
  consult-dir--pick
  :preface
  (defun eshell/j ()
    "Jump to a directory in eshell using consult-dir."
    (eshell/cd (substring-no-properties (consult-dir--pick "cd: ")))))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless emacs22)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :preface
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  :custom
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :ensure t
  :after (consult embark))


;;;; Projectile
;;;;------------

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))


;;; Major modes

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package arduino-mode
  :ensure t)

(use-package hy-mode
  :ensure t)

;; Major mode for editing X Keyboard Extension files
(use-package xkb-mode
  :ensure t)

;; Major mode for editing various web templates with mixed/embedded languages
(use-package web-mode
  :ensure t)



;;; Python

;; NOTE: Deactivate in favor of auto-virtualenvwrapper
;;(use-package pyvenv
;;  :ensure t)
;;
;;(use-package pyvenv-auto
;;  :ensure t
;;  :after
;;  pyvenv
;;  :hook ((python-mode . pyvenv-auto-run)))

;; Multiline support for Inferior Python
(use-package python-mls
  :ensure t
  :hook
  (inferior-python-mode . python-mls-mode))

(use-package auto-virtualenvwrapper
  :ensure t
  ;; Avoid :custom being overwritten
  ;; These variables are using defvar instead of defcustom
  :config
  (setq auto-virtualenvwrapper-auto-deactivate t)
  (setq auto-virtualenvwrapper-verbose nil)
  :hook
  (python-base-mode . auto-virtualenvwrapper-activate)
  (window-configuration-change . auto-virtualenvwrapper-activate)
  (eshell-directory-change . (lambda ()
                               (auto-virtualenvwrapper-activate)
                               ;; Recalculate eshell path
                               (setq eshell-path-env-list nil)
                               (eshell-get-path)))
  (focus-in . auto-virtualenvwrapper-activate))

;;; Completion
;;;---------------

(use-package corfu
  :ensure t
  :defer 2
  :functions
  corfu-mode
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height)
    "Advise `corfu--make-frame' to be monitor-aware."
    ;; Get the geometry of the currently focused monitor
    (let* ((monitor-geometry (get-focused-monitor-geometry))
           (monitor-x (nth 0 monitor-geometry))
           (monitor-y (nth 1 monitor-geometry))
           ;; You may want to adjust the logic below if you have specific preferences
           ;; on where on the monitor the posframe should appear.
           ;; Currently, it places the posframe at its intended X and Y, but ensures
           ;; it's within the bounds of the focused monitor.
           (new-x (if (< x monitor-x) (+ monitor-x x) x))
           (new-y (if (< y monitor-y) (+ monitor-y y) y)))
      ;; Call the original function with potentially adjusted coordinates
      (funcall orig-fun frame new-x new-y width height)))
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Don't autoinsert first candidate when pressing tab.
  :bind (:map corfu-map
              ("TAB" . corfu-complete)) ; default is corfu-complete
  :custom
  (corfu-auto nil)
  ;;(corfu-auto-delay 0.6)
  (corfu-cycle t)
  (corfu-preview-current t)
  ; Don't freeze quick scrolling through candidates
  (corfu-popupinfo-delay 0.2)
  (tab-always-indent 'complete)
  :config
  (with-eval-after-load "sly"
    (sly-symbol-completion-mode -1))
  (advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (global-corfu-mode 1))


(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :custom
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Disabled for performance issues
;;(use-package corfu-candidate-overlay
;;  :ensure t
;;  :after corfu
;;  :custom-face
;;  (corfu-candidate-overlay-face ((t :foreground nil :inherit font-lock-comment-face)))
;;  :config
;;  (corfu-candidate-overlay-mode 1))

(use-package cape
  :ensure t
  :defer 2
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :custom
  (completion-at-point-functions
   (list (cape-capf-super #'cape-keyword #'yasnippet-capf)
         (cape-capf-super #'cape-dict #'cape-dabbrev)))
  ;; #'cape-file)
  ;; #'cape-elisp-block)
  ;; #'cape-history)
  ;; #'cape-tex)
  ;; #'cape-sgml)
  ;; #'cape-rfc1345)
  ;; #'cape-abbrev)
  ;; #'cape-dict)
  ;; #'cape-elisp-symbol)
  ;; #'cape-line)
  :preface
  (defun my/use-package-at-point ()
    "Return the use-package form around point, or nil."
    ;; Doesn't handle unbalanced parenthesis
    (save-excursion
      (beginning-of-defun)
      (let ((sexp (list-at-point t))) ;; Ignores comments and strings
        (when (eq 'use-package (car sexp))
          sexp))))

  ;; Checks previous form if not inside a form
  (defun my/inside-use-package-p ()
    "Check if point is inside a use-package form."
    (save-excursion
      (beginning-of-defun)
      (forward-to-word)
      (eq 'use-package (read (current-buffer)))))

  (defun my/use-package-heading-at-point (&optional return-marker)
    "Return the current use-package heading at point."
    (when (my/inside-use-package-p)
      (let* ((point (point)))
        (save-excursion
          (beginning-of-defun)
          ;; Enter the use-package form
          (forward-to-word)
          (let ((sexp-marker (point-marker))
                keyword-marker)       ; Point of last keyword
            (ignore-errors            ; We may encounter an unclosed paren
              (while (< (marker-position sexp-marker) point)
                (when (keywordp (read sexp-marker))
                  (setq keyword-marker (point-marker)))
                (goto-char sexp-marker)))
            (or (and return-marker keyword-marker)
                (and keyword-marker (read keyword-marker))))))))

  (defun my/use-package-first-level-sexp-at-point ()
    "Return a 'first level' sexp inside a use-package form."
    (when-let* ((marker (my/use-package-heading-at-point t))
                (keyword (read marker))
                (completion-point (point)))
      (save-excursion
        (condition-case err
            ;; Skip over sexps until we reach completion-point
            (while (< (marker-position marker) completion-point)
              (goto-char marker)
              (read marker)) ; We return the sexp containing point
          ;; Or try to close it
          (t (read (concat (buffer-substring-no-properties
                            (point) completion-point)
                           ;; HAHA: make sure sexp is closed
                           ")))))))))))))))))))))))))))))")))))))

  (defun my/use-package-completion-at-point ()
    "Completion at point function for use-package forms."
    (when-let* ((marker (my/use-package-heading-at-point t))
                (keyword (read marker))
                (completion-point (point)))
      (save-excursion
        ;;(beginning-of-defun)
        ;;(forward-to-word) ; Enter the use-package form
        ;;(ignore-errors ; We may encounter an unclosed parenthesis
        ;;  (while (< (point) completion-point)
        ;;    (setq sexp (read (current-buffer)))
        ;;    (when (keywordp sexp)
        ;;      (setq keyword sexp))))
        ;;(when (keywordp keyword)
        ;;  (backward-sexp)   ; Start of exp containing point
        ;;  (forward-to-word) ; Start of first element in sexp
        ;;  (forward-sexp)    ; End of first element in sexp
        (goto-char marker) ; Start of first sexp after keyword
        ;; Try to find start of containing sexp.
        ;; We might fail if the sexp is unclosed
        (ignore-errors
          (progn
            (while (< (point) completion-point)
              (read (current-buffer)))
            (backward-sexp)   ; Beginning of containing sexp
            (forward-to-word) ; First element
            (forward-sexp)))  ; End of first element
        (pcase keyword
          ;; Completion point is inside a :custom heading
          (:custom
           (and (= (point) completion-point) ; Completing a var name
                (my/cape-elisp-variables)))
          ;; Completion point is inside a :hook heading
          (:hook
           (if (= (point) completion-point)
               ;; Completing a hook name
               (my/cape-elisp-hooks-strip-suffix)
             ;; Completing a hook value
             ;; TODO: This fails to run
             (my/cape-elisp-functions)))))))

  (defun my/hook-p (symbol)
    "Return non-nil if SYMBOL is a hook."
    (and (boundp symbol)
         (string-suffix-p "-hook"
                          (symbol-name symbol))))

  (defalias 'my/cape-elisp-hooks
    (cape-capf-inside-code
     (cape-capf-predicate #'cape-elisp-symbol
                          #'my/hook-p)))

  ;; Completion function for hook names in use-package :hook section
  ;; Strips the -hook suffix, as use-package requires
  (defalias 'my/cape-elisp-hooks-strip-suffix
    (cape-capf-properties #'my/cape-elisp-hooks
                          ;; TODO: Use affixation function or
                          ;; something to strip the suffix inside the
                          ;; completion list instead
                          :affixation-function
                          (lambda (completions)
                            (mapcar (lambda (cand)
                                      (let ((base-name (string-remove-suffix "-hook" cand))
                                            (suffix (propertize "-hook" 'face 'font-lock-comment-face)))
                                        (list (concat base-name suffix) "" "")))
                                    completions))
                          :exit-function
                          (lambda (string status)
                            (let ((end (point-marker)))
                              (backward-to-word)
                              (delete-region (point) end)))))

  (defalias 'my/cape-elisp-functions
    (cape-capf-inside-code
     (cape-capf-predicate #'cape-elisp-symbol
                          #'fboundp)))

  (defalias 'my/cape-elisp-variables
    (cape-capf-inside-code
     (cape-capf-predicate #'cape-elisp-symbol
                          #'boundp)))

  (defalias 'my/cape-elisp-super-capf
    (cape-capf-nonexclusive
     (cape-capf-super #'yasnippet-capf
                      #'my/use-package-completion-at-point
                      #'elisp-completion-at-point)))

  (defun my/setup-elisp-capf ()
    (setq-local completion-at-point-functions
                #'my/cape-elisp-super-capf))

  (defalias 'my/cape-python-super-capf
    (cape-capf-nonexclusive
     (cape-capf-super #'yasnippet-capf
                      #'eglot-completion-at-point
                      #'python-completion-at-point)))

  (defalias 'my/cape-snippets-super-capf
    (cape-capf-interactive
    (cape-capf-nonexclusive
     (cape-capf-super #'yasnippet-capf
                      #'tempel-complete))))

  (defun my/setup-python-capf ()
    (setq-local completion-at-point-functions
                #'my/cape-python-super-capf))
  :hook
  ((python-mode python-ts-mode) . my/setup-python-capf)
  (emacs-lisp-mode . my/setup-elisp-capf))


;; Completion for shell commands
(use-package pcmpl-args
  :ensure t
  :demand t)

(use-package bash-completion
  :ensure t
  :defer t
  :autoload
  bash-completion-dynamic-complete
  bash-completion-capf-nonexclusive
  :custom
  (bash-completion-use-separate-processes t)
  :init
  (add-to-list 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  :hook
  (eshell-mode . (lambda ()
                   (add-hook 'completion-at-point-functions
                             'bash-completion-capf-nonexclusive nil t))))

;;;; Lisp
;;;;-----

(use-package sly
  :ensure t
  :defer t
  :config
  (evil-define-key 'insert sly-mrepl-mode-map (kbd "<up>") 'sly-mrepl-previous-input-or-button)
  (evil-define-key 'insert sly-mrepl-mode-map (kbd "<down>") 'sly-mrepl-next-input-or-button))


;;;; Scheme
;;;;---------

(use-package geiser
  :ensure nil
  :defer t
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :ensure nil
  :after geiser
  :config
  (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/"))

;;;; Snippets
;;;;----------

(use-package yasnippet
  :ensure t
  ;; Load after idle 2 sec
  :defer 2
  :custom
  (yas-indent-line 'auto)
  (yas-also-auto-indent-first-line t)
  :bind (:map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil))
  :config
  (yas-global-mode 1))

;; List all snippets with M-x `yas-describe-tables'
;; Or use `consult-yasnippet' installed below
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

(use-package yasnippet-capf
  :ensure t
  :after yasnippet)

(use-package consult-yasnippet
  :ensure t
  :after (yasnippet consult))


(use-package tempel
  :ensure t)

(use-package tempel-collection
  :ensure t
  :after tempel)

;;;; LSP
;;;;-----

;; Disabled in favor of eglot
;;(use-package lsp-mode
;;  :ensure t
;;  :defer t
;;  :commands (lsp lsp-deferred)
;;  :init
;;  (setq lsp-keymap-prefix "C-c l")
;;  :config
;;  (lsp-enable-which-key-integration t))

;;(use-package dap-mode
;;  :ensure t
;;  :defer t)

;;(use-package lsp-latex
;;  :ensure t
;;  :defer t
;;  :after lsp-mode
;;  :init
;;  (require 'lsp-latex)
;;
;;  :hook
;;  (tex-mode . 'lsp)
;;  (latex-mode . 'lsp)
;;  (LaTeX-mode . 'lsp)
;;  :config
;;
;;  ;; For YaTeX
;;  (with-eval-after-load "yatex"
;;    (add-hook 'yatex-mode-hook 'lsp))
;;
;;  ;; For bibtex
;;  (with-eval-after-load "bibtex"
;;    (add-hook 'bibtex-mode-hook 'lsp))
;;
;;  :custom
;;  (lsp-latex-forward-search-executable "okular")
;;  (lsp-latex-forward-search-args '("--noraise" "--unique" "file:%p#src:%l%f"))
;;  (lsp-latex-build-forward-search-after t)
;;  (lsp-latex-build-on-save t))

(use-package eglot
  :ensure t
  :defer t
  :config
  (setq-default eglot-workspace-configuration
                '(:pylsp
                  (:plugins
                   (:pylint (:enabled t)
                    :pycodestyle (:enabled :json-false)))))
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode)
                 "clangd"
                 "-j=2"
                 "--query-driver=/usr/bin/c++*,/usr/bin/g++*,/usr/bin/gcc*,/usr/bin/cc*"
                 "--compile-commands-dir=build"
                 "--clang-tidy=true"
                 "--all-scopes-completion=true"
                 "--header-insertion=iwyu"
                 "--header-insertion-decorators=true"))
  (add-to-list 'eglot-server-programs '(bitbake-mode . ("bitbake-language-server")))
  :hook
  (prog-mode . eglot-ensure)
  :custom
  (eglot-extend-to-xref t))

(use-package cc-mode
  :config
  (defun c-indent-then-complete ()
    (interactive)
    (if (= 0 (c-indent-line-or-region))
	(completion-at-point)))
  :bind (:map c++-mode-map
              ("<tab>" . c-indent-then-complete)))


(use-package eldoc-box
  :ensure t
  :after eglot
  :config
  (evil-define-key 'normal eglot-mode-map (kbd "K") 'eldoc-box-help-at-point))
  ;;:hook
  ;;(eglot-managed-mode . eldoc-box-hover-at-point-mode))

;; NOTE: Already handled by `eldoc-box'.
;; Show flymake error on hover
;;(use-package flymake-popon
;;  :ensure t
;;  :config
;;  (global-flymake-popon-mode 1))

;; This is awesome!
;; NOTE: Needs support in the LSP.
;; pylsp doesn't support it, but pyright and
;; jedi-language-server does.
(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package consult-eglot-embark
  :ensure t
  :after (consult eglot embark)
  :config
  (consult-eglot-embark-mode 1))

;;; Git
;;;-----

;; Open the current file on Github or similar
(use-package browse-at-remote
  :ensure t
  :defer t)

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :autoload
  ;; Used in eshell prompt
  magit-get-current-branch
  magit-anything-modified-p
  :custom
  ;; Don't nag extreme amounts when having an unsaved file
  (magit-save-repository-buffers nil)
  (magit-diff-refine-hunk t)
  :config
  ;; Create "commits" in reflog of uncommitted changes
  (magit-wip-before-change-mode 1)
  (magit-wip-initial-backup-mode 1))

(use-package magit-todos
  :ensure t
  :after magit
  :functions
  magit-todos-mode
  :config
  (magit-todos-mode 1))


;;; Org
;;;-----


(use-package org
  :config
  (require 'org-inlinetask)
  (with-eval-after-load "org"
    (add-to-list 'org-modules 'org-checklist))
    ;;(add-to-list 'org-modules 'org-habit))
  ;; Don't open links in a new window (for fucks sake)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-return-follows-link  t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)

  (org-todo-keywords
   '((sequence
      "TODO(t)" ; doing later
      "NEXT(n)" ; doing now or soon
      "|"
      "DONE(d!)" ; done
      )
     (sequence
      "BLOCKED(b@/!)"
      "HOLD(h)"
      "IDEA(i)" ; maybe someday
      "|"
      "CANCELED(c@/!)" ; stopped waiting, decided not to work on it
      )))
  (org-todo-keyword-faces
   '(("NEXT" . "green")
     ("BLOCKED" . "orange")
     ("CANCELED" . (:foreground "red" :weight bold))))

  (org-agenda-prefix-format '(
                              ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
                              (agenda  . " %i %(org-get-title) ")
                              (timeline  . "  %(org-get-title) ")
                              (todo  . " %i %(org-get-title) ")
                              (tags  . " %i %(org-get-title) ")
                              (search . " %i %(org-get-title) ")))

  (org-agenda-custom-commands
   `(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "work"
                  ((org-agenda-overriding-header "Work Tasks")))
       (tags-todo "+irl-TODO=\"HOLD\"-recurring"
                  ((org-agenda-overriding-header "IRL Tasks")))
       ,@(my/org-agenda-create-project-heading-agenda-views)))
     ("h" "Habits"
      ((tags-todo "STYLE=\"habit\""
                  ((org-agenda-overriding-header "Habits")))))
     ("p" "Projects"
      ,(my/org-agenda-create-project-heading-agenda-views))
     ("t" "Tags"
      ,(my/org-agenda-create-tag-heading-agenda-views))))


  :hook (org-mode . (lambda ()
                      (visual-line-mode 1)
                      (org-fold-hide-drawer-all)
                      (org-fold-all-done-entries)))

  :preface
  ;; This function generates headings for org-agenda for project files
  (defun my/org-agenda-create-project-heading-agenda-views ()
    (mapcar (lambda (file)
              `(todo "" ((org-agenda-files '(,file))
                         (org-agenda-overriding-header ,(my/org-roam-get-title file))
                         (org-agenda-prefix-format '((todo . ""))))))
            (my/org-roam-list-notes-by-tag "project")))

  ;; This function generates headings for org-agenda per tag
  (defun my/org-agenda-create-tag-heading-agenda-views ()
    (mapcar (lambda (tag)
              `(tags-todo ,(car tag) ((org-agenda-overriding-header ,(car tag)))))
            (seq-filter (lambda (it) (not (or (string-equal (car it) "project")
                                              (string-equal (car it) "todo"))))
                        (org-roam-db-query [:select :distinct [tag] :from tags ]))))

  (defun my/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (interactive)
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done)))))))))

  (add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)

  (defun org-summary-todo (_ n-not-done)
    "Switch entry to DONE when all subentries are done."
    (if (= n-not-done 0) (org-todo "DONE")))

  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

  ;; TODO only collapse DONE items if there are no TODO children
  (defun org-fold-all-done-entries ()
    "Close/fold all entries marked DONE."
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (while (outline-previous-heading)
        (when (org-entry-is-done-p)
          (hide-entry))))))

(use-package org-contrib
  :ensure t
  :after org)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-edna
  :ensure t
  :after org
  :config
  (org-edna-mode 1))

(use-package org-transclusion
  :ensure t
  :after org)


;;;; Roam
;;;;-------

(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :after org
  :init
  (make-directory "~/org-roam" t)
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-dailies-directory "dailies")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n p" . my/org-roam-find-project)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available

  (defun my/org-roam-get-title (file)
    (save-window-excursion
      (with-temp-buffer
        (insert-file-contents file)
        (org-get-title))))

  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (delete-dups (append (my/org-roam-list-notes-by-tag "project")
                                                (my/org-roam-list-notes-by-tag "todo")))))

  ;; Build the agenda list the first time for the session
  (my/org-roam-refresh-agenda-list)

  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "project")
     nil
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
        :unnarrowed t))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))
  (org-roam-db-autosync-mode))


;;;; Thesis
;;;;--------

;;(require 'async)
;;(defun get-package-deps (package)
;;  (mapcar #'car (package-desc-reqs (cadr (assq package package-alist)))))
;;(defun async-export ()
;;  (interactive)
;;  (async-start
;;   `(lambda ()
;;      (setq load-path ',load-path)
;;      (require 'org)
;;      (require 'ox-latex)
;;      (require 'org-ref)
;;      (require 'engrave-faces)
;;      (require 'org-inlinetask)
;;      (require 'solarized-theme)
;;      (load-theme 'solarized-dark t)
;;      (setq engrave-faces-themes ',engrave-faces-themes)
;;      (setq default-directory ,(file-name-directory (buffer-file-name)))
;;      (find-file ,(buffer-file-name))
;;      (setq enable-local-variables :all)
;;      (hack-local-variables)
;;      (org-latex-export-to-pdf)
;;      "Export completed")
;;   (lambda (result)
;;     (message "Async export result: %s" result))))
;;
;;
;;(use-package org-gantt
;;  :after org
;;  :defer t
;;  :quelpa (org-gantt :fetcher github :repo "swillner/org-gantt"))

(use-package org-ref
  :ensure t
  :after org)

;;(use-package bibtex-completion
;;  :ensure t
;;  :defer t
;;  :custom
;;  (bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
;;  (bibtex-completion-additional-search-fields '(keywords))
;;  (bibtex-completion-display-formats
;;  '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
;;    (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
;;    (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;    (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;    (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
;;  (bibtex-completion-pdf-open-function
;;    (lambda (fpath)
;;      (call-process "okular" nil 0 nil fpath)))
;;
;;  :config
;;  (defun my-open-citation-at-point ()
;;    (interactive) (bibtex-completion-open-pdf (list (thing-at-point 'symbol))))
;;
;;  (with-eval-after-load "evil"
;;    (evil-define-key 'normal 'latex-mode-map "gp" 'my-open-citation-at-point)))



;;; System
;;;---------

;;;; System packages

(use-package system-packages
  :ensure (:wait t)
  :config
  (setq my/emacs-guix-profile (expand-file-name (concat user-emacs-directory "guix-profile")))
  (setf (alist-get 'guix system-packages-supported-package-managers)
        `((install . ,(format "guix install --profile=%s" my/emacs-guix-profile))
          (search . "guix search")
          (uninstall . ,(format "guix remove --profile=%s" my/emacs-guix-profile))
          (update . ,(format "guix upgrade --profile=%s" my/emacs-guix-profile))
          (clean-cache . ,(format "guix gc"))
          (list-installed-packages . ,(format "guix package --profile=%s -I" my/emacs-guix-profile))
          (list-installed-packages-all . ,(format "guix package --profile=%s -I" my/emacs-guix-profile)))))

(use-package bluetooth
  :ensure t
  :ensure-system-package (bluetoothctl . bluez))

;;;; D-Bus and Power
;;;;------------------

(use-package dbus
  :ensure nil)

;;;; Terminal
;;;;----------

;; This section contains all terminal emulators and shells such as
;; vterm, eat, and eshell.


;;;;; Eshell
;;;;;-------

(use-package eshell-prompt-extras
  :ensure t)

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

(use-package eshell
  :config
  ;; Define a variable to hold the cd history
  (defvar eshell-cd-history nil
    "History of directories visited in EShell.")

  ;; Function to view and select from cd history
  (defun eshell-cd-history-view ()
    "View and select a directory from the cd history."
    (interactive)
    (if eshell-cd-history
        (let ((directory (completing-read "Select directory: " eshell-cd-history)))
          (cd directory))
      (message "No history available.")))

  ;; Advice function to modify the original cd command
  (defun eshell-cd-advice (orig-fun &rest args)
    "Advice around the original cd function to record history."
    (let ((directory (car args)))  ;; Get the first argument (the directory)
      (when (and directory (stringp directory))
        (setq eshell-cd-history (cons (expand-file-name directory) eshell-cd-history)))
      ;; Call the original cd function
      (apply orig-fun args)))

  ;; Apply the advice to the original eshell/cd function
  (advice-add 'eshell/cd :around #'eshell-cd-advice)

  (defun my-eshell-evil-insert ()
    "Move cursor to end of prompt when entering insert mode in Eshell."
    (when (and (eq major-mode 'eshell-mode)
               (evil-insert-state-p))
      (unless (eq (line-number-at-pos)
                  (line-number-at-pos (point-max)))
        (goto-char (point-max))
        (end-of-line))))

  (defun git-status ()
    (let ((default-directory (eshell/pwd)))
      (with-output-to-string
        (with-current-buffer standard-output
          (call-process "git" nil t nil "status" "--porcelain")))))

  ;;(defun git-status--dirty-p ()
  ;;  (not (string-blank-p (git-status))))
  (defun git-status--dirty-p ()
    (magit-anything-modified-p))

  (defun my/eshell-consult-dir-pick ()
    "Select a dir with consult and insert it in eshell."
    (interactive)
    (insert (consult-dir--pick)))
  (defun my/eshell-consult-dir-cd ()
    "Select a dir with consult and cd to it in eshell."
    (interactive)
    (eshell/cd (consult-dir--pick))
    (eshell-emit-prompt))
  :hook
  (eshell-mode . (lambda ()
                   ;; (setenv "TERM" "xterm-256color")
                   ;; Buffer local hook
                   (add-hook 'evil-insert-state-entry-hook
                             #'my-eshell-evil-insert nil t)))
  :general
  (:keymaps 'eshell-mode-map
   :states 'insert
   "C-r" 'consult-history
   "C-c C-d" 'my/eshell-consult-dir-cd)
  :custom
  (eshell-history-size 10000)
  (eshell-history-append t)
  (eshell-hist-ignoredups t)
  ;;(setq eshell-visual-commands
  (eshell-prompt-function
   (lambda ()
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
        (propertize (format " [%s]" (f-abbrev (eshell/pwd))) 'face default-prompt-face)
        (when (magit-get-current-branch)
          (concat
           (propertize (format " [%s" (magit-get-current-branch)) 'face git-branch-face)
           (when (git-status--dirty-p)
             (propertize "*" 'face `(:foreground "red" :background ,prompt-bg :bold)))
           (propertize "]" 'face git-branch-face)))
        "\n"
        (propertize "\n" 'face default-prompt-face)
        (propertize " ->" 'face `(:foreground ,blue :background ,prompt-bg :bold))
        (propertize " " 'face default-prompt-face)))))
  (eshell-prompt-regexp "^ -> "))

;; Disable in favor of `eat'
;;(use-package eshell-vterm
;;  :ensure t
;;  :after
;;  vterm
;;  :config
;;  (eshell-vterm-mode 1))

(use-package eat
  :ensure t
  :defer 2
  :hook
  (eat-exit . (lambda (process)
                (kill-buffer (process-buffer process))))
  :custom
  ;; This may cause problems for commands run with sudo.
  ;; The TERMINFO may not be passed through.
  (eat-term-name 'eat-term-get-suitable-term-name)
  ;; Disable this, since eshell is running inside eat now.
  (eshell-visual-commands nil)
  :config
  (eat-eshell-mode 1))

(use-package eshell-toggle
  :ensure t
  :after eshell
  :custom
  (eshell-toggle-default-directory (getenv "HOME"))
  (eshell-toggle-find-project-root-package 'project)
  (eshell-toggle-init-function 'eshell-toggle-init-eshell)
  :preface
  (defun eshell-toggle--set-window-dedicated (orig-fun &rest args)
    (apply orig-fun args)
    (when eshell-toggle--toggle-buffer-p
      (set-window-dedicated-p (selected-window) t)))
  (defun eshell-toggle--hide-buffers (orig-fun &rest args)
    "Make eshell-toggle buffers hidden."
    (concat " " (funcall orig-fun)))
  :config
  (advice-add 'eshell-toggle :around #'eshell-toggle--set-window-dedicated)
  (advice-add 'eshell-toggle--make-buffer-name :around #'eshell-toggle--hide-buffers))

(use-package eshell-outline
  :ensure t
  :after eshell
  :hook
  (eshell-mode . eshell-outline-mode))

;; Highlight command names in eshell
(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  ;;:custom
  ;; Do not print the "nobreak" character
  ;; (nobreak-char-display nil)
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode 1))

;; BUG: Messes up `eshell-previous-matching-input-from-input'.
;; The fringe symbols seem to be saved to history
;; See `eshell-list-history'.
;;(use-package eshell-fringe-status
;;  :ensure t
;;  :hook
;;  (eshell-mode . eshell-fringe-status-mode))


;;;;; Vterm
;;;;;--------

(use-package vterm
  :ensure nil
  :defer t
  :custom
  (vterm-buffer-name-string "VTerm: %s")
  :config
  ;; Fix background
  (defun old-version-of-vterm--get-color (index &rest args)
    "This is the old version before it was broken by commit
https://github.com/akermu/emacs-libvterm/commit/e96c53f5035c841b20937b65142498bd8e161a40.
Re-introducing the old version fixes auto-dim-other-buffers for vterm buffers."
    (cond
     ((and (>= index 0) (< index 16))
      (face-foreground
       (elt vterm-color-palette index)
       nil 'default))
     ((= index -11)
      (face-foreground 'vterm-color-underline nil 'default))
     ((= index -12)
      (face-background 'vterm-color-inverse-video nil 'default))
     (t
      nil)))
  (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color)
  ;; Use libvterm installed in Guix
  (advice-add 'vterm-module-compile :around
              (lambda (f &rest r)
                (make-symbolic-link (expand-file-name "~/.guix-home/profile/lib/libvterm.so.0")
                                    (file-name-directory (locate-library "vterm.el" t)) t)
                (make-symbolic-link (expand-file-name "~/.guix-home/profile/lib/vterm-module.so")
                                    (file-name-directory (locate-library "vterm.el" t)) t)))
  ;; Disable running process warning when only shell is running
  ;; TODO: Send PR?
  (advice-add 'kill-buffer :around
              (lambda (orig-fun &rest args)
                (if (eq major-mode 'vterm-mode)
                    (let* ((proc (get-buffer-process (current-buffer)))
                           (proc-name (if proc (process-name proc) ""))
                           (only-shell-running (not (process-running-child-p proc-name))))
                      (if only-shell-running
                          (set-process-query-on-exit-flag proc nil))))
                (apply orig-fun args))))

(use-package multi-vterm
  :ensure nil
  :defer t
  :after vterm)


;;;; Dired
;;;;-------

(use-package dired
  :ensure nil
  :after evil-collection
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-lAh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package dired-imenu
  :ensure t
  :after dired
  :hook (dired-mode . dired-setup-imenu))

(use-package dired-git-info
  :ensure t
  :after dired
  :config
  (defun my/advice-dired-git-info-clamp-string (orig-fun str max)
    "Fix line wrapping of long git commits in dired-git-info."
    (when all-the-icons-dired-mode
      (setq max (- max 3)))
    (funcall orig-fun str max))
  (advice-add 'dgi--clamp-string :around #'my/advice-dired-git-info-clamp-string)
  :custom
  (dgi-auto-hide-details-p nil)
  :hook (dired-after-readin . dired-git-info-auto-enable))

;; TODO: tab to enter directory, (dired-narrow-enter-directory),
;; or expand minibuffer input to hovered file.
(use-package dired-narrow
  :ensure t
  :after dired
  :config
  (defun my-dired-narrow-and-select ()
    "Narrow dired to filter results, then select the file at point."
    (interactive)
    (call-interactively 'dired-narrow)
    (dired-find-file)))

;; Open archive files seamlessly in dired
(use-package dired-avfs
  :ensure t
  :after dired)

;; Filetype icons in dired
;; TODO: Clashes with dired-git-info-mode, when long commit messages.
;; Makes the lines not wrap correctly
(use-package all-the-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Collapse multiple dirctory levels if each only has one dir
;; Like the file explorer on github does
(use-package dired-collapse
  :ensure t
  :after dired
  :config
  (global-dired-collapse-mode 1))

;; Keep reusing a single dired buffer instead of opening new
;; every time you navigate to another folder
;; TODO: Integrate in normal dired commands
;;(use-package dired-single
;;  :ensure t
;;  :after dired)

;; Open some filetypes in external program
;; TODO: Automatically open correct program through mime/xdg
(use-package dired-open
  :ensure t
  :after dired
  :custom
  (dired-open-extensions
   '(("mp4" . "mpv"))))

(use-package pdf-tools
  :ensure nil ;; Use package from Guix
  :hook
  ;; Disable blinking border around pdf pages (caused by cursor blink)
  (pdf-view-mode . (lambda ()
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                     (set (make-local-variable 'evil-evilified-state-cursor) (list nil))
                     (pdf-view-fit-page-to-window)))
  :config
  (pdf-tools-install))

(use-package saveplace-pdf-view
  :ensure t
  :after pdf-tools
  :demand t)

(use-package image-mode
  :hook
  ;; Disable blinking border around images (caused by cursor blink)
  (image-mode . (lambda ()
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                     (set (make-local-variable 'evil-evilified-state-cursor) (list nil)))))





;;;; Proced
;;;;--------

;; Process Editor (htop-like)
(use-package proced
  :preface
  (defvar proced-guix-nix-readable-mode-keywords
    '(("\\(/nix/store/[0-9a-z]*-\\)"
       (1 '(face nil invisible t)))
      ("\\(/gnu/store/[0-9a-z/\.-]*/\\).* ?.*"
       (1 '(face nil invisible t)))))

  (define-minor-mode proced-guix-nix-readable-mode
    "Make proced filenames more readable in Guix and Nix"
    :lighter " proced-hash-filter-mode"
    (if proced-guix-nix-readable-mode
        (progn
          (make-variable-buffer-local 'font-lock-extra-managed-props)
          (add-to-list 'font-lock-extra-managed-props 'invisible)
          (font-lock-add-keywords nil
                                  proced-guix-nix-readable-mode-keywords)
          (font-lock-mode t))
      (progn
        (font-lock-remove-keywords nil
                                   proced-guix-nix-readable-mode-keywords)
        (font-lock-mode t))))
  :custom
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t)
  ;; Enable remote proced over tramp
  (proced-show-remote-processes t)
  :hook
  (proced-mode . proced-guix-nix-readable-mode))




;;; Media players
;;;----------------

(use-package empv
  :ensure t
  :defer t
  :preface
  (defun empv-set-background (color)
    (empv--send-command-sync (list "set_property" "background-color" color)))
  (defun lr/empv-undim (orig-fun &rest next-window args)
    (when (string= "mpv"  exwm-class-name)
      (empv-set-background (face-background 'default))))
  (defun lr/empv-dim (orig-fun &rest next-window args)
    (when (string= "mpv"  exwm-class-name)
      (empv-set-background (face-background 'auto-dim-other-buffers-face))))
  :config
  ;;(advice-add #'adob--dim-buffer :
  (advice-add #'select-window :before #'lr/empv-dim)
  (advice-add #'select-window :after #'lr/empv-undim)
  :custom
  (empv-invidious-instance "https://invidious.nerdvpn.de/api/v1")
  (empv-mpv-args `(,(format "--background-color=%s" (face-background 'default))
                   "--no-terminal" "--idle" "--stream-buffer-size=20MiB"
                   "--input-ipc-server=/tmp/empv-socket")))

(use-package elfeed-tube
  :ensure t
  :defer t)

(use-package elfeed-tube-mpv
  :ensure t
  :after elfeed-tube)

(use-package emms
  :ensure t
  :defer t
  :config
  (require 'emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv)
  :preface
  (defun my/get-youtube-title (url)
    "Get the title of a YouTube video using yt-dlp."
    (with-temp-buffer
      (call-process "yt-dlp" nil t nil "--get-title" url)
      (string-trim (buffer-string))))
  (advice-add 'emms-format-url-track-name :override #'my/get-youtube-title)
  (defun advise-emms-playlist-mode-kill-track (orig-fun &rest args)
    "Get the actual track name, instead of the formatted name."
    (cl-letf (((symbol-function 'kill-line)
               (lambda ()
                 (delete-line)
                 (kill-new (emms-track-get track 'name)))))
      (funcall orig-fun args)))
  (advice-add 'emms-playlist-mode-killtrack :around #'advise-emms-playlist-mode-kill-track)
  (defvar emms-player-mpv-volume 100)
  (defun emms-player-mpv-get-volume ()
    "Sets `emms-player-mpv-volume' to the current volume value
and sends a message of the current volume status."
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq emms-player-mpv-volume vol)
                                 (message "Volume: %s%%"
                                          vol))))))

  (defun emms-player-mpv-raise-volume (&optional amount)
    (interactive)
    (let* ((amount (or amount 5))
           (new-volume (+ emms-player-mpv-volume amount)))
      (if (> new-volume 100)
          (emms-player-mpv-cmd '(set_property volume 100))
        (emms-player-mpv-cmd `(add volume ,amount))))
    (emms-player-mpv-get-volume))
  (defun emms-player-mpv-lower-volume (&optional amount)
    (interactive)
    (emms-player-mpv-cmd `(add volume ,(- (or amount '5))))
    (emms-player-mpv-get-volume))
  :custom
  (emms-volume-change-function 'emms-player-mpv-raise-volume))


;;; Communication
;;;-----------------
;;;; Mail
;;;;------

(use-package consult-mu
  :when (memq 'mail my/enabled-features)
  :after (consult mu4e)
  :ensure (:fetcher github :repo "armindarvish/consult-mu"))

(defun insert-cut-here-start ()
  "Insert opening \"cut here start\" snippet."
  (interactive)
  (insert "--8<---------------cut here---------------start------------->8---"))

(defun insert-cut-here-end ()
  "Insert closing \"cut here end\" snippet."
  (interactive)
  (insert "--8<---------------cut here---------------end--------------->8---"))

(use-package mu4e
  :when (memq 'mail my/enabled-features)
  :ensure nil
  :ensure-system-package mu
  :init
  (when (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))
  :config
  (require 'mu4e-contrib)
  :commands
  mu4e
  :functions
  mu4e-mark-handle-when-leaving
  mu4e-search-maildir
  mu4e--query-items-refresh
  :preface
  ;; Override this to avoid the IDIOTIC mue4 "main window"
  (defun my-mu4e~headers-quit-buffer (&rest _)
    "Quit the mu4e-headers buffer and do NOT go back to the main view."
    (interactive)
    (mu4e-mark-handle-when-leaving)
    (quit-window t)
    (mu4e--query-items-refresh 'reset-baseline))
  (defun my-disabled-mu4e--main-menu ()
    "Skip the USELESS main menu."
    (mu4e-search-maildir "/All Mail"))
  :config
  (advice-add 'mu4e~headers-quit-buffer :override #'my-mu4e~headers-quit-buffer)
  (advice-add 'mu4e--main-menu :override #'my-disabled-mu4e--main-menu)
  (mu4e-modeline-mode -1)
  :hook
  ;; Don't create tons of "draft" messages
  (mu4e-compose-mode . (lambda () (auto-save-mode -1)))
  :custom
  (mu4e-headers-open-after-move nil)
  ;; Don't spam the echo area all the time
  (mu4e-hide-index-messages t)
  ;; Don't mess with my window layout
  (mu4e-split-view 'single-window)
  ;; Do as I say
  (mu4e-confirm-quit nil)
  (mu4e-update-interval 30)
  ;; Use with font-google-noto, or a later version of font-openmoji
  (mu4e-headers-unread-mark    '("u" . "📩"))
  (mu4e-headers-draft-mark     '("D" . "✏️"))
  (mu4e-headers-flagged-mark   '("F" . "🚩"))
  (mu4e-headers-new-mark       '("N" . "✨"))
  (mu4e-headers-passed-mark    '("R" . "↪️"))
  (mu4e-headers-replied-mark   '("R" . "↩️"))
  (mu4e-headers-seen-mark      '("S" . "✔️"))
  (mu4e-headers-trashed-mark   '("T" . "🗑️"))
  (mu4e-headers-attach-mark    '("a" . "📎"))
  (mu4e-headers-encrypted-mark '("x" . "🔒"))
  (mu4e-headers-signed-mark    '("s" . "🔑️"))
  (mu4e-headers-calendar-mark  '("c" . "📅"))
  (mu4e-headers-list-mark      '("l" . "📰"))
  (mu4e-headers-personal-mark  '(""  . ""  )) ; All emails are marked personal; hide this mark

  (mu4e-compose-dont-reply-to-self t)

  (mu4e-attachment-dir "~/Downloads")

  ;; Gmail takes care of sent messages
  (mu4e-sent-messages-behavior 'delete)

  ;; use mu4e for e-mail in emacs
  (mail-user-agent 'mu4e-user-agent)
  (user-mail-address "rustand.lars@gmail.com")
  (mail-host-address "gmail.com")
  (user-full-name "Lars Rustand")
  ;; these must start with a "/", and must exist
  ;; (i.e.. /home/user/Maildir/sent must exist)
  ;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
  ;; already exist

  ;; below are the defaults; if they do not exist yet, mu4e offers to
  ;; create them. they can also functions; see their docstrings.
  (mu4e-sent-folder   "/Sent Mail")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder  "/Trash"))

(use-package message
  ;; Not using msmtp anymore, switch to Emacs builtin smtpmail
  ;;(sendmail-program "msmtp")
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from")))

;; Use emacs builtin smtpmail package to send mail.
(use-package smtpmail
  :custom
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-user "rustand.lars@gmail.com")
  ;; Password needs to be set in ~/.authinfo.gpg, like this:
  ;; machine <smtp server> port <smtp port> login <smtp user> password <secret>
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl))

(defun my-confirm-empty-subject ()
  "Allow user to quit when current message subject is empty."
  (or (message-field-value "Subject")
      (yes-or-no-p "Really send without Subject? ")
      (keyboard-quit)))

(add-hook 'message-send-hook #'my-confirm-empty-subject)

(use-package mu4e-thread-folding
  :ensure (:fetcher github
           :repo "rougier/mu4e-thread-folding")
  :after mu4e
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))

  (evil-define-key 'normal mu4e-headers-mode-map
    (kbd "TAB")  'mu4e-headers-toggle-at-point)

  :custom
  (mu4e-headers-fields '((:empty         .    2)
                         (:human-date    .   12)
                         (:flags         .    6)
                         ;;(:mailing-list  .   10)
                         (:from          .   22)
                         (:subject       .   nil)))
  (mu4e-thread-folding-default-view 'folded)
  (mu4e-headers-found-hook '(mu4e-headers-mark-threads mu4e-headers-fold-all)))


;;;; Chat
;;;;-----

(use-package ement
  :when (memq 'chat my/enabled-features)
  :defer t
  :ensure (:fetcher github :repo "alphapapa/ement.el"))

(use-package erc
  :when (memq 'chat my/enabled-features)
  :defer t
  :defines
  erc-modules
  :functions
  define-erc-module
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT")))

(use-package erc-twitch
  :when (memq 'chat my/enabled-features)
  :ensure t
  :after erc
  :functions
  erc-twitch-enable
  :config
  (erc-twitch-enable))

(use-package erc-hl-nicks
  :when (memq 'chat my/enabled-features)
  :ensure t
  :after erc
  :functions
  erc-hl-nicks-enable
  :config
  (erc-hl-nicks-enable))

(use-package erc-image
  :when (memq 'chat my/enabled-features)
  :ensure t
  :after erc
  :functions (erc-image-enable
              erc-image-disable)
  :config
  (erc-image-enable))


;;; Web
;;;-----


(use-package engine-mode
  :when (memq 'web my/enabled-features)
  :ensure t
  :functions
  engine-mode
  engine--search-prompt
  :custom
  (engine/browser-function 'qutebrowser-open-url)
  :preface

  (defvar engine-search-history '())

  (defun my-engine-use-completing-read (engine-name)
    "Advice to use completing-read instead of read-string in engine-mode."
    (let ((current-word (or (thing-at-point 'symbol 'no-properties) "")))
      (completing-read (engine--search-prompt engine-name current-word)
                       engine-search-history nil nil nil 'engine-search-history current-word)))
  :config
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")

  (defengine google
    "https://google.com/search?q=%s"
    :keybinding "g")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine youtube
    "https://youtube.com/results?search_query=%s"
    :keybinding "y")

  (defengine amazon
    "https://amazon.com/s?k=%s"
    :keybinding "a")

  (defengine ebay
    "https://ebay.com/sch/i.html?_nkw=%s"
    :keybinding "e")

  (advice-add 'engine--prompted-search-term :override #'my-engine-use-completing-read)
  (engine-mode 1))


;;;; Qutebrowser
;;;;--------------

(use-package qutebrowser
  :when (and (eq window-system 'x)
             (memq 'web my/enabled-features))
  :ensure (:fetcher github
           :repo "lrustand/qutebrowser.el"
           :files (:defaults "*.py"))
  :custom
  (browse-url-browser-function #'qutebrowser-open-url)
  :config
  (qutebrowser-theme-export-mode 1)
  (global-qutebrowser-doom-modeline-mode 1)
  (qutebrowser-evil-state-mode 1)
  (global-qutebrowser-exwm-mode 1))


;;; Mobile/touchscreen
;;;--------------------

(defun toggle-svkbd ()
  "Toggle onscreen keyboard."
  (interactive)
  (let* ((proc (get-process "svkbd"))
         (monitor-geometry (get-focused-monitor-geometry))
         (monitor-x (nth 0 monitor-geometry))
         (monitor-y (nth 1 monitor-geometry)))
    (if proc
        (kill-process proc)
      (progn
        (start-process "svkbd" nil "svkbd-mobile-intl" "-l" "minimal,symbols" "-d" "-g"
                       (format "%sx300+%s+%s"
                               (frame-pixel-width)
                               monitor-x
                               monitor-y))
        (sleep-for 0.1)
        (set-frame-height (selected-frame)
                          (- (frame-pixel-height)
                             300
                             (if tab-bar-mode
                                 (tab-bar-height nil t)
                               0))
                          nil t)))))


;;; Random bullshit
;;;-----------------


(defun tmux-navigate-directions ()
  "Navigate in tmux."
  (let* ((x (nth 0 (window-edges)))
         (y (nth 1 (window-edges)))
         (w (nth 2 (window-edges)))
         (h (nth 3 (window-edges)))

         (can_go_up (> y 2))
         (can_go_down (<  (+ y h) (- (frame-height) 2)))
         (can_go_left (> x 1))
         (can_go_right (< (+ x w) (frame-width))))

    (send-string-to-terminal
     (format "\e]2;emacs %s #%s\a"
    (buffer-name)
        (string
          (if can_go_up    ?U 1)
          (if can_go_down  ?D 1)
          (if can_go_left  ?L 1)
          (if can_go_right ?R 1))))))

(unless (display-graphic-p)
  (add-hook 'buffer-list-update-hook 'tmux-navigate-directions))



(use-package shrface
  :ensure t
  :defer t
  :functions
  shrface-basic
  shrface-trial
  shrface-default-keybindings
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  :custom
  (shrface-href-versatile t))


(use-package eww
  :defer t
  :requires
  shrface
  :config
  (add-hook 'eww-after-render-hook #'shrface-mode))


;; TODO one of the following options disables shrface conversion to org-mode headings
;; Figure out what and fix it
;;(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))


(use-package bitbake
  :ensure t
  :defer t
  :mode "bitbake-mode"
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(bb\\|bbappend\\|bbclass\\|inc\\|conf\\)\\'" . bitbake-mode)))


(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq-default TeX-master "main") ; All master files called "main".
  :custom
  (TeX-view-program-list '(("Okular" "okular --noraise --unique file:%o#src%n%a")))
  (TeX-view-program-selection '((output-pdf "Okular"))))


;;(use-package scad-dbus
;;  :defer t
;;  :quelpa (scad-dbus :fetcher github :repo "Lenbok/scad-dbus"))


(use-package chess
  :ensure t
  :defer t
  :custom
  (chess-images-separate-frame nil)
  :config
  (evil-define-key 'normal chess-display-mode-map (kbd "<down-mouse-1>") 'chess-display-mouse-select-piece))



(use-package xkcd
  :ensure t
  :defer t
  :functions
  xkcd
  xkcd-get
  :hook
  (xkcd-mode . olivetti-mode)
  (xkcd-mode . (lambda ()
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                     (set (make-local-variable 'evil-evilified-state-cursor) (list nil))))
  :bind
  (:map xkcd-mode-map
        ("h" . xkcd-prev)
        ("l" . xkcd-next)))



(defun get-focused-monitor-geometry (&optional frame)
  "Get the geometry of the monitor displaying FRAME in EXWM."
  (let* ((monitor-attrs (frame-monitor-attributes frame)))
    ;; Returns (x y width height)
    (alist-get 'workarea monitor-attrs)))

(defun split-window-below-and-switch-buffer ()
  "Make a new window below and focus it."
  (interactive)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer (other-buffer)))

(defun split-window-right-and-switch-buffer ()
  "Make a new window to the right and focus it."
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (other-buffer)))

(defun exwm-list-x-windows ()
  "List all EXWM mode buffers."
  (interactive)
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (eq major-mode 'exwm-mode)))
              (buffer-list)))

(defun exwm-buffer->pid (buf)
  "Get the PID of an EXWM buffer BUF."
  (let* ((id (exwm--buffer->id buf))
         (resp (xcb:+request-unchecked+reply
                   exwm--connection
                   (make-instance 'xcb:ewmh:get-_NET_WM_PID
                                  :window id))))
    (slot-value resp 'value)))

(defun get-sink-input-pids ()
  "Get list of PIDs for active PulseAudio sink inputs."
  (let ((output (shell-command-to-string "pacmd list-sink-inputs"))
        (pids '()))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward "application.process.id = \"\\([0-9]+\\)\"" nil t)
        (push (string-to-number (match-string 1)) pids)))
    pids))

(defun exwm-list-sound-playing-buffers ()
  "List buffers playing sound.
Might give duplicates, if a process has multiple windows."
  (let ((window-pids (mapcar #'exwm-buffer->pid (exwm-list-x-windows))))
    (cl-intersection window-pids (get-sink-input-pids))))


(use-package emacs
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

;;; init.el ends here
