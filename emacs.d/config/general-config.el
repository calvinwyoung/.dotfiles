;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Set window title
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Prevent splash screen
(setq inhibit-splash-screen t)

;; Set fringe mode
(fringe-mode '(0 . 1))

;; Prevent leftover backup turds
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Stop asking me to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Make initial scratch mode usable
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Set tab width
(setq tab-width 4)

;; Set fill column
(setq-default fill-column 80)

;; Set whitespace mode to highlight column 80+ chars
(setq whitespace-style '(lines-tail)
      whitespace-line-column 80)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable mouse modes
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Enable line numbers
(global-linum-mode t)
(setq linum-format "%d ")

;; Enable paren mode
(show-paren-mode t)
(setq show-paren-delay 0)

;; Enable CUA mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Enable column number mode
(column-number-mode t)

;; Set default tramp mode protocol
(setq tramp-default-method "ssh")

;; Turn on visual line mode for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Discard trailing whitespace on file save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Set the default browser to chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Enable autopair mode plugin
(require 'autopair)
(autopair-global-mode t)

;; Integrate emacs and X clipboards
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Daemon mode settings
(if 'server-mode
    (progn
      (setq default-frame-alist '((vertical-scroll-bars)
                                  (left-fringe . 0)
                                  (right-fringe . 1)
                                  ; (cursor-color . "#64a8d8")
                                  (initial-major-mode . 'text-mode)
                                  ))
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
      ))
