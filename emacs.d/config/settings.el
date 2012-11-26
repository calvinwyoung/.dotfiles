;; Set window title
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Turn off the splash screen
(setq inhibit-splash-screen t)

;; Remove the menu bar and toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Set fringe mode
(fringe-mode '(0 . 1))

;; Prevent leftover backup turds
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Use wombat color-theme in window mode only. Use the default color theme if in
;; terminal mode.
(load-file (concat vendor-dir "color-theme-wombat.el"))
(defun set-frame-color-theme (frame)
  "Sets the color theme for the given frame"
  (select-frame frame)
  (let ((color-theme-is-global nil))
    (if (window-system frame)
        (color-theme-wombat))))

;; Add a hook to set the frame's color theme each time a new frame is created in
;; daemon/client mode.
(add-hook 'after-make-frame-functions 'set-frame-color-theme)

;; Set the current frame's color theme if emacs was started normally.
(set-frame-color-theme (selected-frame))

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

;; Omit hidden files in dired mode
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Set the default browser to chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Integrate emacs and X clipboards
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Stop emacs from prompting us before killing buffers in daemon mode
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
