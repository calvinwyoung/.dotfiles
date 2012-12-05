;; Set window title
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Turn off the splash screen
(setq inhibit-splash-screen t)

;; Remove the menu bar and toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Hide the left fringe, and show a narrow right fringe
(fringe-mode '(0 . 1))

;; Prevent leftover backup turds
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Use wombat color-theme in window mode only. Use the default color theme if in
;; terminal mode
(load-file (concat vendor-dir "color-theme-wombat.el"))
(defun set-frame-color-theme (frame)
  "Sets the color theme for the given frame"
  (select-frame frame)
  (let ((color-theme-is-global nil))
    (if (window-system frame)
        (color-theme-wombat))))
;; Set the current frame's color theme, then add a hook to set the frame's color
;; theme each time a new frame is created in daemon/client mode
(set-frame-color-theme (selected-frame))
(add-hook 'after-make-frame-functions 'set-frame-color-theme)

;; Stop asking me to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Make initial scratch mode usable. Initialize it to org-mode by default
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use 4-character tabs
(setq tab-width 4)

;; Use 80-character lines
(setq-default fill-column 80)

;; Set whitespace mode to highlight column 80+ chars
(setq whitespace-style '(lines-tail)
      whitespace-line-column 80)

;; Make trailing whitespaces visible
(setq-default show-trailing-whitespace t)

;; Delete trailing whitespaces on file save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Enable mouse modes
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Show line numbers. Seriously, who codes without line numbers?
(global-linum-mode t)
(setq linum-format "%d ")

;; Highlight matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0)

;; Enable CUA mode for rectangle selection, but disable its key bindings
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Display the current column number in the mode line
(column-number-mode t)

;; Use ssh in tramp mode by default
(setq tramp-default-method "ssh")

;; Turn on visual line mode for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Omit hidden files in dired mode
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Set the default browser to chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Integrate emacs and X clipboards
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Stop emacs from prompting us before killing buffers in daemon mode
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Files with extensions in the completion-ignored-extensions list will be
;; omitted from the file completions list.
(defadvice completion--file-name-table (after
                                        ignoring-backups-f-n-completion
                                        activate)
  "Filter out results when the have completion-ignored-extensions"
  (let ((res ad-return-value))
    (if (and (listp res)
             (stringp (car res))
             (cdr res))                 ; length > 1, don't ignore sole match
        (setq ad-return-value
              (completion-pcm--filename-try-filter res)))))
