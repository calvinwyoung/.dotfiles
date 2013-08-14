;; Set window title
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Turn off the splash screen
(setq inhibit-splash-screen t)

(defun set-window-system-settings (frame)
  "Sets the color theme for the given frame"
  (select-frame frame)
  (if (window-system frame)
      ;; GUI emacs settings
      (progn
        ;; Set fringe mode (undefined in terminal mode)
        (fringe-mode '(0 . 1))
        ;; Never show the toolbar
        (tool-bar-mode 0)
        ;; Never show the menubar, unless we're on a Mac, in which case it's
        ;; useful.
        (if (not (eq system-type 'darwin))
            (menu-bar-mode 0)
          )
        )
    (progn
      (menu-bar-mode 0)

      ;; Enable mouse support in terminal mode
      ;; (require 'mouse)
      (xterm-mouse-mode t)
      ;; (mouse-wheel-mode t)

      ;; ;; Enable selection
      ;; (setq mouse-sel-mode t)

      ;; ;; Mouse clicks should change focus when clicking in split panes
      ;; (defun track-mouse (e))

      ;; ;; Enable mouse wheel
      ;; (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
      ;; (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
      )
    )
  )

;; We need to use a more complex system for configuring settings that depend on
;; the window system since we sometimes use emacs in server/client mode where
;; clients can be created in either GUI or terminal modes. This implementation
;; configures those settings each time a new frame is created.
(set-window-system-settings (selected-frame))
(add-hook 'after-make-frame-functions 'set-window-system-settings)

;; Prevent leftover backup turds
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Use the default Emacs24 wombat color theme.
(load-theme 'wombat t)

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

;; Enable winner-mode for managing window configurations
(winner-mode t)

;; Turn on visual line mode for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Make autocompletion for buffer names case-insensitive
(setq read-buffer-completion-ignore-case 1)

;; Kill buffers that have been open for a long time
(require 'midnight)
(setq clean-buffer-list-delay-general 1)

;; Omit hidden files in dired mode
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Set the default browser to chrome
(if (eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome")
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;; Integrate emacs and X clipboards on GNU/Linux systems. This works by default
;; in Darwin systems.
(if (eq system-type 'gnu/linux)
    (progn
      (setq x-select-enable-clipboard t)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

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
