;; Set the default directory to the home directory regardless of where Emacs was
;; started.
(setq default-directory "~/")

;; Define a custom variable for the path to the shared notes directory.
(defvar cy/shared-notes-dir "~/Dropbox/Notes/")

;; Set file shortcut registers.
(set-register ?t (cons 'file (concat cy/shared-notes-dir "Todo.txt")))
(set-register ?v (cons 'file (concat cy/shared-notes-dir "Vocab.txt")))
(set-register ?s (registerv-make
                  "jump to scratch buffer"
                  :jump-func (lambda (k) (cy/jump-to-scratch-buffer))
                  :print-func (lambda (k) (princ "jump to scratch buffer"))))

;; Set frame title.
(setq frame-title-format '(buffer-file-name "%f - Emacs" "Emacs"))

;; Turn off the splash screen.
(setq inhibit-splash-screen t)

(defun set-window-system-settings (frame)
  "Sets various window settings for the given frame"
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

;; Prevent leftover backup turds.
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Use the custom wombat theme.
(load-theme 'wombat t)

;; Stop asking me to type "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Make text mode the default mode for new buffers, and for the scratch buffer.
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Use 4-character tabs.
(setq tab-width 4)

;; Use 80-character lines.
(setq-default fill-column 80)

;; Never allow emacs to automatically split windows horizontally (i.e., position
;; windows to the left/right).
(setq split-width-threshold nil)

;; TODO (CY): Come back to this.
;; Enable electric pair mode for auto-pairing delimiters. Disable
;; `electric-pair-preserve-balance' so as not to insert extraneous double-quotes
;; when there's an unbalanced pairing.
;; (See https://lists.gnu.org/archive/html/emacs-devel/2014-04/msg00020.html
(electric-pair-mode t)
;; (setq electric-pair-preserve-balance nil)

;; Set whitespace mode to highlight column 80+ chars, as well as any trailing
;; whitespaces.
(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

;; Delete trailing whitespaces on file save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Show line numbers. Seriously, who codes without line numbers?
(global-linum-mode t)
(setq linum-format "%d ")

;; Highlight matching parentheses.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Highlight matches immediately when performing incremental search.
(setq lazy-highlight-initial-delay 0)

;; Enable delete-selection mode so that pasting while an active region is
;; selected overwrites that region.
(delete-selection-mode t)

;; Display the current column number in the mode line.
(column-number-mode t)

;; Use ssh in tramp mode by default
(setq tramp-default-method "ssh")

;; Enable winner-mode for managing window configurations.
(winner-mode t)

;; Turn on visual line mode for text mode. This enables word-wrap in the current
;; buffer, and rebinds C-a, C-e, etc. to operate on visual lines instead of
;; logical lines.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Make autocompletion for buffer names case-insensitive.
(setq read-buffer-completion-ignore-case 1)

;; Stop emacs from prompting us before killing buffers in daemon mode.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Enable midnight mode. Kill *all* buffers that haven't been accessed in 1 day.
(require 'midnight)
(setq clean-buffer-list-delay-general 1)

;; Omit hidden files in dired mode.
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; When using wgrep, auto-save buffers after we finish editing. Also bind "e" to
;; enable wgrep mode to be consistent with hotkey for occur-edit-mode.
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-enable-key "e")

;; Jump to grep-mode, occur-mode, and help-mode buffers as soon as they're
;; active.
(add-hook 'grep-mode-hook (lambda () (pop-to-buffer (get-buffer "*grep*"))))
(add-hook 'occur-hook (lambda () (pop-to-buffer occur-buf)))
(add-hook 'help-mode-hook (lambda () (pop-to-buffer (get-buffer "*Help*"))))

;; Monkey patch to ignore -C -C option from git blame command.
;; Source: http://blog.kyanny.me/entry/2014/08/16/022311
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "Remove '-C -C' option from `git blame` command"
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; Files with extensions in the completion-ignored-extensions list (e.g., *.pyc,
;; *.pyo) should be omitted from the file completions list.
;; Source: http://stackoverflow.com/questions/1731634/dont-show-uninteresting-files-in-emacs-completion-window#1731634
(defadvice completion-file-name-table (after
                                       ignoring-backups-f-n-completion
                                       activate)
  "Filter out results when the have completion-ignored-extensions"
  (let ((res ad-return-value))
    (if (and (listp res)
             (stringp (car res))
             ;; length > 1, don't ignore sole match
             (cdr res))
        (setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

;; Set the default browser to chrome.
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

;; When using OS X, ensure the PATH inside emacs is the same as in the
;; shell. This ensures that binaries that are installed via Homebrew, `go get`,
;; etc. are visible to emacs.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
