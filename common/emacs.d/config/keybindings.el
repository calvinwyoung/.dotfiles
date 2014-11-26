;;;;;;;;;;;
;; Editing
;;;;;;;;;;;

;; Add commands for redo. The first binding is for GUI mode, and the second one
;; is for terminal mode.
(global-set-key (kbd "C-M-/") 'redo)
(global-set-key (kbd "C-M-_") 'redo)

;; Bind both Ctrl + V and Meta + V to paste.
(global-set-key "\C-v" 'yank)
(global-set-key "\M-v" 'yank)

;; Bind Ctrl + Enter to enable rectangle-mark-mode (a la cua-selection-mode).
(global-set-key (kbd "<C-return>") 'rectangle-mark-mode)

;; Make Ctrl + W kill previous word with custom function.
(global-set-key "\C-w" 'backward-kill-word)
(define-key (current-global-map) [remap backward-kill-word]
  'my-backward-kill-word)

;; Make it easier to execute goto line.
(global-set-key "\M-g" 'goto-line)

;; Behave like vim's open line commands.
(global-set-key "\C-\M-o" 'open-previous-line)
(global-set-key "\C-o" 'open-next-line)

;; Map some copy chords.
(global-set-key "\C-xl" 'mark-line)
(global-set-key "\C-cw" 'copy-word)
(global-set-key "\C-cl" 'copy-line)

;; Duplicate line and optionally comment it.
(global-set-key "\C-cy" 'duplicate-line)
(global-set-key "\C-c;" 'duplicate-line-and-comment)

;; Make it easier to move lines and regions.
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<left>") 'shift-text-left)
(global-set-key (kbd "M-<right>") 'shift-text-right)
(global-set-key (kbd "M-[") (lambda() (interactive) (shift-text-left 4)))
(global-set-key (kbd "M-]") (lambda() (interactive) (shift-text-right 4)))

;; Use custom comment function.
(global-set-key "\M-;" 'comment-dwim-line)

;; Emulate vim's "%" command for moving to a matching parentheses.
(global-set-key [?\C-%] 'goto-match-paren)

;; Emulate vim's "*" command for searching for the word under the cursor.
(global-set-key [?\C-*] 'my-isearch-word-at-point)

;; Easier window switching.
(global-set-key "\C-\M-k" 'windmove-up)
(global-set-key "\C-\M-j" 'windmove-down)

;; Define custom minor mode keys.
(defvar my-keys-map (make-keymap) "my-keys keymap.")
(define-minor-mode my-keys
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keys-map)

;; Scroll screen up and down.
(define-key my-keys-map "\C-\M-p" (lambda() (interactive) (scroll-down 5)))
(define-key my-keys-map "\C-\M-n" (lambda() (interactive) (scroll-up 5)))

;; Move up and down by 5 lines with M-n and M-p..
(define-key my-keys-map "\M-n" (lambda() (interactive) (next-line 10)))
(define-key my-keys-map "\M-p" (lambda() (interactive) (previous-line 10)))

;;;;;;;;;;;;;;;;;;;;;
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;

;; Use a simpler buffer list.
(global-set-key "\C-x\C-b" 'bs-show)
(setq bs-default-configuration "files-and-scratch")

;; Easier buffer switching. The naming here is kind of confusing -- the
;; `cycle-buffer` command walks DOWN the stack (i.e., calling it will show the
;; most recently used buffer). `cycle-buffer-backward` goes in the opposite
;; direction.
(global-set-key "\C-\M-h" 'cycle-buffer)
(global-set-key "\C-\M-l" 'cycle-buffer-backward)

;;;;;;;;;;;;;;;;;;;
;; File management
;;;;;;;;;;;;;;;;;;;

;; Make it easier to sudo edit files.
(global-set-key "\C-x\C-\M-f" 'sudo-find-file)

;; Enable custom rename file and buffer command.
(global-set-key "\C-xR" 'rename-file-and-buffer)

;; Enable easier file deletion.
(global-set-key "\C-xD" 'delete-current-file)

;; Custom key map for reusing dired buffers with the `dired-single` plugin.
(defun my-dired-keys-map ()
  "Custom key mappings to allow reusing single buffer in dired "
  ;; <add other stuff here>
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
  (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^" (lambda()
                                   (interactive)
                                   (dired-single-buffer ".."))))

;; If dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; We're good to go; just add our bindings.
    (my-dired-keys-map)
  ;; It's not loaded yet, so add our bindings to the load-hook.
  (add-hook 'dired-load-hook 'my-dired-keys-map))

;; Override the default dired binding to open the "magic buffer" in the current
;; file's directory.
(global-set-key "\C-xd" (lambda()
                          (interactive)
                          (dired-single-magic-buffer default-directory)))

;;;;;;;;;
;; Misc.
;;;;;;;;;

;; Compile command.
(global-set-key [\C-f11] 'compile)
(global-set-key [\S-f11] 'kill-compilation)
(global-set-key [f11] 'next-error)

;; Execute extended command.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
