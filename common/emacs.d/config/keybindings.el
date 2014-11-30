;;;;;;;;;;;
;; Editing
;;;;;;;;;;;

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

;; Easier window switching.
(global-set-key "\C-\M-k" 'windmove-up)
(global-set-key "\C-\M-j" 'windmove-down)

;; Replace mark-sexp with the superior expand-region.
(global-set-key (kbd "C-M-SPC") 'er/expand-region)

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

;; Enable multiple cursors mode.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;
;; Find / replace
;;;;;;;;;;;;;;;;;;

;; Make it easier to call rgrep. Also make sure we hide the grep hider on
;; completion since it's not useful to us.
(global-set-key "\C-xg" 'rgrep)
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;; Make it easier to call occur. Use our custom function that defaults to the
;; symbol under the cursor.
(global-set-key "\C-xo" 'occur-symbol-at-point)

;; Emulate vim's "*" command for searching for the word under the cursor.
(global-set-key [?\C-*] 'isearch-forward-symbol-at-point)
(define-key isearch-mode-map [?\C-*] 'isearch-repeat-forward)

;;;;;;;;;;;;;;;;;;;;;
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;

;; Use a simpler buffer list.
(global-set-key "\C-x\C-b" 'bs-show)
(setq bs-default-configuration "files-and-scratch")

;;;;;;;;;;;;;;;;;;;
;; File management
;;;;;;;;;;;;;;;;;;;

;; Make it easier to sudo edit files.
(global-set-key "\C-x\C-\M-f" 'sudo-find-file)

;; Enable custom rename file and buffer command.
(global-set-key "\C-xR" 'rename-file-and-buffer)

;; Enable easier file deletion.
(global-set-key "\C-xD" 'delete-current-file)

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
