;;;;;;;;;;;
;; Editing
;;;;;;;;;;;

;; Bind both Ctrl + V and Meta + V to paste.
(define-key custom-keys-mode-map (kbd "C-v") 'yank)
(define-key custom-keys-mode-map (kbd "M-v") 'yank)

;; Bind Ctrl + Enter to enable rectangle-mark-mode (a la cua-selection-mode).
(define-key custom-keys-mode-map (kbd "<C-return>") 'rectangle-mark-mode)

;; Make Ctrl + W kill previous word with custom function.
(define-key custom-keys-mode-map (kbd "C-w") 'backward-kill-word)

;; Remap backward-kill-word to my-backward-kill-word everywhere. In mini-buffer
;; mode, remap it instead to backward-delete-word, which doesn't save to the
;; kill ring.
(define-key custom-keys-mode-map [remap backward-kill-word] 'my-backward-kill-word)
(define-key minibuffer-local-map [remap backward-kill-word] 'backward-delete-word)

;; Make it easier to execute goto line.
(define-key custom-keys-mode-map (kbd "M-g") 'goto-line)

;; Behave like vim's open line commands.
(define-key custom-keys-mode-map (kbd "C-M-o") 'open-previous-line)
(define-key custom-keys-mode-map (kbd "C-o") 'open-next-line)

;; Map some copy chords.
(define-key custom-keys-mode-map (kbd "C-x l") 'mark-line)
(define-key custom-keys-mode-map (kbd "C-c w") 'copy-word)
(define-key custom-keys-mode-map (kbd "C-c l") 'copy-line)

;; Duplicate line and optionally comment it.
(define-key custom-keys-mode-map (kbd "C-c y") 'duplicate-line)
(define-key custom-keys-mode-map (kbd "C-c ;") 'duplicate-line-and-comment)

;; Make it easier to move lines and regions.
(define-key custom-keys-mode-map (kbd "M-<up>") 'move-text-up)
(define-key custom-keys-mode-map (kbd "M-<down>") 'move-text-down)
(define-key custom-keys-mode-map (kbd "M-<left>") 'shift-text-left)
(define-key custom-keys-mode-map (kbd "M-<right>") 'shift-text-right)
(define-key custom-keys-mode-map (kbd "M-[") (lambda() (interactive) (shift-text-left 4)))
(define-key custom-keys-mode-map (kbd "M-]") (lambda() (interactive) (shift-text-right 4)))

;; Use custom comment function.
(define-key custom-keys-mode-map (kbd "M-;") 'comment-dwim-line)

;; Emulate vim's "%" command for moving to a matching parentheses.
(define-key custom-keys-mode-map (kbd "C-%") 'goto-match-paren)

;; Easier window switching.
(define-key custom-keys-mode-map (kbd "C-M-k") 'windmove-up)
(define-key custom-keys-mode-map (kbd "C-M-j") 'windmove-down)

;; Replace mark-sexp with the superior expand-region.
(define-key custom-keys-mode-map (kbd "C-M-SPC") 'er/expand-region)

;; Scroll screen up and down.
(define-key custom-keys-mode-map (kbd "C-M-p") (lambda() (interactive) (scroll-down 5)))
(define-key custom-keys-mode-map (kbd "C-M-n") (lambda() (interactive) (scroll-up 5)))

;; Move up and down by 5 lines with M-n and M-p.
(define-key custom-keys-mode-map (kbd "M-n") (lambda() (interactive) (next-line 10)))
(define-key custom-keys-mode-map (kbd "M-p") (lambda() (interactive) (previous-line 10)))

;; Enable multiple cursors mode.
(define-key custom-keys-mode-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key custom-keys-mode-map (kbd "C->") 'mc/mark-next-like-this)
(define-key custom-keys-mode-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key custom-keys-mode-map (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;
;; Find / replace
;;;;;;;;;;;;;;;;;;

;; Make it easier to call rgrep. Also make sure we hide the grep hider on
;; completion since it's not useful to us.
(define-key custom-keys-mode-map (kbd "C-x g") 'rgrep)

;; When in grep mode, let the TAB key show the error source without switching
;; focus to it. Also add hotkeys to occur-mode to make it behave like grep-mode.
(define-key grep-mode-map (kbd "<tab>") 'compilation-display-error)
(define-key occur-mode-map (kbd "<tab>") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-error-no-select)
(define-key occur-mode-map (kbd "p") 'previous-error-no-select)

;; Make it easier to call occur. Use our custom function that defaults to the
;; symbol under the cursor.
(define-key custom-keys-mode-map (kbd "C-x o") 'occur-symbol-at-point)
(define-key isearch-mode-map (kbd "C-x o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-x g") 'isearch-rgrep)

;; Emulate vim's "*" command for searching for the word under the cursor.
(define-key custom-keys-mode-map (kbd "C-*") 'isearch-forward-symbol-at-point)
(define-key isearch-mode-map (kbd "C-*") 'isearch-repeat-forward)

;;;;;;;;;;;;;;;;;;;;;
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;

;; Use a simpler buffer list.
(define-key custom-keys-mode-map (kbd "C-x C-b") 'bs-show)
(setq bs-default-configuration "files-and-scratch")

;; Make it easy to kill both a buffer and the window it's in.
(define-key custom-keys-mode-map (kbd "C-x K") 'kill-buffer-and-window)

;;;;;;;;;;;;;;;;;;;
;; File management
;;;;;;;;;;;;;;;;;;;

;; Make it easier to sudo edit files.
(define-key custom-keys-mode-map (kbd "C-x C-M-f") 'sudo-find-file)

;; Enable custom rename file and buffer command.
(define-key custom-keys-mode-map (kbd "C-x R") 'rename-file-and-buffer)

;; Enable easier file deletion.
(define-key custom-keys-mode-map (kbd "C-x D") 'delete-current-file)

;;;;;;;;;
;; Misc.
;;;;;;;;;

;; Use C-p and C-n to cycle through minibuffer history.
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

;; Disable custom keys in minibuffer mode.
(add-hook 'minibuffer-setup-hook (lambda () (custom-keys-mode 0)))

;; Execute extended command.
(define-key custom-keys-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key custom-keys-mode-map (kbd "C-c C-m") 'execute-extended-command)

;; Compile command.
(define-key custom-keys-mode-map [C-f11] 'compile)
(define-key custom-keys-mode-map [S-f11] 'kill-compilation)
(define-key custom-keys-mode-map [f11] 'next-error)
(define-key custom-keys-mode-map [f12] 'previous-error)
