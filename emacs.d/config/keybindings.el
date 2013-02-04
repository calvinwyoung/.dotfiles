;; Map Mac "Command" key to "Meta"
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

;; Enable auto indenting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Easier buffer list
(global-set-key "\C-x\C-b" 'bs-show)
(setq bs-default-configuration "files-and-scratch")

;; Bind both Ctrl+v and Meta+v to paste
(global-set-key "\C-v" 'cua-paste)
(global-set-key "\M-v" 'cua-paste)

;; Make Ctrl+W kill previous word with custom function
(global-set-key "\C-w" 'backward-kill-word)
(define-key (current-global-map) [remap backward-kill-word]
  'my-backward-kill-word)

;; Execute extended command
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Make it easier to execute goto line
(global-set-key "\M-g" 'goto-line)

;; Behave like vim's open line commands
(global-set-key "\C-\M-o" 'open-previous-line)
(global-set-key "\C-o" 'open-next-line)

;; Map some copy chords
(global-set-key "\C-xl" 'mark-line)
(global-set-key "\C-cw" 'copy-word)
(global-set-key "\C-cl" 'copy-line)

;; Duplicate a line and comment it
(global-set-key "\C-cy" 'duplicate-line-and-comment)

;; Make it easier to move lines and regions
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<left>") 'shift-text-left)
(global-set-key (kbd "M-<right>") 'shift-text-right)
(global-set-key (kbd "M-[") (lambda() (interactive) (shift-text-left 4)))
(global-set-key (kbd "M-]") (lambda() (interactive) (shift-text-right 4)))

;; Use custom comment function
(global-set-key "\M-;" 'comment-dwim-line)

;; Enable custom rename file and buffer command
(global-set-key "\C-cr" 'rename-file-and-buffer)

;; Make it easier to sudo edit files
(global-set-key "\C-x\C-\M-f" 'sudo-find-file)

;; Emulate vim's "%" command to match parentheses
(global-set-key [?\C-%] 'goto-match-paren)

;; Compile command
(global-set-key [\C-f11] 'compile)
(global-set-key [\S-f11] 'kill-compilation)
(global-set-key [f11] 'next-error)

;; Define custom minor mode keys
(defvar my-keys-map (make-keymap) "my-keys keymap.")

(define-minor-mode my-keys
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keys-map)

;; Scroll screen up and down
(define-key my-keys-map "\C-\M-p" (lambda() (interactive) (scroll-down 5)))
(define-key my-keys-map "\C-\M-n" (lambda() (interactive) (scroll-up 5)))

;; Move up and down by 5 lines with M-n and M-p
(define-key my-keys-map "\M-n" (lambda() (interactive) (next-line 10)))
(define-key my-keys-map "\M-p" (lambda() (interactive) (previous-line 10)))

;; Set windmove keybindings
(global-set-key "\C-\M-h" 'windmove-left)
(global-set-key "\C-\M-l" 'windmove-right)
(global-set-key "\C-\M-k" 'windmove-up)
(global-set-key "\C-\M-j" 'windmove-down)
