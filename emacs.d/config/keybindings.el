;; Enable auto indenting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Easier buffer list
(global-set-key "\C-x\C-b" 'bs-show)
(setq bs-default-configuration "files-and-scratch")

;; Make Ctrl+v paste to integrate with clipboard manager
(global-set-key "\C-v" 'cua-paste)

;; Make Ctrl+W kill previous word with custom function
(global-set-key "\C-w" 'backward-kill-word)
(define-key (current-global-map) [remap backward-kill-word]
  'my-backward-kill-word)

;; Execute extended command
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Compile command
(global-set-key [\C-f11] 'compile)
(global-set-key [\S-f11] 'kill-compilation)
(global-set-key [f11] 'next-error)

;; Easier goto line
(global-set-key "\M-g" 'goto-line)

;; Behave like vim's open line commands
(global-set-key "\C-\M-o" 'open-previous-line)
(global-set-key "\C-o" 'open-next-line)

;; Map some copy chords
(global-set-key "\C-xl" 'mark-line)
(global-set-key "\C-cw" 'copy-word)
(global-set-key "\C-cl" 'copy-line)

;; Use custom comment function
(global-set-key "\M-;" 'comment-dwim-line)

;; Enable redo
(load-file "~/.emacs.d/vendor/redo.el")
(require 'redo)
(global-set-key (kbd "C-M-/") 'redo)    ; for window-system
(global-set-key (kbd "C-M-_") 'redo)    ; for termminal

;; Emulate vim's "%" command to match parentheses
(global-set-key [?\C-%] 'goto-match-paren)

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

;;;;;;;;;;;
;; DEFUNS
;;;;;;;;;;;

;; Behave like vim's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vim's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause
them to autoindent.")

;; Mark line without selection
(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

;; Copy word without selection
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (let ((beg (progn
               (if (looking-back "[a-zA-Z0-9]" 1)
                   (backward-word 1))
               (point)))
        (end (progn
               (forward-word arg)
               (point))))
    (copy-region-as-kill beg end)))

;; Copy line without selection
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
   (interactive "P")
   (let ((beg (line-beginning-position))
         (end (line-end-position arg)))
     (copy-region-as-kill beg end)))


;; Change default behavior of comment-dwim
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at
the end of the line, then comment current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

;; Define custom backward-kill-word function
(defun my-backward-kill-word (&optional arg)
  "Replacement for the backward-kill-word command
If the region is active, then invoke kill-region.  Otherwise, use the
following custom backward-kill-word procedure.
If the previous word is on the same line, then kill the previous word.
Otherwise, if the previous word is on a prior line, then kill to the
beginning of the line.  If point is already at the beginning of the line,
then kill to the end of the previous line.

With argument ARG and region inactive, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (kill-region (mark) (point))
    (let (count)
      (dotimes (count arg)
        (if (bolp)
            (kill-backward-chars 1)
          (kill-region (point)
                       (max (save-excursion (backward-word)(point))
                            (line-beginning-position))))))
    ))

;; Emulate vim's "%" command to match parentheses
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))
