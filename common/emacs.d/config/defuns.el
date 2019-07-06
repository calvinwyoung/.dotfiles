;; Define custom minor mode keys.
(defvar custom-keys-mode-map (make-keymap)
  "Keymap for custom-keys-mode-mode minor mode.")

(define-minor-mode custom-keys-mode
  "A minor mode so that my key settings override annoying major modes."
  t nil 'custom-keys-mode-map)

;; Emulate vim's o and O commands for opening new lines above/below the current
;; line.
;; Based on: http://www.emacswiki.org/emacs/OpenNextLine
(defun cy/open-next-line (arg)
  "Move to the next line and then opens a line.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun cy/open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

;; Copy word without selection
(defun cy/copy-word (&optional arg)
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
(defun cy/copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
   (interactive "P")
   (let ((beg (line-beginning-position))
         (end (line-beginning-position (+ 1 (if arg arg 1)))))
     (copy-region-as-kill beg end)))

;; Vim-like custom backward-kill-word function
(defun cy/backward-kill-word (&optional arg)
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
                       (max (save-excursion (backward-word) (point))
                            (line-beginning-position))))))))

;; Also add support for deleting words backwards without adding them to the yank
;; ring.
;; Source: http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs#6133921
(defun cy/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Wrap occur-mode to default to the symbol under the cursor if one exists.
;; Source: https://groups.google.com/forum/#!topic/gnu.emacs.help/3hFe5aSs3kM
(defun cy/occur-symbol-at-point ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        ; regexp-history defvared in replace.el
        (push (regexp-quote sym) regexp-history))
    (call-interactively 'occur)))

;; Similar to `isearch-occur', activates rgrep and uses the last isearch query
;; string as the default regexp. This should only be called from isearch-mode.
(defun cy/isearch-rgrep ()
  "Runs `rgrep' using the last search string as the regexp."
  (interactive)
  (let ((read-regexp-defaults-function (lambda nil
                                         (if isearch-regexp
                                             isearch-string
                                           (regexp-quote isearch-string)))))
    (isearch-exit)
    (call-interactively 'rgrep)))

;; Find all non-ascii characters in the current buffer.
(defun cy/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; Change default behavior of comment-dwim.
(defun cy/comment-dwim (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at
the end of the line, then comment current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

;; Duplicate a line, optionally commenting the original.
;; Based on: http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun cy/duplicate-line (&optional comment)
  "Duplicates the current line, and comments the original."
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (if comment
        (comment-region (region-beginning) (region-end)))
    (insert-string
     (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

(defun cy/duplicate-line-and-comment ()
  "Duplicates the current line, and comments the original."
  (interactive)
  (cy/duplicate-line t))

;; Emulate vim's "%" command to match parentheses.
;; Source: http://www.emacswiki.org/emacs/NavigatingParentheses#toc2
(defun cy/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

;; Source: https://github.com/bbatsov/crux/blob/master/crux.el#L261
(defun cy/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Source: http://stackoverflow.com/questions/95631/open-a-file-with-su-sudo-inside-emacs#7043786
(defun cy/sudo-find-file (file-name)
  "Opens a file as sudo"
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Source: http://xahlee.blogspot.com/2010/05/emacs-command-to-delete-current-file.html
(defun cy/delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.  If no file is associated, just
close buffer without prompt for save."
  (interactive)
  (let (current-file)
    (setq current-file (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file?: " current-file " "))
      (kill-buffer (current-buffer))
      (when (not (equal current-file nil))
        (delete-file current-file)))))

;; Source: http://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun cy/jump-to-scratch-buffer ()
  "Jumps to the scratch buffer, creating it if necessary."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;; Make it easier to move text in all direction.
;; Source: http://www.emacswiki.org/emacs/MoveText
(defun cy/move-text-vertically (arg)
  "Move a region or line of text args lines up or down."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun cy/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg
lines down."
  (interactive "*p")
  (cy/move-text-vertically arg))

(defun cy/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg
lines up."
  (interactive "*p")
  (cy/move-text-vertically (- arg)))

;; Source: http://stackoverflow.com/questions/3156450/shift-a-region-or-line-in-emacs#3156642
(defun cy/shift-text-horizontally (arg)
  "Shift a region of text args columns left or right."
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning) (region-end) arg)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position) (line-end-position) arg)))

(defun cy/shift-text-right (arg)
  "Shift region (transient-mark-mode active) or current line arg
columns right."
  (interactive "p")
  (cy/shift-text-horizontally arg))

(defun cy/shift-text-left (arg)
  "Shift region (transient-mark-mode active) or current line arg
columns left."
  (interactive "p")
  (cy/shift-text-horizontally (- arg)))

(defun cy/shift-text-tabstop-right (arg)
  "Shift region (transient-mark-mode active) or current line arg
tabstops right."
  (interactive "p")
  (cy/shift-text-horizontally (* arg (cy/major-mode-shift-offset))))

(defun cy/shift-text-tabstop-left (arg)
  "Shift region (transient-mark-mode active) or current line arg
tabstops left."
  (interactive "p")
  (cy/shift-text-horizontally (- (* arg (cy/major-mode-shift-offset)))))

;; Source: https://github.com/hbin/smart-shift
(defvar cy/major-mode-shift-offsets-alist
  '((lisp-mode . lisp-body-indent)
    (emacs-lisp-mode . lisp-body-indent)

    ;; Modes directly supported by CC Mode
    (c-mode . c-basic-offset)
    (c++-mode . c-basic-offset)
    (objc-mode . c-basic-offset)
    (java-mode . c-basic-offset)
    (idl-mode . c-basic-offset)
    (pike-mode . c-basic-offset)
    (awk-mode . c-basic-offset)

    (ruby-mode . ruby-indent-level)
    (python-mode . python-indent-offset)
    (swift-mode . swift-indent-offset)

    (js-mode . js-indent-level)
    (js2-mode . js2-basic-offset)
    (coffee-mode . coffee-tab-width)

    (css-mode . css-indent-offset)
    (slim-mode . slim-indent-offset)
    (html-mode . sgml-basic-offset)
    (web-mode . (lambda ()
                  (cond ((string= web-mode-content-type "css")
                         web-mode-css-indent-offset)
                        ((member web-mode-content-type
                                 '("javascript" "json" "jsx" "php"))
                         web-mode-code-indent-offset)
                        (t web-mode-markup-indent-offset)))) ; xml, html, etc...

    (sh-mode . sh-basic-offset)
    (yaml-mode . yaml-indent-offset)
    (text-mode . tab-width)
    (fundamental-mode . tab-width))
  "Alist which maps major modes to its indentation-level.")

(defun cy/major-mode-shift-offset ()
  "Returns the indentation-level of current major mode."
  (let ((offset (assoc-default major-mode cy/major-mode-shift-offsets-alist
                               (lambda (k v)
                                 (derived-mode-p k)))))
    (cond ((numberp offset) offset)
          ((functionp offset) (funcall offset))
          ((symbolp offset) (symbol-value offset))
          (t tab-width))))

(defun cy/convert-to-unix-file ()
  "Converts line endings to unix-style '\n' line endings."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))
