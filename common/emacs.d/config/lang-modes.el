;;;;;;;;;;;;;
;; Prog mode
;;;;;;;;;;;;;
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Enable Fly Spell spell-checking in all programming modes.
            (flyspell-prog-mode)))

;;;;;;;;;;
;; Python
;;;;;;;;;;
(add-hook 'python-mode-hook
          (lambda ()
            ;; Enable whitespace mode for highlighting long lines.
            (whitespace-mode)))

;; PEP 8/PEP 257 prefers preceding the triple quotes at the end of a multiline
;; docstring with a blank line, I prefer using a docstring style that doesn't
;; insert the extra blank line.
;; (See http://www.python.org/dev/peps/pep-0008/#documentation-strings)
(setq python-fill-docstring-style 'pep-257-nn)

;;;;;;;;
;; Ruby
;;;;;;;;
(eval-after-load 'ruby-mode
  '(progn
     ;; Prevent ruby-mode from mapping Ctrl + Meta + h to backward-word-kill, for
     ;; which we already use Ctrl + w. Instead, we reserve this binding for
     ;; buffer-switching.
     (define-key ruby-mode-map (kbd "C-M-h") nil)))

;;;;;;;;;;;;
;; CSS/SCSS
;;;;;;;;;;;;
(autoload 'scss-mode "scss-mode.el"
  "Major mode for editing SCSS files" t)

(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;
(add-hook 'js2-mode-hook
          (lambda()
            ;; Enable extra imenu goodies
            (js2-imenu-extras-mode)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;
;; HTML
;;;;;;;;
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (setq sgml-basic-offset 4)

            ;; Enable flyspell-prog-mode. This gets enabled automatically for
            ;; major-modes that descend from `prog-mode', but unfortunately
            ;; `html-mode' descends from `text-mode'.
            (flyspell-prog-mode)))

(add-to-list 'auto-mode-alist '("\\.htmlmk$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jqt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

;;;;;;;
;; Lua
;;;;;;;
(setq lua-indent-level 4)

;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;
(add-hook 'org-mode-hook
          (lambda ()
            ;; Indent mode makes nested lists easier to look at.
            (org-indent-mode t)

            (setq tab-stop-list (number-sequence 2 200 2))
            (setq tab-width 2)
            (setq org-list-empty-line-terminates-plain-lists t)

            ;; Enable speed keys when cursor is at the beginning of a headline.
            (setq org-use-speed-commands t)

            ;; Causes Ctrl-A to move the cursor to the beginning of the list
            ;; contents (i.e., after the stars / bullet points / etc.)
            ;; (setq org-special-ctrl-a/e t)

            ;; Enable org-autolist-mode to make it easier to edit lists.
            (org-autolist-mode t)

            ;; Disable electric pair mode b/c it's awkward for writing prose.
            (setq-local electric-pair-inhibit-predicate (lambda() nil))

            ;; We want M-[ and M-] to shift list items in/out by a tabstop, so
            ;; we bind them here.
            (local-set-key (kbd "M-[") 'org-metaleft)
            (local-set-key (kbd "M-]") 'org-metaright)

            ;; We want to add custom keybindings for M-[ and M-], but to make
            ;; that work we need to unbind those keys from custom-keys-mode,
            ;; which would otherwise take precedence. Unfortunately this is a
            ;; bit tricky...
            ;; Source: http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
            (let ((oldmap (cdr (assoc 'custom-keys-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (define-key newmap (kbd "M-[") nil)
              (define-key newmap (kbd "M-]") nil)
              (define-key newmap (kbd "M-<up>") nil)
              (define-key newmap (kbd "M-<down>") nil)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(custom-keys-mode . ,newmap) minor-mode-overriding-map-alist))))

;; Open all txt files in org-mode.
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; Prevent org-mode from opening files in folded view by default. This needs to
;; be set outside of the org-mode-hook so it takes effect before org-mode is
;; loaded.
(setq org-startup-folded nil)

;; Enable org-capture.
(define-key global-map "\C-cc" 'org-capture)

;; Configure org-capture
(setq org-capture-templates
      '(("h" "Hi-pri"
         checkitem
         (file+headline (concat cy/shared-notes-dir "Todo.txt") "TODO (Hi-pri)")
         "- [ ] %?"
         :prepend t)
        ("l" "Low-pri"
         checkitem
         (file+headline (concat cy/shared-notes-dir "Todo.txt") "TODO (Low-pri)")
         "- [ ] %?"
         :prepend t)))

;;;;;;;;
;; Conf
;;;;;;;;
(add-hook 'conf-mode-hook
          (lambda ()
            ;; Must manually update tab stops to occur every 4 characters
            (setq tab-stop-list (number-sequence 4 200 4))))

;;;;;;;;
;; JAVA
;;;;;;;;
(add-hook 'java-mode-hook 'java-mode-hook)

;; Define Java specific hook
(defun java-mode-hook ()
  (local-set-key [C-f7] 'javac-current))

;; Compile the current Java buffer
(defun javac-current()
  (interactive)
  (compile (concat "javac " buffer-file-name)))

;;;;;;;;;;;;
;; C / C++
;;;;;;;;;;;;
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Define C mode common hook
(defun my-c-mode-common-hook ()
  (c-set-style "bsd")
  (setq c-tab-always-indent t)
  (setq c-basic-offset 4)
  (set-default-compile-command)
  (whitespace-mode))

;; Set default compile command
(defun set-default-compile-command ()
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s %s %s"
                   (or (getenv "CC") "g++")
                   (or (getenv "CFLAGS") "-g")
                   file)))))

;; Set defalut compilation window height
(setq compilation-window-height 10)

;; Reformat c code
;;  -st	 - print to stdout
;;  -bap	 - blank line after procedures
;;  -bli0 - braces below statements indented 0 spaces
;;  -cbi0 - braces below case statements indented 0 spaces
;;  -cdb  - put comment delimiters on blank lines
;;  -cdw	 - cuddle while statements next to } in do-while
;;  -cli4 - indent case statements by 4 spaces
;;  -ci4  - indent continued lines by 4 spaces
;;  -fca  - format comments that start after first column
;;  -fc1  - format comments that start at first column
;;  -i4	 - indent line continuations by 4 spaces
;;  -l79	 - break long lines to 80 chars per line
;;  -lc79 - break long comments to 80 chars per line
;;  -lp	 - line up start of continued lines with parenthesis
;;  -nbad - blank line after declaration
;;  -ncs	 - don't put a space between cast operators
;;  -npsl - put type of procecure on same line as its name
;;  -nut	 - use spaces instead of tabs
;;  -pcs	 - add a space after names of procedure calls

(defun c-reformat-buffer()
  (interactive)
  (save-buffer)
  (setq sh-indent-command
        (concat
         "indent -st -bap -bli0 -cbi0 -cdb -cdw -cli4 "
         "-ci4 -fca -fc1 -i4 -l79 -lc79 -lp -nbad -ncs "
         "-npsl -nut -pcs"
         buffer-file-name
         ))
  (mark-whole-buffer)
  (universal-argument)
  (shell-command-on-region
   (point-min)
   (point-max)
   sh-indent-command
   (buffer-name))
  (save-buffer))
