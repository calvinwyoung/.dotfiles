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
     (define-key ruby-mode-map "\C-\M-h" nil)))

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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;
;; HTML
;;;;;;;;
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (setq sgml-basic-offset 4)))

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
            (setq tab-stop-list (number-sequence 2 200 2))
            (setq tab-width 2)
            (org-indent-mode t)
            (local-set-key (kbd "M-[") 'org-metaleft)
            (local-set-key (kbd "M-]") 'org-metaright)))

;; Prevent org-mode from opening files in folded view by default.
(setq org-startup-folded nil)

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
