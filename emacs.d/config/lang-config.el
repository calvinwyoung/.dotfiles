;;;;;;;;;
;; HTML
;;;;;;;;;
(add-hook 'html-mode-hook
     (lambda ()
       ;; Default indentation is usually 2 spaces, changing to 4.
       (setq sgml-basic-offset 4)))

;; Set html-mode as default for .htmltt files
(add-to-list 'auto-mode-alist '("\\.htmltt$" . html-mode))

;; Set html-mode as default for .jqt files
(add-to-list 'auto-mode-alist '("\\.jqt$" . html-mode))

;;;;;;;;;
;; JS
;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.jstt$" . js-mode))

;;;;;;;;;;;;
;; CSS/SCSS
;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scsstt$" . scss-mode))

;;;;;;;;
;; MD
;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;
;; JAVA
;;;;;;;;;;
(add-hook 'java-mode-hook 'java-mode-hook)

;; Compile the current Java buffer
(defun javac-current()
  (interactive)
  (compile (concat "javac " buffer-file-name)))

;; Define Java specific hook
(defun java-mode-hook ()
  (local-set-key [C-f7] 'javac-current))

;;;;;;;;;;
;; C / C++
;;;;;;;;;;
;; Enable c mode hook
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
