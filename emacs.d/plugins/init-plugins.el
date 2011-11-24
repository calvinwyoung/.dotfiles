;;;;;;;;
;; Redo
;;;;;;;;
(require 'redo)
(global-set-key (kbd "C-M-/") 'redo)    ; for window-system
(global-set-key (kbd "C-M-_") 'redo)    ; for termminal

;;;;;;;;;;;;;;;;
;; Autocomplete
;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/autocomplete/ac-dict")
(setq ac-use-comphist nil)
(ac-config-default)

;; Appearance
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;; Keys
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;;;;;;;;;;;;
;; SCSS Mode
;;;;;;;;;;;;;
(autoload 'scss-mode "scss-mode"
   "Major mode for editing SCSS files" t)
(setq scss-compile-at-save nil)

;;;;;;;;;;;;;;;;;
;; Markdown Mode
;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
