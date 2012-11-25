(add-to-list 'load-path (concat vendor-dir "auto-complete/"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/dict"))
(require 'auto-complete-config)
(setq ac-use-comphist nil)
(ac-config-default)
(setq ac-auto-start nil)

;; Appearance
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;; Emacs has two tab keys: "TAB" and "<tab>". YASnippet tries to bind to the
;; <tab> key. We must manually bind this trigger key to <tab> and make sure this
;; gets loaded after YASnippet so that this binding takes precedence.
(ac-set-trigger-key "<tab>")
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
