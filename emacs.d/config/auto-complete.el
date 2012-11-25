(add-to-list 'load-path (concat vendor-dir "auto-complete/"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/dict"))
(require 'auto-complete-config)
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
