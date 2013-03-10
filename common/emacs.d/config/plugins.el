;;;;;;;;;;;;
;; Autopair
;;;;;;;;;;;;
(add-to-list 'load-path (concat vendor-dir "autopair/"))
(require 'autopair)

;; Enable autopair mode everywhere
(autopair-global-mode t)

;; Disable blinking on matching parens
(setq autopair-blink nil)

;; Enable triple-quoting in Python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;;;;;;;;;;;;;
;; YASnippet
;;;;;;;;;;;;;

(add-to-list 'load-path (concat vendor-dir "yasnippet/"))
(require 'yasnippet)

;; Enable yasnippets everywhere
(yas-global-mode 1)

;; Enable dropdown menu when there are multiple snippet options
(setq yas-prompt-functions '(yas-dropdown-prompt yas-no-prompt))

;; Bind trigger to be Ctrl + Tab so that it doesn't conflict with other plugins
;; that use the Tab key (e.g., Auto-Complete).
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas/expand)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat vendor-dir "auto-complete/"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/lib/popup"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/lib/fuzzy"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/lib/ert"))
(add-to-list 'load-path (concat vendor-dir "auto-complete/dict"))
(require 'auto-complete-config)

(setq ac-use-comphist nil)
(ac-config-default)
(setq ac-auto-start nil)

;; Appearance
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;; Keybindings
(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;;;;;;;
;; Redo
;;;;;;;;
(load-file (concat vendor-dir "redo.el"))
(require 'redo)

;; For gui mode
(global-set-key (kbd "C-M-/") 'redo)

;; For terminal mode
(global-set-key (kbd "C-M-_") 'redo)