;;;;;;;;;;;;;
;; YASnippet
;;;;;;;;;;;;;
;; Enable yasnippets everywhere
(yas-global-mode t)

;; Enable dropdown menu when there are multiple snippet options
(setq yas-prompt-functions '(yas-dropdown-prompt yas-no-prompt))

;; Bind trigger to be Ctrl + Tab so that it doesn't conflict with other plugins
;; that use the Tab key (e.g., Auto-Complete).
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas/expand)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;

;; Change projectile prefix from \C-c p -> \C-x p. Note this must be set before
;; enabling projectile mode globally.
(setq projectile-keymap-prefix "\C-xp")

;; Enable Projectile everywhere.
(projectile-global-mode t)

;; Use grizzle instead of ido-mode for autocompletion.
(setq projectile-completion-system 'grizzl)

;; Always enable caching when indexing project files.
(setq projectile-enable-caching t)

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

;;;;;;;;;;;;;;;;
;; Cycle buffer
;;;;;;;;;;;;;;;;
(load-file (concat vendor-dir "cycle-buffer.el"))
(require 'cycle-buffer)

;;;;;;;;;;;;;;;;
;; Dired single
;;;;;;;;;;;;;;;;
(load-file (concat vendor-dir "dired-single.el"))
(require 'dired-single)
