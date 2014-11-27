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
(require 'auto-complete-config)
(ac-config-default)

;; Set some colors.
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;; By default, auto-complete analyzes completion operations and ranks candidates
;; higher in the search results if they've been selected multiple times. This
;; usually just gets in the way, so we disable it here.
(setq ac-use-comphist nil)

;; By default, the auto-complete dropdown will open each time new characters are
;; inserted. Here we disable this behavior, and require hitting the trigger key
;; (i.e., tab) to activate the dropdown.
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;;;;;;;
;; Redo
;;;;;;;;
(require 'redo)

;;;;;;;;;;;;;;;;
;; Cycle buffer
;;;;;;;;;;;;;;;;
(require 'cycle-buffer)

;;;;;;;;;;;;;;;;
;; Dired single
;;;;;;;;;;;;;;;;
(require 'dired-single)
