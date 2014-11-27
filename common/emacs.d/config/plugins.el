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

;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(ac-config-default)

;; The latest versions of auto-complete stumbles on yasnippet. This is
;; supposedly fixed in the latest version of auto-complete, but it still breaks
;; for us. Until this is fixed for real, we should just prevent yasnippet from
;; appearing in auto-complete. (11/27/2014)
;; Source: http://www.kurup.org/blog/2012/10/15/emacs-autocomplete-stumbles-on-yasnippet/
(setq ac-source-yasnippet nil)

;; Set some colors.
(set-face-background 'ac-candidate-face "#141414")
(set-face-foreground 'ac-candidate-face "#f6f3e8")
(set-face-background 'ac-selection-face "#64a8d8")

;; By default, auto-complete analyzes completion operations and ranks candidates
;; higher in the search results if they've been selected multiple times. This
;; usually just gets in the way, so we disable it here.
(setq ac-use-comphist nil)

;; Remove delay before auto-complete shows completions.
(setq ac-delay 0.0)

(ac-set-trigger-key "TAB")
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;

;; Change projectile prefix from \C-c p -> \C-x p. Note this must be set before
;; enabling projectile mode globally.
(setq projectile-keymap-prefix "\C-xp")

;; Enable Projectile everywhere.
(projectile-global-mode t)

;; Always enable caching when indexing project files.
(setq projectile-enable-caching t)

;; Use helm instead of ido-mode for autocompletion.
(setq projectile-completion-system 'helm)

;; Allow switching between js, scss, and html files more easily.
(add-to-list 'projectile-other-file-alist '("js" "scss" "html" "htmlmk"))
(add-to-list 'projectile-other-file-alist '("scss" "js" "html" "htmlmk"))
(add-to-list 'projectile-other-file-alist '("htmlmk" "scss" "js"))
(add-to-list 'projectile-other-file-alist '("html" "scss" "js"))

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
