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
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)

;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;

;; Change projectile prefix from Ctrl-c p -> Ctrl-x p. Note this must be set before
;; enabling projectile mode globally.
(setq projectile-keymap-prefix (kbd "C-x p"))

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

;; By default, projectile-replace operates on the entire project root unless a
;; prefix argument is specified, in which case it will prompt for a
;; directory. Since we prefer specifying a directory, we reverse the default so
;; we only operate on the project root when a prefix is used.
(define-key projectile-command-map (kbd "r") (lambda(&optional arg)
                                               (interactive "P")
                                               (projectile-replace (not arg))))

;;;;;;;;
;; Helm
;;;;;;;;

;; Enable helm mode everywhere.
(helm-mode)

;; Prevent helm-find-files from showing files matching regexps in
;; `helm-boring-file-regexp-list'.
(setq helm-ff-skip-boring-files t)

;; Rebind tab to run persistent action (e.g., complete directory name in
;; find-file). First binding is for GUI mode, and the second is for terminals.
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; Remape Ctrl + tab to list select actions.
(define-key helm-map (kbd "C-<tab>") 'helm-select-action)

;; Helm hijakcs the Ctrl + W command -- we should rebind it to what we expect.
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-k") 'kill-visual-line)

;; Show helm's keyring instead of blindly cycling through the keyring.
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Replace the normal find-file command with helm's pimped-out version.
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;;;;;;;
;; Redo
;;;;;;;;
(require 'redo)

;; First binding is for GUI mode, and the second is for terminals.
(global-set-key (kbd "C-M-/") 'redo)
(global-set-key (kbd "C-M-_") 'redo)

;;;;;;;;;;;;;;;;
;; Cycle buffer
;;;;;;;;;;;;;;;;
(require 'cycle-buffer)

;; Easier buffer switching. The naming here is kind of confusing -- the
;; `cycle-buffer` command walks DOWN the stack (i.e., calling it will show the
;; most recently used buffer). `cycle-buffer-backward` goes in the opposite
;; direction.
(global-set-key (kbd "C-M-h") 'cycle-buffer)
(global-set-key (kbd "C-M-l") 'cycle-buffer-backward)

;;;;;;;;;;;;;;;;
;; Dired single
;;;;;;;;;;;;;;;;
(require 'dired-single)

;; Override the default dired binding to open the "magic buffer" in the current
;; file's directory. This prevents dired from creating a new buffer each time a
;; new directory is visited.
(global-set-key (kbd "C-x d") (lambda()
                                (interactive)
                                (dired-single-magic-buffer default-directory)))

;; Custom key map for reusing dired buffers with `dired-single`.
(defun my-dired-keys-map ()
  "Custom key mappings to allow reusing single buffer in dired "
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
  (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "^") (lambda()
                                         (interactive)
                                         (dired-single-buffer ".."))))

;; If dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; We're good to go; just add our bindings.
    (my-dired-keys-map)
  ;; It's not loaded yet, so add our bindings to the load-hook.
  (add-hook 'dired-load-hook 'my-dired-keys-map))
