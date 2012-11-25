(add-to-list 'load-path (concat vendor-dir "yasnippet/"))
(require 'yasnippet)

;; Enable yasnippets everywhere
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-dropdown-prompt yas-no-prompt))
