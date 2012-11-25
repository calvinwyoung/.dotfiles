(add-to-list 'load-path (concat vendor-dir "yasnippet/"))
(require 'yasnippet)
(require 'popup)

;; Enable yasnippets everywhere
(yas-global-mode 1)

(setq yas/trigger-key (kbd "<C-tab>"))

;; Use auto-complete's popup menu for choosing snippets
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
