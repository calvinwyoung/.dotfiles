(load-file (concat vendor-dir "redo.el"))
(require 'redo)

;; For gui mode
(global-set-key (kbd "C-M-/") 'redo)

;; For terminal mode
(global-set-key (kbd "C-M-_") 'redo)
