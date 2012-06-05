(add-to-list 'load-path "~/.emacs.d/vendor/autopair")
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
