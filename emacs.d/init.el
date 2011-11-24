;;;;;;;;;;;;;;;;;;;;
;; LOAD CONFIG FILES
;;;;;;;;;;;;;;;;;;;;

;; Add all config files in my ~/.emacs.d directory
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
    (let ((default-directory (file-name-as-directory dir)))
        (add-to-list 'load-path dir)
            (normal-top-level-add-subdirs-to-load-path)))
            (add-subdirs-to-load-path "~/.emacs.d")

;; Load plugins
(load-library "init-plugins")

;; Load general config settings
(load-library "general-config")

;; Load language-specific-configs
(load-library "lang-config")

;; Load global key bindings
(load-library "keybindings")

;; Set color theme
(load-library "color-theme-wombat")
(color-theme-wombat)
