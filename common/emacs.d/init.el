(setq expanded-user-emacs-directory (expand-file-name user-emacs-directory))

(defvar config-dir (concat expanded-user-emacs-directory "config/")
  "Custom emacs config ")
(defvar vendor-dir (concat expanded-user-emacs-directory "vendor/")
  "Vendor-provided Emacs modules")

(add-to-list 'load-path config-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'custom-theme-load-path vendor-dir)

;; Load core configuration
(load-library "packages")
(load-library "settings")
(load-library "defuns")
(load-library "lang-modes")
(load-library "keybindings")
(load-library "plugins")
