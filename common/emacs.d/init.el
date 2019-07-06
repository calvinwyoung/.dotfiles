;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode rjsx-mode js2-mode flycheck yasnippet yaml-mode wgrep seq org-autolist multiple-cursors lua-mode let-alist helm-projectile go-mode expand-region exec-path-from-shell deft auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
