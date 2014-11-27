(require 'cl)

;; List of packages we need to install.
(defvar required-packages
  '(
    auto-complete
    ;; For better auto-completion with projectile.
    grizzl
    js2-mode
    lua-mode
    projectile
    yaml-mode
    yasnippet))

;; List the repositories we want to install from.
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Helper function for checking whether all required packages are installed.
;; Source: http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
(defun check-packages-installed-p ()
  "Returns true if any of the packages in `required-packages' aren't installed."
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If any of the required packages aren't installed, then refresh the package
;; contents and install everything.
(unless (check-packages-installed-p)
  ;; Check for new packages (package versions).
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")

  ;; Install the missing packages
  (dolist (p required-packages)
    (unless (package-installed-p p)
      (package-install p))))
