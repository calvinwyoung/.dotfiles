(require 'cl)

;; List of packages we need to install.
(defvar required-packages
  '(js2-mode
    lua-mode
    projectile
    scss-mode
    yaml-mode))

;; List the repositories we want to install from.
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

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
