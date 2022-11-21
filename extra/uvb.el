
(defvar ci-base "https://ci.esafeline.com")
;; (defvar ci-projects )
;;  = config.get_ci_projects()
;; download_path = config.archives_path()


;; (require 'url)
;; (url-http (url-generic-parse-url "https://ci.esafeline.com/") (lambda () nil) '())


(defun uvb-list-packages (&optional no-fetch)
  "Display a list of packages.
This first fetches the updated list of packages before
displaying, unless a prefix argument NO-FETCH is specified.
The list is displayed in a buffer named `*Packages*', and
includes the package's version, availability status, and a
short description."
  (interactive "P")
  ;; (require 'finder-inf nil t)
  ;; Initialize the package system if necessary.
  ;; (unless package--initialized
  ;;   (package-initialize t))
  ;; Integrate the package-menu with updating the archives.
  (add-hook 'uvb-package--post-download-archives-hook
            #'uvb-package-menu--post-refresh)
  (add-hook 'uvb-package--post-download-archives-hook
            #'uvb-package-menu--mark-or-notify-upgrades 'append)

  ;; Generate the Package Menu.
  (let ((buf (get-buffer-create "*Packages*")))
    (with-current-buffer buf
      ;; Since some packages have their descriptions include non-ASCII
      ;; characters...
      (setq buffer-file-coding-system 'utf-8)
      (package-menu-mode)

      ;; Fetch the remote list of packages.
      (unless no-fetch (package-menu--refresh-contents))

      ;; If we're not async, this would be redundant.
      (when package-menu-async
        (package-menu--generate nil t)))
    ;; The package menu buffer has keybindings.  If the user types
    ;; `M-x list-packages', that suggests it should become current.
    (pop-to-buffer-same-window buf)))
