;; ------------------------------------------------------------------------------
;; (setq projectile-project-search-path '(("~/Projects/" . 1)))
;; (push '("~/Projects/ug/branch-6.0.3/utm/" . 1) projectile-project-search-path)
;; (push '("~/Projects/ug/ugos-7.0.1/ugos/package/utm-core/" . 1) projectile-project-search-path)
;; (push '("~/Projects/ug/ugos-7.0.2/ugos/package/utm-core/" . 1) projectile-project-search-path)
;; (push '("~/Projects/ug/ugos-7.1.0/ugos/package/utm-core/" . 1) projectile-project-search-path)

(defgroup ug nil
  "UserGate working utils."
  :group 'tools)

(defcustom ug-project-root-dir "~/Projects/ug/"
  "Ugos project root path."
  :group 'ug
  :type 'string)

(defcustom ug-remote-host-default "ugos-utm-1"
  "Remote host address."
  :group 'ug
  :type 'string)

(defvar ug-remote-hosts
  (list ug-remote-host-default "ugos-utm-2" "ugos-utm-3" "utm-node-1" "utm-node-2" "ugos-mc-1"
        ;; "utm-node-10" "utm-node-11" "utm-node-12"
        ))

(defun drop-after (what where &optional shift)
  (let* ((shift (if shift shift 0))
         (end (+ (length what) (cl-search what where) shift)))
    (cl-subseq where 0 end)))

(defun current-ug-ver ()
  (let ((cwd (projectile-acquire-root)))
    (cond
     ((cl-search "branch-6.0.3" cwd) 6)
     ((cl-search "ugos-7." cwd) 7))))

(defun current-project-root ()
  (let ((cwd (projectile-acquire-root)))
    (cond
     ((cl-search "branch-6.0.3" cwd) (drop-after "branch-6.0.3/" cwd))
     ((cl-search "ugos-7." cwd) (drop-after "ugos-7." cwd 4))
     (t cwd))))

(defun current-project-rel-path (rel-path)
  (cl-concatenate 'string (current-project-root) rel-path))

(defun abs-path (rel-path)
  (cl-concatenate 'string ug-project-root-dir rel-path))

(defun ug-open-ngfw-remote (h)
  (interactive (list (projectile-completing-read "P" ug-remote-hosts)))
  (dired (format "/ssh:root@%s:/root/build/corenew/core" h)))

(defun ug-rebuild-tags ()
  "Build TAGS from all erl+hrl files in current project."
  (interactive)
  (let ((root (cl-case (current-ug-ver)
                (6 (current-project-rel-path "utm/"))
                (7 (current-project-rel-path "ugos/package/")))))
    (shell-command (format "cd %s; fd -e hrl -e erl | etags -" root))))

(defun ug-open-core ()
  "Open utm-core project"
  (interactive)
  (projectile-switch-project-by-name (abs-path "ugos-7.0.2/ugos/package/utm-core/corenew/core/") 'nil))

(defun ug-open-appliance ()
  "Open utm-appliance project"
  (interactive)
  (projectile-switch-project-by-name (abs-path "ugos-7.0.2/ugos/package/utm-core/appliance/") 'nil))

(defun ug-open-shared-core ()
  "Open shared-core project"
  (interactive)
  (projectile-switch-project-by-name (abs-path "ugos-7.0.2/ugos/package/utm-core/shared-core") 'nil))


(defun ug-remote-sync (h)
  "Sync core into remote node"
  (interactive (list (projectile-completing-read "Sync to: " ug-remote-hosts)))
  (let ((cwd (projectile-acquire-root)))
    (cond
     ((cl-search "branch-6.0.3" cwd) (ug-remote-sync-6 h "branch-6.0.3/"))
     ((cl-search "ugos-7.0.1" cwd)   (ug-remote-sync-7 h "ugos-7.0.1/ugos/package/utm-core/"))
     ((cl-search "ugos-7.0.2" cwd)   (ug-remote-sync-7 h "ugos-7.0.2/ugos/package/utm-core/"))
     ((cl-search "ugos-7.1.0" cwd)   (ug-remote-sync-7 h "ugos-7.1.0/ugos/package/utm-core/")))))

(defun ug-remote-sync-6 (h root)
  (async-shell-command
   (format
    "cd %s;\
     rsync --chmod +w -avR --exclude='*.beam' l7 utm/core utm/tools utm/sysstat utm/core-binary utm/shared-core utm/webconsole_proxy utm/ldap_sync utm/logan_client utm/license utm/namedlists utm/flog utm/utmdata %s:builds/master" (abs-path root) h)))

(defun ug-remote-sync-7 (h root)
  (shell-command
   (format
    "cd %s;\
     scp -O workspace-files/Makefile.runtime.ngfw root@%s:Makefile;\
     rsync --chmod +w -avzR --exclude '*.beam' --exclude '*.so' corenew/ sharedlibs/ root@%s:/root/build/" (abs-path root) h h)))


(cl-defun ug-download-beams-6 (h &optional (root "/tmp/ug-6-beams/"))
  "Скачивает .beam файлы c dev-инстанса в указанную папку"
  (interactive (list (projectile-completing-read "Sync to: " ug-remote-hosts)))
  (shell-command
   (format
    "mkdir -p %s; cd %s;\
     rsync -avzR %s:builds/master/utm/core/_build/dev/rel/utmcore/lib/*/ebin/ ." root root h))
  (expand-file-name "builds/master/utm/core/_build/dev/rel/utmcore/lib/" root))

(cl-defun ug-upload-beams-6 (h &optional (lib-root "/tmp/ug-6-beams/builds/master/utm/core/_build/dev/rel/utmcore/"))
  "Подливает .beam файлы на удлалённый, не девелоперский инстанс."
  (interactive (list (projectile-completing-read "Sync to: " ug-remote-hosts)))
  (shell-command
   (format
    "cd %s;\
     rsync -avzR . %s:/opt/entensys/utmcore/" lib-root h)))

;; (defun ug-attach-ngfw ()
;;   (interactive)
;;   (call-interactively (shell-command "sh -c ssh utm-ugos-1")))

(global-set-key (kbd "C-c u o c") #'ug-open-core)
(global-set-key (kbd "C-c u o a") #'ug-open-appliance)
(global-set-key (kbd "C-c u o s c") #'ug-open-shared-core)
(global-set-key (kbd "C-c u s") #'ug-remote-sync)
(global-set-key (kbd "C-c u t") #'ug-rebuild-tags)

(provide 'ug)
