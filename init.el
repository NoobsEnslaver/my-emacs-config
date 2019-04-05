;; Для установки всех пакетов, раскомментируйте следующие строки,
;; переведите на них курсор и нажмите C-M-x и перезапустите emacs
;; (package-refresh-contents)
;; (package-install-selected-packages)

;; Для переопределения любой из переменных для конкретного файлы (например, именно в этом проекте используют особое значение отступа)
;; откройте файл и введите add-file-local-variable-prop-line

;---- internal functions ---------------------------
(require 'cl)

(defun maybe-add-to-list (path list)
  (let ((absolute-path (expand-file-name path)))
    (when (file-exists-p absolute-path)
      (add-to-list list absolute-path))))

(defun maybe-add-to-load-path (path)
  (maybe-add-to-list path 'load-path))

(defun maybe-add-to-exec-path (path)
  (maybe-add-to-list path 'exec-path))

;; ---------- Init -----------------------------------
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-basic-offset 8)
 '(custom-enabled-themes (quote (zerodark)))
 '(custom-safe-themes
   (quote
    ("e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "83b1fda71a1cf78a596891c0cc10601e93d5450148f98e9b66dde80349b20195" "edea0b35681cb05d1cffe47f7eae912aa8a930fa330f8c4aeb032118a5d0aabf" default)))
 '(display-time-mode t)
 '(erlang-new-clause-with-arguments t)
 '(fill-column 120)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (racket-mode org-jira htmlize smex ac-js2 js2-mode vlf zerodark-theme web-mode expand-region geiser projectile projectile-codesearch slime smartparens erlang auto-complete auto-complete-distel magit)))
 '(safe-local-variable-values (quote ((org-image-actual-width quote true))))
 '(sgml-basic-offset 8)
 '(show-paren-mode t)
 '(standard-indent 8)
 '(tab-always-indent nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal)))))

;=======Shortcuts==============================
(global-set-key (kbd "<XF86Calculator>") 'calculator)
(global-set-key (kbd "<f12>") (lambda() (interactive)
                                (find-file user-init-file)))

;------Web-mode---------------------------------
;; doc: http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

;------Expand-region---------------------------------
;; doc: https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "M-@") 'er/expand-region)  ;более умное выделение блоками
(global-set-key (kbd "M-#") 'er/contract-region);на одно выделение назад
(global-set-key (kbd "M-x") 'smex)              ;ido-mode для M-x

;------Smartparens---------------------------------
;; doc: https://github.com/Fuco1/smartparens
(require 'smartparens)
(smartparens-global-mode 1)             ;Скобки ставятся парами во всех режимах

;------auto-complete---------------------------------
;; doc: https://github.com/auto-complete/auto-complete
(require 'auto-complete)
(ac-config-default)

;------Magit---------------------------------
;; doc: https://magit.vc/manual/magit.html
(global-set-key (kbd "C-c m d p") 'magit-dispatch-popup)
(global-set-key (kbd "C-c m s") 'magit-status)

;------Erlang--------------------------------
;; doc: http://erlang.org/doc/apps/tools/erlang_mode_chapter.html
(cond ((file-exists-p "/usr/lib/erlang/erts-9.3")
       (setq erlang-root-dir "/usr/lib/erlang/erts-9.3"))
      ((file-exists-p (expand-file-name "~/.guix-profile/lib/erlang/erts-10.0.5/"))
       (setq erlang-root-dir (expand-file-name "~/.guix-profile/lib/erlang/erts-10.0.5/"))))

(maybe-add-to-load-path "/usr/lib/erlang/lib/tools-2.11.2/emacs/")
(maybe-add-to-load-path "~/.guix-profile/lib/erlang/lib/tools-3.0.1/emacs/")
(maybe-add-to-exec-path "/usr/lib/erlang/bin")
(maybe-add-to-exec-path "~/.guix-profile/lib/erlang/bin/")

(require 'erlang-start)
(add-hook 'erlang-mode-hook 'auto-complete-mode)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))    ;использовать пробелы вместо табуляций

;------Distel--------------------------------
;; doc: https://github.com/massemanet/distel
(maybe-add-to-load-path "/usr/share/distel/elisp")
(maybe-add-to-load-path "/usr/local/share/distel/elisp")
(maybe-add-to-load-path "~/.local/share/distel/elisp")

;; (when (not (require 'distel nil t))
;;   (message "Cloning and installing distel")
;;   (shell-command "git clone https://github.com/massemanet/distel.git distel-src --depth 1 && cd distel-src && make && make install prefix=~/.local && cd ../ && rm -rf distel-src")
;;   (maybe-add-to-load-path "~/.local/share/distel/elisp"))

;; (require 'distel)
;; (distel-setup)

;-----ORG-Jira-------------------------------
;; doc: https://github.com/ahungry/org-jira
;; echo 'machine your-site.atlassian.net login you@example.com password yourPassword port 80' > ~/.authinfo
(setq jiralib-url "https://jira.ringcentral.com")

;------Java Script---------------------------
;; doc: https://github.com/mooz/js2-mode/
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode)) ;web-mode also supports
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-basic-offset 4)

;; -------- View Large Files mode -------------------------
;; doc: https://github.com/m00natic/vlfi
(require 'vlf-setup)                    ;ask to use mode when opening large file

;; -------- Org Mode --------------------------------------
(add-hook 'org-mode-hook 'auto-fill-mode)

;========== Variables =====================================
(global-linum-mode 1)

(setq display-time-24hr-format t)       ;24-часовой временной формат в mode-line
(show-paren-mode 2)                     ;Выделять пару для скобки
(setq show-paren-style 'mixed)          ;Подсвечивать скобку если видна, иначе всё выражение
(setq use-dialog-box     nil)           ;никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)           ;лучшая отрисовка буфера
(setq ring-bell-function nil)           ;отключить звуковой сигнал

;Отключаем файлы автосохранения
(setq make-backup-file nil)
(setq auto-save-default nil)

; Delete selection
(delete-selection-mode t)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Delete trailing whitespaces and untabify when save buffer
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(require 'geiser)
(require 'smartparens-config)
