;; Для установки всех пакетов, раскомментируйте следующие строки,
;; переведите на них курсор и нажмите C-M-x и перезапустите emacs
;; (package-refresh-contents)
;; (package-install-selected-packages)


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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-basic-offset 4)
 '(custom-enabled-themes '(tsdh-dark))
 '(custom-safe-themes
   '("e8830baf7d8757f15d9d02f9f91e0a9c4732f63c3f7f16439cc4fb42a1f2aa06" "d88049c628f3a8a92f9e46982d3e891867e4991de2b3a714f29f9f5eb91638c1" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" default))
 '(display-time-mode t)
 '(erlang-new-clause-with-arguments t)
 '(fill-column 120)
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(package-selected-packages
   '(dhall-mode psc-ide psci eldoc-box exunit elixir-mode mix eglot macrostep-geiser geiser-guile flx-ido company racket-mode org-jira htmlize smex ac-js2 js2-mode vlf zerodark-theme web-mode expand-region geiser projectile projectile-codesearch slime smartparens erlang auto-complete magit rust-mode cargo haskell-mode lsp-mode flymake lsp-ui company-mode lsp-treemacs helm-lsp lsp-ivy dap-mode))
 '(safe-local-variable-values '((org-image-actual-width quote true)))
 '(sgml-basic-offset 4)
 '(standard-indent 4)
 '(tab-always-indent nil)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray20" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "PT Mono")))))

;=======Shortcuts==============================
(global-set-key (kbd "C-.") 'undo)
(global-set-key (kbd "<XF86Calculator>") 'calculator)
(global-set-key (kbd "<f12>") (lambda() (interactive)
                                (find-file user-init-file)))

;------Web-mode---------------------------------
;; doc: http://web-mode.org/
;(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

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
;(ac-config-default) ; ..it is rare to change a source setting because it is already optimized to use.
(setq ac-ignore-case nil)               ;default: 'smart

;------Magit---------------------------------
;; doc: https://magit.vc/manual/magit.html
(global-set-key (kbd "C-c m d") 'magit-dispatch)
(global-set-key (kbd "C-c m s") 'magit-status)

;----------LSP-----------------

(require 'lsp-mode)
;; (setq lsp-keymap-prefix "C-c l")

;; (setq lsp-ui-doc-enable nil)
;; (use-package lsp-mode
;;              :commands lsp
;;              :ensure t
;;              :diminish lsp-mode
;;              :hook (elixir-mode . lsp)
;;              :init (add-to-list 'exec-path "~/Projects/elixir-ls/release/language_server.sh"))

;----------Elixir---------------
(require 'eglot)
(require 'exunit)
(require 'eldoc-box)

(add-to-list 'eglot-server-programs
             '(elixir-mode "~/Projects/elixir-ls/release/language_server.sh"))
(add-hook
 'elixir-mode-hook
 (lambda ()
   (subword-mode)
   (eglot-ensure)
   (company-mode)
   (flymake-mode)
   (add-hook 'before-save-hook 'eglot-format nil t)))

;------Erlang--------------------------------
;; doc: http://erlang.org/doc/apps/tools/erlang_mode_chapter.html
(cond ((file-exists-p "/usr/lib/erlang/erts-11.0.3/")
       (setq erlang-root-dir "/usr/lib/erlang/erts-11.0.3/"))
      ((file-exists-p (expand-file-name "~/.guix-profile/lib/erlang/erts-11.0.3/"))
       (setq erlang-root-dir (expand-file-name "~/.guix-profile/lib/erlang/erts-11.0.3/"))))

(maybe-add-to-load-path "/usr/lib/erlang/lib/tools-3.4/emacs/")
(maybe-add-to-load-path "~/.guix-profile/lib/erlang/lib/tools-3.3/emacs/")
(maybe-add-to-exec-path "/usr/lib/erlang/bin")
(maybe-add-to-exec-path "~/.guix-profile/lib/erlang/bin/")

(require 'erlang-start)
(add-hook 'erlang-mode-hook 'auto-complete-mode)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))    ;использовать пробелы вместо табуляций
;; (add-hook 'erlang-mode-hook 'lsp) ;; require install https://github.com/erlang-ls/erlang_ls

;-----ORG-Jira-------------------------------
;; doc: https://github.com/ahungry/org-jira
;; echo 'machine your-site.atlassian.net login you@example.com password yourPassword port 80' > ~/.authinfo
;; (setq jiralib-url "https://jira.ringcentral.com")

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
(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)))

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
(setq make-backup-files nil)
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

(setq geiser-active-implementations '(guile))
;(require 'geiser)                       ;minor scheme mode
(require 'smartparens-config)

;; Rust
;(require 'rust-mode)
;(require 'cargo)
;(add-hook 'rust-mode-hook 'cargo-minor-mode)
;(add-hook 'rust-mode-hook 'auto-complete-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'auto-complete-mode)
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
;(add-hook 'haskell-mode-hook 'lsp) ;; require https://github.com/haskell/haskell-language-server


;; Common Lisp
;(setq inferior-lisp-program "sbcl")
;(require 'slime)
;(slime-setup)

(require 'company)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching nil)
(setq ido-use-faces nil)

;; FIXME: complementary with scheme-mode or not? If yes - add to hook. What about indention?
;; What about skribe-major-mode? Skribe or skribilo?
(autoload 'skribe-mode "skribe.el" "Skribe mode." t)
(add-to-list 'auto-mode-alist '("\\.skr\\'" . skribe-mode))
(add-to-list 'auto-mode-alist '("\\.skb\\'" . skribe-mode))
;; (add-hook 'skribilo-mode-hook #'scheme-mode)

;;Pure Script
;;on start have to C-c C-s for start server
;(require 'psc-ide)

;;(setq psc-ide-use-npm-bin t)            ;?
;; (add-hook 'purescript-mode-hook
;;   (lambda ()
;;     (psc-ide-mode)
;;     (company-mode)
;;     (flycheck-mode)
;;     (turn-on-purescript-indentation)))
