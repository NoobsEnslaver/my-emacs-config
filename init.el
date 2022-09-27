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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-basic-offset 4)
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes '(default))
 '(display-time-mode t)
 '(erlang-new-clause-with-arguments t)
 '(fill-column 120)
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(package-selected-packages
   '(jenkins projectile-ripgrep counsel flycheck-ocaml magit-p4 utop tuareg company-web merlin-company company-erlang merlin psc-ide psci eldoc-box exunit elixir-mode mix eglot macrostep-geiser geiser-guile company racket-mode htmlize js2-mode vlf zerodark-theme web-mode expand-region geiser projectile slime smartparens erlang magit rust-mode cargo haskell-mode lsp-mode flymake lsp-ui company-mode lsp-treemacs lsp-ivy dap-mode))
 '(projectile-tags-backend 'etags-select)
 '(projectile-tags-command "find -name *.[he]rl -print | etags -")
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

;------Smartparens---------------------------------
;; doc: https://github.com/Fuco1/smartparens
(require 'smartparens)
(smartparens-global-mode 1)             ;Скобки ставятся парами во всех режимах

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
(cond ((file-exists-p "/usr/lib/erlang/erts-12.2.1/")
       (setq erlang-root-dir "/usr/lib/erlang/erts-12.2.1/"))
      ;; ((file-exists-p (expand-file-name "~/.guix-profile/lib/erlang/erts-11.0.3/"))
      ;;  (setq erlang-root-dir (expand-file-name "~/.guix-profile/lib/erlang/erts-11.0.3/")))
      )

(maybe-add-to-load-path "/usr/lib/erlang/lib/tools-3.5.2/emacs/")
(maybe-add-to-exec-path "/usr/lib/erlang/bin")
;; (maybe-add-to-load-path "~/.guix-profile/lib/erlang/lib/tools-3.3/emacs/")
;; (maybe-add-to-exec-path "~/.guix-profile/lib/erlang/bin/")

(require 'erlang-start)
;(add-hook 'erlang-mode-hook 'company-mode)
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
;(add-hook 'haskell-mode-hook 'company-mode)
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
;(add-hook 'haskell-mode-hook 'lsp) ;; require https://github.com/haskell/haskell-language-server


;; Common Lisp
;(setq inferior-lisp-program "sbcl")
;(require 'slime)
;(slime-setup)

;; Company
;; doc: http://company-mode.github.io/
(require 'company)
(require 'company-erlang)
(require 'merlin-company)
(add-hook 'after-init-hook 'global-company-mode)

;; Counsel (Ivy)
;; https://oremacs.com/swiper/
(ivy-mode 1)
;(global-set-key (kbd "C-s") 'swiper-isearch)
;(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)

;; Projectile
;; https://github.com/sharkdp/fd
;; https://github.com/BurntSushi/ripgrep
;; sudo apt install fd-find ripgrep
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-sort-order 'recentf)
(setq revert-without-query '("TAGS"))
(setq projectile-completion-system 'ivy)
;(add-hook 'projectile-idle-timer-hook #'foo)  ; run this hook every 30 sec (by default there TAGS rebuild)
(setq projectile-project-search-path '(("~/Projects/" . 1)))

;; FIXME: complementary with scheme-mode or not? If yes - add to hook. What about indention?
;; What about skribe-major-mode? Skribe or skribilo?
;; (autoload 'skribe-mode "skribe.el" "Skribe mode." t)
;; (add-to-list 'auto-mode-alist '("\\.skr\\'" . skribe-mode))
;; (add-to-list 'auto-mode-alist '("\\.skb\\'" . skribe-mode))
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

;; --------------- OCaml -----------------
;; https://github.com/ocaml/merlin
;; https://github.com/ocaml/tuareg
;(setq merlin-command "<PATH>/ocamlmerlin") ; needed only if ocamlmerlin not already in your PATH
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(setq utop-command "opam config exec -- utop -emacs") ;; Use the opam installed utop

;; flycheck
;; (with-eval-after-load 'merlin
;;      (setq merlin-error-after-save nil) ;; Disable Merlin's own error checking
;;      (flycheck-ocaml-setup)) ;; Enable Flycheck checker

(add-to-list 'load-path "~/.emacs.d/extra/")
