;; Для установки всех пакетов, раскомментируйте следующие строки,
;; переведите на них курсор и нажмите C-M-x и перезапустите emacs
;; (package-refresh-contents)
;; (package-install-selected-packages)

;; Для переопределения любой из переменных для конкретного файлы (например, именно в этом проекте используют особое значение отступа)
;; откройте файл и введите add-file-local-variable-prop-line

;---- internal functions ---------------------------
(require 'cl-lib)

(defun maybe-add-to-list (path list)
  (let ((absolute-path (expand-file-name path)))
    (when (file-exists-p absolute-path)
      (add-to-list list absolute-path))))

(defun maybe-add-to-load-path (path)
  (maybe-add-to-list path 'load-path))

(defun maybe-add-to-exec-path (path)
  (maybe-add-to-list path 'exec-path))

;; ---------- Init -----------------------------------
(require 'package)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-enabled-themes '(modus-vivendi))
 '(default-input-method "russian-computer")
 '(erlang-new-clause-with-arguments t)
 '(fill-column 120)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(go-errcheck go-scratch go-tag gotest go-mode go-projectile go-snippets sly yasnippet jsonrpc project erlang flycheck projectile-ripgrep counsel eldoc-box company htmlize js2-mode vlf zerodark-theme web-mode expand-region projectile smartparens magit haskell-mode flymake eglot))
 '(projectile-project-search-path '(("~/Projects/" . 1)))
 '(ring-bell-function 'ignore)
 '(sgml-basic-offset 4)
 '(standard-indent 4)
 '(tab-always-indent nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))

;; Tags:
;; +  if ggtags:
;; require: sudo apt install global
;; (require 'ggtags)
;; +  if builtin emacs ctags/etags:
;; '(projectile-tags-backend 'etags-select)
;; '(projectile-tags-command "fd -tf -e hrl -e erl -e jl -c never | etags -") ;; "find -name *.[he]rl -print | etags -"
;; +  if universal-ctags: (works)
;; sudo apt install universal-ctags
;; sudo update-alternatives --config ctags
;; sudo update-alternatives --config etags
;; and rename/remove old ctags/etags in ~/.local/bin (or where emacs)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anonymous Pro" :foundry "mlss" :slant normal :weight regular :height 151 :width normal)))))

;; ----------- Tree sitter ---------------
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; M-x treesit-install-language-grammar julia
;; (treesit-language-available-p 'julia) -> t
;; TODO: replace all modes to -ts versions, auto install ts trammars
;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq treesit-language-source-alist
   '((bash      "https://github.com/tree-sitter/tree-sitter-bash")
     (html      "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (json      "https://github.com/tree-sitter/tree-sitter-json")
     (python    "https://github.com/tree-sitter/tree-sitter-python")
     (toml      "https://github.com/tree-sitter/tree-sitter-toml")
     (haskell   "https://github.com/tree-sitter/tree-sitter-haskell")
     (julia     "https://github.com/tree-sitter/tree-sitter-julia")
     (rust      "https://github.com/tree-sitter/tree-sitter-rust")
     (go        "https://github.com/tree-sitter/tree-sitter-go")
     (c         "https://github.com/tree-sitter/tree-sitter-c")
     (cpp       "https://github.com/tree-sitter/tree-sitter-cpp")
     (yaml      "https://github.com/ikatyang/tree-sitter-yaml")))

;======= Some extra config and utils ================
;(add-to-list 'load-path "~/.emacs.d/extra/")

;=======Shortcuts==============================
(global-set-key (kbd "C-.") 'undo)
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

;------Smartparens---------------------------------
;; doc: https://github.com/Fuco1/smartparens
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)             ;Скобки ставятся парами во всех режимах

;------Magit---------------------------------
;; https://magit.vc/manual/magit.html
(global-set-key (kbd "C-c m d") 'magit-dispatch)
(global-set-key (kbd "C-c m s") 'magit-status)

;------Erlang--------------------------------
;; doc: http://erlang.org/doc/apps/tools/erlang_mode_chapter.html
;; (cond ((file-exists-p "~/.local/erlang-27.0.1/erts-15.0.1")
;;        (setq erlang-root-dir "~/.local/erlang-27.0.1/erts-15.0.1")))
(cond ((file-exists-p "/usr/lib/erlang/erts-14.2.5.2")
       (setq erlang-root-dir "/usr/lib/erlang/erts-14.2.5.2")))

(maybe-add-to-load-path "/usr/lib/erlang/lib/tools-3.6/emacs")
(maybe-add-to-exec-path "/usr/lib/erlang/bin")

(require 'erlang-start)
;(add-hook 'erlang-mode-hook 'company-mode)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))    ;использовать пробелы вместо табуляций

;------Java Script---------------------------
;; doc: https://github.com/mooz/js2-mode/
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq js2-basic-offset 4)

;; -------- View Large Files mode -------------------------
;; doc: https://github.com/m00natic/vlfi
(require 'vlf-setup)                    ;ask to use mode when opening large file

;; -------- Org Mode --------------------------------------
;; (require 'org)
;; (require 'ox-latex)
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)

;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq org-src-fontify-natively t)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    (latex . t)))

;; (add-hook 'org-mode-hook 'auto-fill-mode)

;========== Variables =====================================
(global-display-line-numbers-mode 1)

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


;; Common Lisp
;; (setq inferior-lisp-program "sbcl")
;; (require 'sly-autoloads)

;; company
;; doc: http://company-mode.github.io/
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Haskell
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;; Counsel (Ivy)
;; https://oremacs.com/swiper/
(ivy-mode 1)
;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)

;; Projectile
;; https://github.com/sharkdp/fd
;; https://github.com/BurntSushi/ripgrep
;; sudo apt install fd-find ripgrep
(projectile-mode +1)
;; (projectile-discover-projects-in-search-path)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-sort-order 'recentf)
(setq revert-without-query '("TAGS"))
(setq projectile-completion-system 'ivy)
;(add-hook 'projectile-idle-timer-hook #'foo)  ; run this hook every 30 sec (by default there TAGS rebuild)

(yas-global-mode)
(maybe-add-to-exec-path "~/.local/bin)")

;; ---------------- GO ----------------------------------->>
;; go install github.com/kisielk/errcheck@latest
;; go install golang.org/x/tools/cmd/guru@latest
;; go install golang.org/x/tools/cmd/gorename@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/gopls@latest
;; go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

;; ??
;; go install github.com/rogpeppe/godef@latest
;; go install github.com/jstemmer/gotags@latest

(maybe-add-to-exec-path "~/.local/go/bin")
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


;; <<------------ End GO ---------------------------------<<

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
