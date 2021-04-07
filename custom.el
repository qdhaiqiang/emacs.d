;;; custom.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (yasnippet org-bullets ox-reveal keycast undo-tree cnfonts uptimes dotenv-mode daemons osx-location dsvn htmlize lua-mode gnuplot flycheck-ledger ledger-mode dash-at-point origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode cl-libify flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit-everywhere paredit nginx-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode sqlformat projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide reformatter purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode hindent intero haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink grab-mac-link company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint xref-js2 prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode cmd-to-echo alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode guide-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay rainbow-delimiters goto-line-preview beacon mode-line-bell vlf list-unicode-display unfill mmm-mode switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy smex flycheck-color-mode-line flycheck ibuffer-vc wgrep-ag ag wgrep anzu diff-hl diredfl disable-mouse default-text-scale ns-auto-titlebar dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell fullframe seq ox-pandoc flycheck-clj-kondo pdf-tools try grip-mode flycheck-pos-tip org-tree-slide calfw uimage 0blayout org-download clojure-snippets markdown-preview-mode clj-refactor neotree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 中文对齐
(require-package 'cnfonts)
(cnfonts-enable)

;; 撤回undotree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; 开启换页线
(global-page-break-lines-mode)

;; 分享快捷键
(require-package 'keycast)

(add-hook 'clojure-mode-hook
          (lambda ()
            (page-break-lines-mode)))
(add-hook 'clojurescript-mode-hook
          (lambda ()
            (page-break-lines-mode)))

;; 80个字符处放置竖线
(setq-default fill-column 80)

;; org文件生成reveal.js PPT
(require-package 'ox-reveal)
(load-library "ox-reveal")


;;code check tools
;; 使用clj-kondo来做clj/cljc/cljs的语法检查, 需要安装clj-kondo,`brew install borkdude/brew/clj-kondo`
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

;;; org-mode setting beginning

;; 使用org-bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;开启自动折行,防止一行文字的长度超出屏幕范围时，行会继续往右延伸而导致部分内容不可见(因在屏幕范围外而无法看见)
(setq truncate-lines nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (dot . t)
   (python . t)
   (clojure . t)
   ))

;; 打开 org-indent mode
(setq org-startup-indented t)

;; 设置 bullet list
(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
;;(setq org-ellipsis "⤵")

;;;设置jar包路径
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/scripts/plantuml.jar"))
(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/scripts/ditaa0_9.jar"))

;;;生成图像时不予提示
(setq org-confirm-babel-evaluate nil)

;;;预览图像
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; Make babel results blocks lowercase
;;(setq org-babel-results-keyword "results")

;;;agenda 模式配置
(setq org-agenda-start-on-weekday 1) ;我喜欢一周以周一做开始
(setq org-agenda-files (list "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org"
                             "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/learn.org"
                             "~/git/redcreation/rcpubdoc/早会/record.org"
                             ))

(setq org-agenda-text-search-extra-files
      (list "~/org/someday.org"
            "~/org/config.org"
            ))

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)

(setq org-todo-keywords '((sequence "TODO" "DOING" "DONE" "BLOCK")))
;;;通过修改 org-todo-keyword-faces 这个变量可以达到这个目的。
;;;例如我们希望 "TODO" 以红色显示，"DOING" 以黄色显示，"DONE" 用绿色显示
(setq org-todo-keyword-faces '(("TODO" . "red")
                               ("DOING" . "yellow")
                               ("DONE" . "green")
                               ("BLOCK" . (:foreground "blue" :weight bold))))

;;;;插动图片到org 文件时， 自动将文件放到org下的imgs/下，并插入[[file:…imgs/image.jpg]]
(defun vmacs-org-insert-image (event)
  (interactive "e")
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fromname (cadr payload))
         (img-regexp "\\(gif\\|png\\|jp[e]?g\\)\\>")
         (destname fromname)
         img-dir
         )
    (when (file-exists-p "../imgs/")
      (setq img-dir "../imgs/"))
    (when (file-exists-p "./imgs/")
      (setq img-dir "./imgs/"))
    (when (and  (eq 'drag-n-drop (car event))
                (eq 'file type)
                (string-match img-regexp fromname)
                img-dir)
      (let ((filebasename (file-name-base (buffer-file-name)) ))
        (setq destname (concat img-dir filebasename "-" (format-time-string "%Y-%m-%d-%H-%M-%S") "." (file-name-extension fromname)))
        (rename-file fromname destname t))
      (goto-char (nth 1 (event-start event)))
      (insert (format "[[file:%s]]" (file-relative-name destname (file-name-directory (buffer-file-name))))))))

;;;; org-mode setting end



(require-package 'yasnippet)
(yas-global-mode 1)

;; End:
;;; custom.el ends here
