;;; custom.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur


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

;; 分享快捷键
(require-package 'keycast)

;; 使用clj-kondo来做clj/cljc/cljs的语法检查, 需要安装clj-kondo
(require-package 'flycheck-clj-kondo)


;; 开启换页线
(global-page-break-lines-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (page-break-lines-mode)))
(add-hook 'clojurescript-mode-hook
          (lambda ()
            (page-break-lines-mode)))

;; 80个字符处放置竖线
(setq-default fill-column 120)

;; 区域选择
(require-package 'expand-region)
;; 按 <C-c => 扩大选中区域，按 <C-c -> 缩小选中区域
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;执行命令 `toggle-company-english-helper’ , 就可以在Emacs中飞速的编写英文文档了
;;安装参考：https://manateelazycat.github.io/emacs/2018/08/08/company-english-helper.html
(require 'company-english-helper)

;;代码区块注释
(global-set-key (kbd "C-c /") 'comment-region)
;; 设置magit的log默认显示age为datetime
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))


;;; ------------------org-mode setting beginning

;; org文件生成reveal.js PPT
(require-package 'ox-reveal)
(load-library "ox-reveal")

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
   (sql . t)
   ))

;; 打开 org-indent mode
(setq org-startup-indented t)

;; 设置默认不要展开，全部折叠
(setq org-startup-folded t)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

;; 设置 bullet list
(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
;;(setq org-ellipsis "⤵")

;;;设置jar包路径
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/jar/plantuml.1.2021.9.jar"))
(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/jar/ditaa0_9.jar"))

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
      (list "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org"
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

;;java支持：https://segmentfault.com/a/1190000040158765
(require-package 'lsp-java)

(require 'ob-sql)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-blue))
 '(package-selected-packages
   '(lsp-java yasnippet org-bullets ox-reveal expand-region flycheck-clj-kondo keycast undo-tree cnfonts envrc uptimes shfmt dotenv-mode osx-location htmlize lua-mode gnuplot sudo-edit flycheck-ledger ledger-mode dash-at-point origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company slime cask-mode flycheck-relint cl-libify flycheck-package highlight-quoted macrostep aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit nginx-mode company-nixos-options nixos-options nix-buffer nix-sandbox nixpkgs-fmt nix-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode flycheck-rust racer rust-mode flycheck-nim nim-mode j-mode dune-format dune merlin-eldoc merlin-company merlin tuareg sqlformat projectile-rails yard-mode bundler yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode dante haskell-mode reformatter toml-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink grab-mac-link company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint coffee-mode xref-js2 prettier-js typescript-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode crontab-mode alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs browse-at-remote whitespace-cleanup-mode which-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy browse-kill-ring symbol-overlay rainbow-delimiters beacon mode-line-bell vlf list-unicode-display unfill mmm-mode session windswap switch-window company-quickhelp company marginalia consult-flycheck embark-consult projectile consult embark orderless vertico flycheck-color-mode-line flycheck ibuffer-vc wgrep-ag ag wgrep anzu diff-hl diredfl disable-mouse default-text-scale ns-auto-titlebar dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq ob-sql-mode))
 '(session-use-package t nil (session)))

;; 启动时打开指定文件
(find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org")
;; End:
;;; custom.el ends here
