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

(require 'dir-treeview)
(global-set-key (kbd "<f9>") 'dir-treeview)
(load-theme 'dir-treeview-pleasant t)

;; 默认启动后最大化
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;按f11让Emacs进入全屏显示
;;参考： http://www.emacswiki.org/cgi-bin/wiki/FullScreen
(defun fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;; 默认启动进入全屏
(fullscreen)

;;菜单我需要显示，工具栏和滚动条不用显示
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; 映射全角字符到半角字符
(let (
      ($replacePairs
       [
        ["，" ","]
        ["。" "."]
        ["；" ";"]
        ["：" ":"]
        ["【" "["]
        ["】" "]"]
        ["（" "("]
        ["）" ")"]
        ["！" "!"]
        ["、" "\\"]
        ["／" "/"]
        ["《" "<"]
        ["》" ">"]
        ["‘" "'"]
        ["’" "'"]
        ["“" "\""]
        ["”" "\""]
        ]
       ))
  (mapcar (lambda(x) (define-key key-translation-map
                       (kbd (elt x 0)) (kbd (elt x 1)))) $replacePairs))


;; 80个字符处放置竖线,对应函数:global-display-fill-column-indicator-mode
(setq-default fill-column 120)

;; 区域选择
(require-package 'expand-region)
;; 按 <C-c => 扩大选中区域，按 <C-c -> 缩小选中区域
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;执行命令 `toggle-company-english-helper’ , 就可以在Emacs中飞速的编写英文文档了
;;安装参考：https://manateelazycat.github.io/emacs/2018/08/08/company-english-helper.html
;;(require 'compan-yenglish-helper)

;;代码区块注释
(global-set-key (kbd "C-c /") 'comment-region)
;; 设置magit的log默认显示age为datetime
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))


;;保存时删除尾部空格
(add-hook 'before-save-hook
          'delete-trailing-whitespace)


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
 '((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (shell . t)
   (python . t)
   (haskell . t)
   (dot . t)
   ;;(latex . t)
   (js . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (clojure . t)
   (sql . t)
   (java . t)
   ))

;; 打开 org-indent mode
(setq org-startup-indented t)

;; 设置默认不要展开，全部折叠
(setq org-startup-folded t)

;; 设置 bullet list,可以用UTF-8 Miscellaneous Symbols自己定义
(setq org-bullets-bullet-list '("☀" "☳" "☯" "☰" "★" "☷" "☭" "⚙" "✤"))
;;(setq org-ellipsis "⤵")

;;;设置jar包路径
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/jar/plantuml-1.2022.7.jar"))
(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/jar/ditaa0_9.jar"))

;;;code执行免应答 Y/n
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

(setq org-todo-keywords
      '(
        (sequence "TODO(t!)" "DOING(i)" "DONE(d!)" "SOMEDAY(s)" "CANCELED(c @/!)" "BLOCK(c @/!)")
        (sequence "BUG(b!)" "|" "FIXED(f!)")
        ))
;;(setq org-todo-keywords '((sequence "TODO" "DOING" "DONE" "CANCELLED" "BLOCK")))
;;;通过修改 org-todo-keyword-faces 这个变量可以达到这个目的。
;;;例如我们希望 "TODO" 以红色显示，"DOING" 以黄色显示，"DONE" 用绿色显示
(setq org-todo-keyword-faces '(("TODO" . "red")
                               ("DOING" . "yellow")
                               ("DONE" . "green")
                               ("BUG" . "red")
                               ("FIXED" . "green")
                               ("CANCELLED" . "red")
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


;; -- Display images in org mode
;; enable image mode first
(iimage-mode)
;; add the org file link format to the iimage mode regex
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))
;;  add a hook so we can display images on load
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))
;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

(setq org-image-actual-width '(800))

;;;; org-mode setting end


;; 启动时打开指定文件
(find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org")

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
 '(custom-enabled-themes
   '(smart-mode-line-dark sanityinc-tomorrow-blue dir-treeview-pleasant))
 '(package-selected-packages
   '(swiper lsp-java yasnippet org-bullets ox-reveal expand-region flycheck-clj-kondo keycast undo-tree cnfonts envrc uptimes shfmt dotenv-mode osx-location htmlize lua-mode gnuplot sudo-edit flycheck-ledger ledger-mode dash-at-point origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company slime cask-mode flycheck-relint cl-libify flycheck-package highlight-quoted macrostep aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit nginx-mode company-nixos-options nixos-options nix-buffer nix-sandbox nixpkgs-fmt nix-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode flycheck-rust racer rust-mode flycheck-nim nim-mode j-mode dune-format dune merlin-eldoc merlin-company merlin tuareg sqlformat projectile-rails yard-mode bundler yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode dante haskell-mode reformatter toml-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink grab-mac-link company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint coffee-mode xref-js2 prettier-js typescript-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode crontab-mode alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs browse-at-remote whitespace-cleanup-mode which-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy browse-kill-ring symbol-overlay rainbow-delimiters beacon mode-line-bell vlf list-unicode-display unfill mmm-mode windswap switch-window company-quickhelp company marginalia consult-flycheck embark-consult projectile consult embark orderless vertico flycheck-color-mode-line flycheck ibuffer-vc wgrep-ag ag wgrep anzu diff-hl diredfl disable-mouse default-text-scale ns-auto-titlebar dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq pdf-view-restore gnuplot-mode)))

;; 启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode t)

;;删除快捷键
(define-key org-mode-map (kbd "M-c") nil)

;;下载远程图片到 Org 文件
(defun my/org-download-image (link)
(interactive "sUrl: ")
(setq filename
      (concat
       (make-temp-name
        (concat (file-name-directory (buffer-file-name))
                "imgs/" ;; 相对与当前 org 文件的目录，例如如果 org 位于~, 则把文件放到~/imgs/xx.png
                (format-time-string "%Y%m%d_%H%M%S_")) ) ".png")) ;; 根据时间戳生成文件名
(shell-command-to-string (format "wget %s -O %s" link filename)) ;; 通过 wget 命令下载图片
(message "download image success")
(setq relative-dir (concat "./imgs/" (file-name-nondirectory filename)))
(if (file-exists-p filename)
    (insert (concat "#+ATTR_HTML: :width 70%\n[[file:" relative-dir "]]"))) ;; 将图插入到 org 文件中
)

;;(global-set-key (kbd "C-s") 'swiper-isearch)
;;(require-package 'swiper)
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)
         ("C-c C-r" . ivy-resume)
         ;;("M-x" . counsel-M-x)
         ;;("C-x C-f" . counsel-find-file)
         )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;;自动补全配置
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t)
  )

;;漂亮的间隔线
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme 1
        sml/theme 'automatic)
  (sml/setup))

;;gnuplot绘图工具
(use-package gnuplot-mode)

;; 设置agenda 显示中文 月、周、日
;; https://emacs-china.org/t/agenda/7711/4
(setq-default
 ;;inhibit-startup-screen t;隐藏启动显示画面
 ;; calendar-date-style 'iso
 calendar-day-abbrev-array ["周七" "周一" "周二" "周三" "周四" "周五" "周六"]
 calendar-day-name-array ["周七" "周一" "周二" "周三" "周四" "周五" "周六"]
 calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-week-start-day 1 ;;设置一周从周一开始
 org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前: "))
 org-agenda-scheduled-leaders (quote ("要事:" "%2d次☞"))
 ;; ------时间戳汉化------
 system-time-locale "zh_CN.UTF-8" ;; "C":英文格式
 org-time-stamp-formats  '("<%Y-%m-%d 周%a>" . "<%Y-%m-%d 周%a %H:%M>")
 org-display-custom-times t
 org-time-stamp-custom-formats '("<%Y-%m-%d 周%a>" . "<%Y-%m-%d 周%a %H:%M>")
 org-deadline-warning-days 2 ;;最后期限到达前5天即给出警告
 )


;;设置系统编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 设置垃圾回收阈值,加速启动
(setq gc-cons-threshold most-positive-fixnum)

;; 使用xelatex，配合当前org文件最开始的配置来正常输出中文
;; 这类笔记基本不可能是全英文，所以就安心用xelatex算了
(setq org-latex-pdf-process '("xelatex -file-line-error -interaction nonstopmode %f"
                              "bibtex %b"
                              "xelatex -file-line-error -interaction nonstopmode %f"
                              "xelatex -file-line-error -interaction nonstopmode %f"))

;; 生成PDF后清理辅助文件
;; https://answer-id.com/53623039
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "tex" "aux"
              "idx" "log" "out" "toc" "nav"
              "snm" "vrb" "dvi" "fdb_latexmk"
              "blg" "brf" "fls" "entoc" "ps"
              "spl" "bbl" "xdv")))

;; 图片默认宽度
(setq org-image-actual-width '(300))

(setq org-export-with-sub-superscripts nil)

;; 不要自动创建备份文件
(setq make-backup-files nil)

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
;; elegantpaper.cls
;; https://github.com/ElegantLaTeX/ElegantPaper/blob/master/elegantpaper.cls
(with-eval-after-load 'ox-latex
  ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
  ;; automatically to resolve the cross-references.
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted")))

;; End:
;;; custom.el ends here
