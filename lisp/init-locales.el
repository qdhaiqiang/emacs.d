;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))


;;marvin-start
;;允许下载远程文件
(setq org-allow-http-server-prompt t)

(require-package 'use-package)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;use-package的默认配置
(eval-and-compile
  (setq use-package-always-ensure t) ;;不用每个包都手动添加:ensure t关键字
  (setq use-package-always-defer t) ;; 默认都是延迟加载,不用每个包都手动添加:defer t
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

;; 设置光标为竖条
(setq-default cursor-type 'bar)

;;高亮当前行，当文本内容很多时可以很容易找到光标的位置。
(global-hl-line-mode 1)

;; 启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode t)

;;全局开启undotree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

;;删除快捷键
;;(define-key org-mode-map (kbd "M-c") nil)

;;菜单我需要显示，工具栏和滚动条不用显示
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;tree-sitter
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (sh-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (css-mode . css-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (python-mode . python-ts-mode)))

;;放大缩小的快捷键
(global-set-key (kbd "M-=") 'cnfonts-increase-fontsize)
(global-set-key (kbd "M--") 'cnfonts-decrease-fontsize)
;; 禁用 M-滚轮 方法/缩小页面
(global-unset-key [C-wheel-up])
(global-unset-key [C-wheel-down])
(global-unset-key [C-M-wheel-up])
(global-unset-key [C-M-wheel-down])

;;在当前目录打开iterm
(define-key global-map (kbd "C-0") 'iterm-here)

(defun iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))

;;漂亮的间隔线
(use-package smart-mode-line
  :init
  ;; (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'automatic)
  (sml/setup))

;; 分享快捷键
(require-package 'keycast)

;; 设置agenda 显示中文 月、周、日
;; https://emacs-china.org/t/agenda/7711/4
(setq-default
 ;;inhibit-startup-screen t;隐藏启动显示画面
 ;; calendar-date-style 'iso
 calendar-day-abbrev-array ["日" "一" "二" "三" "四" "五" "六"]
 calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"]
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


;;所有地方都使用utf8编码，并显式地设置它，而不依赖于Emacs默认值
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; 设置垃圾回收阈值,加速启动
(setq gc-cons-threshold most-positive-fixnum)

;; 不要自动创建备份文件
(setq make-backup-files nil)

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

;; 中文对齐
(require-package 'cnfonts)
(cnfonts-mode 1)

;;代码片段
(require-package 'yasnippet)
(yas-global-mode 1)

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
(setq-default fill-column 100)

;;自动换行,在28之后用:(setq word-wrap-by-category )
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; 区域选择,按 <C-c => 扩大选中区域，按 <C-c -> 缩小选中区域
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;执行命令 `toggle-company-english-helper’ , 就可以在Emacs中飞速的编写英文文档了
;;安装参考：https://manateelazycat.github.io/emacs/2018/08/08/company-english-helper.html
;;(require 'compan-yenglish-helper)


;;(require 'auto-save)            ;; 加载自动保存模块
;;(auto-save-enable)              ;; 开启自动保存功能
;;(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我
;;(tool-bar-mode 0)               ;;禁止工具栏
;;(global-linum-mode t)           ;;侧边显示行号
(column-number-mode t)            ;;状态栏显示行列信息
(show-paren-mode t)               ;;括号匹配高亮
;; (global-hl-line-mode 1)        ;;当前行高亮
;;代码折叠：https://blog.csdn.net/pfanaya/article/details/6939310


(setq markdown-command "/usr/local/bin/pandoc")

;;静态代码检查：https://github.com/borkdude/flycheck-clj-kondo
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

;; then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))


;;(setq org-startup-indented t)

;;使用 Emacs 发送电子邮件和检查日历
;;https://linux.cn/article-11932-1-rel.html

;; Or start grip when opening a markdown/org buffer
(use-package grip-mode
  :ensure t
  :config (setq grip-use-grip t))


(set-fontset-font "fontset-default" 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)

;;ivy三剑客
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))

;;treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))
;; treeemacs end

;; 启动时打开指定文件
(find-file "~/git/mine/org-doc/log.org")

;;;代码

;;java支持：https://segmentfault.com/a/1190000040158765
(require-package 'lsp-java)
(require 'ob-sql)
(use-package ob-rust)

;;删除当前行
(use-package crux
  :bind ("C-c k" . crux-smart-kill-line))

;;代码块或者代码行的上下移动
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;;代码区块注释
(global-set-key (kbd "C-c /") 'comment-region)

;; 设置magit的log默认显示age为datetime
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))


;;保存时删除尾部空格
(add-hook 'before-save-hook
          'delete-trailing-whitespace)


;;; ------------------org-mode setting beginning
(use-package org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq-default org-download-image-dir "~/Downloads/")

(global-set-key (kbd "<f12>") 'org-agenda)

;; dired模式默认递归删除目录
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; org文件生成reveal.js PPT
(require-package 'ox-reveal)
(load-library "ox-reveal")

(setq org-hide-leading-stars t) ;;隐藏标题栏里的一堆星号


;; 使用org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; 设置 bullet list,可以用UTF-8 Miscellaneous Symbols自己定义
(setq org-bullets-bullet-list '("☯"  "☷" "◉" "☰" "✤" "☳" "➤" "☀" "★" "⚙" "☭"))
(setq org-hide-emphasis-markers t)   ;;直接显示语法样式
(setq org-ellipsis " ▼" )   ;; 折叠时不在显示[...],换个你喜欢的符号

;;开启自动折行,防止一行文字的长度超出屏幕范围时，行会继续往右延伸而导致部分内容不可见
(setq truncate-lines nil)

;;更改行间距
(setq line-spacing 0.25)

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
   (latex . t)
   (js . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (clojure . t)
   (sql . t)
   (java . t)
   ))

;;org导出pdf或者html时下划线不用转义
(setq org-export-with-sub-superscripts nil)


;;用[ <s ]这样的定义快速开始代码块
(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

;; 打开 org-indent mode
(setq org-startup-indented t)

;; 设置默认不要展开，全部折叠
(setq org-startup-folded t)

;;;设置jar包路径
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/jar/plantuml-1.2022.7.jar"))
(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/jar/ditaa0_9.jar"))

;;;code执行免应答 Y/n
(setq org-confirm-babel-evaluate nil)

;;;预览图像

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; 让代码生成的results关键字小写-
;;(setq org-babel-results-keyword "results")


;;;agenda 模式配置
(setq org-agenda-start-on-weekday 1) ;我喜欢一周以周一做开始
(setq org-agenda-files (list "/Users/mahaiqiang/git/mine/org-doc/log.org"
                             "/Users/mahaiqiang/git/mine/org-doc/learn.org"
                             ))

(setq org-agenda-text-search-extra-files
      (list "/Users/mahaiqiang/git/mine/org-doc/log.org"
            "/Users/mahaiqiang/git/mine/org-doc/learn.org"
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
                               ("DOING" . "origin")
                               ("DONE" . "forest green")
                               ("BUG" . "origin")
                               ("FIXED" . "green")
                               ("CANCELLED" . "red")
                               ("BLOCK" . (:foreground "blue" :weight bold))))

;;;;插动图片到org 文件时， 自动将文件放到org下的imgs/下，并插入[[file:…imgs/imgs.jpg]]
(defun emacs-org-insert-image (event)
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

;;org 文件中设置图片显示尺寸
;;(setq org-image-actual-width '(400))
;;让图片显示的大小固定为屏幕宽度的三分之一
;;(setq org-image-actual-width (/ (display-pixel-width) 4))

(setq org-table-export-default-format "orgtbl-to-csv")

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

;; ORG 导出 pdf配置
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

;; 生成PDF后清理辅助文件
;; https://answer-id.com/53623039
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "tex" "aux"
              "idx" "log" "out" "toc" "nav"
              "snm" "vrb" "dvi" "fdb_latexmk"
              "blg" "brf" "fls" "entoc" "ps"
              "spl" "bbl" "xdv")))


;;;; org-mode setting end

(use-package gptel)

;; OPTIONAL configuration


;; company-mode：代码补全框架
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; 弹层延迟显示时长
  ;; (company-begin-commands nil) ;; 取消注释可以禁用弹层
  :bind
  (:map compnay-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))

;; Rust
(use-package rustic
  :ensure
  :bind (:map rustic-mod-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-wordspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; 减少闪动可以取消这里的注释
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; 注释下面这行可以禁用保存时 rustfmt 格式化
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))
(defun rk/rustic-mode-hook ()
  ;; 所以运行 C-c C-c C-r 无需确认就可以工作，但不要尝试保存不是文件访问的 rust 缓存。
  ;; 一旦 https://github.com/brotzeit/rustic/issues/253 问题处理了
  ;; 就不需要这个配置了
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; cargo.el：Cargo 工具集成
(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode)        ; 在 rust-mode 中启用 cargo
  :config
  (setq cargo-process--command-clippy "clippy")) ; 使用 clippy 进行检查

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; 保存时使用什么进行检查，默认是 "check"，我更推荐 "clippy"
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))



(setenv "GPTEL_TOGETHER_KEY" "1d5ed6159c537302ceb9ef056d2b4daafd42c557bd954c2b4248e17a4c7a8f68")
(setenv "GPTEL_GEMINI_KEY" "AIzaSyDncDcmn5jLRzWD0qGfX51RWX3y5qM3Uek")
(setenv "GPTEL_QWEN_KEY" "sk-766f6f492b264f369bf312de82f5afd7")
(setenv "GPTEL_DEEPSEEK_KEY" "sk-082b867923074ed2b0f420a7ec6af2c4")

(gptel-make-gemini "Gemini" :key (lambda () (getenv "GPTEL_GEMINI_KEY")) :stream t)

(gptel-make-openai "Groq"
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GPTEL_GROQ_KEY"))
  :models '(llama-3.1-70b-versatile
            llama-3.1-8b-instant
            llama3-70b-8192
            llama3-8b-8192
            mixtral-8x7b-32768
            gemma-7b-it))

(gptel-make-openai "Qwen"
  :host "dashscope.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GPTEL_QWEN_KEY"))
  :models '(qwen-max qwen-plus deepseek-v3 deepseek-r1 qwen-turbo deepseek-r1-distill-llama-70b))

(gptel-make-openai "mistral"
  :host "api.mistral.ai"
  :endpoint "/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GPTEL_MISTRAL_KEY"))
  :models '(mistral-large-latest))

(gptel-make-openai "Together"
  :host "api.together.xyz"
  :endpoint "/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GPTEL_TOGETHER_KEY"))
  :models '(deepseek-ai/DeepSeek-V3))

(gptel-make-openai "siliconflow"
  :host "api.siliconflow.cn"
  :endpoint "/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GPTEL_SILICONFLOW_KEY"))
  :models '(deepseek-ai/DeepSeek-V3 deepseek-ai/DeepSeek-R1))

(setq gptel-model 'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key (lambda () (getenv "GPTEL_DEEPSEEK_KEY"))
        :models '(deepseek-chat deepseek-coder)))

;;扩展

(defun my/gptel-mode-auto ()
  "确保此文件打开时启用 `gptel-mode'. "
  (save-excursion
    (let ((enable-local-variables t))   ; 确保我们可以修改局部变量
      (if (and (save-excursion
                 (goto-char (point-min))
                 (looking-at ".*-\\*-"))) ; 如果存在 -*- 行
          ;; 首先删除任何现有的 eval, 然后添加新的
          (modify-file-local-variable-prop-line
           'eval nil 'delete))
      ;; 始终添加我们的 eval
      (add-file-local-variable-prop-line
       'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

(add-hook 'gptel-save-state-hook #'my/gptel-mode-auto)

(defun gptel-send-with-options (&optional arg)
  "发送查询. 带前缀 ARG 时打开 gptel 菜单. "
  (interactive "P")
  (if arg
      (call-interactively 'gptel-menu)
    (gptel--suffix-send (transient-args 'gptel-menu))))




(defun get-staged-diff ()
  "Get the diff of staged files in the current Git repository."
  (string-trim
   (shell-command-to-string "git diff --cached")))

(defun gptel-commit-message ()
  "Insert a generated commit message at point using GPT."
  (interactive)
  (let ((diff (get-staged-diff)))
    (gptel-request
        (concat "请生成commit message:" diff)
      :stream nil)))

(defun gptel-commit-message ()
  "Insert a generated commit message at point using GPT."
  (interactive)
  (let ((diff (get-staged-diff)))
    (message "正在生成提交信息...")
    (gptel-request
        (concat "请直接生成中文的commit message, 要求如下
    1. 使用中文
    2. 使用英文半角标点, 比如`:\",`等
    3. 提交信息简洁明了
    4. 提交标题必须以下前缀之一开头：
       - [feat] 用于新功能
       - [bug] 用于修复bug
       - [ref] 用于代码重构
       - [doc] 用于文档更新
       - [wip] 用于进行中的工作
     5. 有列表详细描述改动的具体内容
    " diff)
      :stream t
      )))


;;marvin-end

(provide 'init-locales)
;;; init-locales.el ends here
