;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))


;;marvin-start
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


;;漂亮的间隔线
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
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


;;设置系统编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
;;(fullscreen)

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
(setq-default fill-column 120)

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
(require 'flycheck-clj-kondo)

(setq org-startup-indented t)

;;使用 Emacs 发送电子邮件和检查日历
;;https://linux.cn/article-11932-1-rel.html

;; Or start grip when opening a markdown/org buffer
;;(add-hook 'markdown-mode-hook #'grip-mode)


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


;; 启动时打开指定文件
(find-file "~/git/mine/org-doc/log.org")

;;;代码

;;java支持：https://segmentfault.com/a/1190000040158765
(require-package 'lsp-java)
(require 'ob-sql)

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
;;
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/Downloads/foo")
;; dired模式默认递归删除目录
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(global-set-key (kbd "<f12>") 'org-agenda)

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
            "/Users/mahaiqiang/git/redcreation/xh-live-training/hc-meeting/doc.org"
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


;;自动换行,在28之后用:(setq word-wrap-by-category )
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))


;;;; org-mode setting end


;;marvin-end















(provide 'init-locales)
;;; init-locales.el ends here
