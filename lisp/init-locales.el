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

;; 中文对齐
(require-package 'cnfonts)
(cnfonts-mode 1)

(require-package 'undo-tree)
(global-undo-tree-mode)

;; 分享快捷键
(require-package 'keycast)

;; 使用clj-kondo来做clj/cljc/cljs的语法检查, 需要安装clj-kondo
(require-package 'flycheck-clj-kondo)

(require 'flycheck-clj-kondo)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))


;; 开启换页线
(global-page-break-lines-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (page-break-lines-mode)))
(add-hook 'clojurescript-mode-hook
          (lambda ()
            (page-break-lines-mode)))

;; 80个字符处放置竖线
(setq-default fill-column 80)

;; 使用cua做矩形区域编辑
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(global-set-key
 (kbd "<C-return>")
 'cua-set-rectangle-mark)

;; org文件生成reveal.js PPT
(require-package 'ox-reveal)
(load-library "ox-reveal")

;; 使用org-bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))

;; 需要expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; 映射全角字符到半角字符
(let (
      ($replacePairs
       [
        ["·" "`"]
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
        ["¥" "$"]
        ["‘" "'"]
        ["’" "'"]
        ["“" "\""]
        ["”" "\""]
        ]
       ))
  (mapcar (lambda(x) (define-key key-translation-map
                       (kbd (elt x 0)) (kbd (elt x 1)))) $replacePairs))



(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (C . t)
   (awk . t)
   (dot . t)
   (plantuml . t)
   (clojure . t)
   (latex . t)
   (python . t)
   (csharp . t)
   (java . t)
   (perl . t)
   (js . t)
   (shell . t)
   (sql . t)
   (org . t)
   (ditaa . t)
   (emacs-lisp . t)
   (lisp . t) ;; slime - lisp interaction mode
   (gnuplot . t)))

(setq org-plantuml-jar-path
     (expand-file-name "~/.emacs.d/jar/plantuml.jar"))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/jar/ditaa0_9.jar"))


(setq org-confirm-babel-evaluate t)

(require 'org-tempo)

(setq org-structure-template-alist
      '(("a" . "export ascii\n")
        ("c" . "center\n")
        ("C" . "comment\n")
        ("e" . "example\n")
        ("E" . "export")
        ("h" . "export html\n")
        ("l" . "export latex\n")
        ("q" . "quote\n")
        ("s" . "src\n")
        ("sdt" . "src ditaa :file \n")
        ("sjs" . "src js :results output :exports both\n")
        ("sel" . "src elisp\n")
        ("scl" . "src clojure\n")
        ("ssh" . "src shell :results pp :exports both\n")
        ("scsx" . "src csharp :results pp :exports both\n")
        ("v" . "verse\n")))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))


(setq org-latex-create-formula-image-program 'imagemagick)


;; 使用pandoc把org文件转为md, 需要安装pandoc
(require-package 'ox-pandoc)
(load-library "ox-pandoc")

(require-package 'smex)
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay


;; counsel & ivy
(require-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

;; eval ditaa block in org file
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ;don't ask for ditaa
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


(defun try-convert (out)
  (shell-command-on-region
   (region-beginning) (region-end)
   (format "convert.clj %s " out)
   nil "REPLACE" nil t))
(defun convert-to-edn  () (interactive) (try-convert "edn"))
(defun convert-to-json () (interactive) (try-convert "json"))
(defun convert-to-yaml () (interactive) (try-convert "yaml"))


(require-package 'org-present)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(setq magit-log-margin-show-committer-date t)

(require-package 'virtualenvwrapper)
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

;; Set correct Python interpreter
(setq venv-postactivate-hook
      (list (lambda ()
              (setq python-shell-interpreter (concat python-shell-virtualenv-path "bin/python3")))))
(setq venv-postdeactivate-hook
      (list (lambda ()
              (setq python-shell-interpreter "python3"))))


(setq org-babel-clojure-backend 'cider)


;; Region lines and then `M-x osx-say' to make OSX speak.

;; Adjust speak speed
(setq osx-say-speed
      (if (eq system-type 'windows-nt)
          70
        250))

;; Change voice
;; Kathy, Vicki, Victoria, Alex, Bruce, Fred
(setq osx-say-voice
      (if (eq system-type 'windows-nt)
          "2"
        "Samantha"))

(setq osx-say-buffer "*osx say*")

(defun osx-say-stop ()
  (interactive)
  (when (get-buffer osx-say-buffer)
    (kill-buffer osx-say-buffer)))

(setq osx-say-cmd
      (if (eq system-type 'windows-nt)
          "wsay"
        "say"))

(setq osx-say-speed-param
      (if (eq system-type 'windows-nt)
          "-s"
        "-r"))

(defun osx-say (&optional $word $speed)
  "Utilize `say' command that Mac OSX has/ or wsay on windows."
  (interactive)
  (unless (or
           (executable-find "wsay")
           (executable-find "say"))
    (error (message "`say' command not found")))
  (osx-say-stop)
  (cond ($word $word)
        (mark-active
         (setq $word (buffer-substring-no-properties
                      (region-beginning) (region-end))))
        ((setq $word (thing-at-point 'word)))
        (t (setq $word (read-string "word: "))))
  (mapc (lambda ($r)
          (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
        (list ;;'("'"   . "\\\\'")
         '("\""  . "\\\\\"")
         '("?"   . "\\\\?")
         '("\n"  . " ")
         '("\("  . "\\\\(")
         '("\)"  . "\\\\)")
         '("\\[" . "\\\\[")
         '("\\]" . "\\\\]")
         '("\;"  . "\\\\;")
         '("\&"  . "\\\\&")
         '("\|"  . "\\\\|")))
  (save-window-excursion
    (start-process "OSX Say" osx-say-buffer
                   osx-say-cmd "-v" osx-say-voice
                   osx-say-speed-param
                   (number-to-string (or $speed osx-say-speed)) $word)))

(show-paren-mode 1)

(global-company-mode)

;;(add-hook 'cider-repl-mode-hook #'company-mode)
;;(add-hook 'cider-mode-hook #'company-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require-package 'ob-mermaid)
(require-package 'mermaid-mode)


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
(setq org-confirm-babel-evaluate nil)
(setq org-babel-clojure-backend 'cider)

(setq mac-command-key-is-meta t)
(require-package 'eldoc)
(load-library "eldoc")
(require-package 'diff-hl)
(load-library "diff-hl")

(which-key-mode)
;; epub reading
(require-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))

(setq org-image-actual-width nil)

(define-key global-map (kbd "C-0") 'iterm-here)

(defun iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))


;; load java & C#

;; (defun my-csharp-repl ()
;;   "Switch to the CSharpRepl buffer, creating it if necessary."
;;   (interactive)
;;   (if-let ((buf (get-buffer "*csi*")))
;;       (pop-to-buffer buf)
;;     (progn (call-interactively 'csi)
;;            (when-let ((buf (get-buffer "*csi*")))
;;              (switch-to-buffer-other-window buf)))))

;; (defun my-csharp-repl ()
;;   "Switch to the CSharpRepl buffer, creating it if necessary."
;;   (interactive)
;;   (
;;    save-window-excursion
;;    (call-interactively 'csi)
;;    (if-let ((buf (get-buffer "*CSharpRepl*")))
;;        (pop-to-buffer buf)
;;      (when-let ((b (make-comint "CSharpRepl" "csharp")))
;;        (switch-to-buffer-other-window b)))))
;; (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl)

;; (defun my-send (beg end)
;;   (interactive "r")
;;   (process-send-string (get-process "csi")
;;                        (concat (buffer-substring-no-properties beg end) "\n"))
;;   )


(setq system-time-locale "C")

;;(define-key js-mode-map [remap eval-last-sexp] #'js-comint-send-last-sexp)
;;(define-key js-mode-map (kbd "C-c b") 'js-send-buffer)


(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;;csharp-mode README.md recommends this too
  ;;(electric-pair-mode 1)       ;; Emacs 24
  ;;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
  (local-set-key (kbd "C-c C-t") 'omnisharp-unit-test-buffer)
  (local-set-key (kbd "C-c C-p") 'omnisharp-unit-test-at-point)
  (local-set-key (kbd "C-c C-c") 'recompile))

;; (add-hook 'csharp-mode-hook
;;           (lambda ()
;;             (page-break-lines-mode)
;;             (setq-local compile-command
;;                         (concat "dotnet run "))))

(defun find-cs-project-name ()
  (let* ((project-name (file-name-directory (buffer-file-name)))
         (paths (file-name-directory (buffer-file-name)))
         (last-path (string-replace "/" "" (car (last (eshell-split-path paths))))))
    last-path))


(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; (projectile-register-project-type 'dotnet '("Program.cs")
;;                                   :project-file "Program.cs"
;;                                   :compile "dotnet build"
;;                                   :test "dotnet test"
;;                                   :run (concat "dotnet run" (find-cs-project-name))
;;                                   :test-suffix "Tests")

;;marvin begin =================================

;; 启动时打开指定文件
(find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org")


(use-package projectile
  :ensure t
  :bind-keymap ("\C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
    :ensure t))


;;删除当前行
(use-package crux
             :bind ("C-c k" . crux-smart-kill-line))

;;代码块或者代码行的上下移动
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))


;;高亮当前行，当文本内容很多时可以很容易找到光标的位置。
(global-hl-line-mode 1)

;; 启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode t)

;;漂亮的间隔线
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'automatic)
  (sml/setup))

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
(fullscreen)

;;菜单我需要显示，工具栏和滚动条不用显示
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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



;; ➽ ➼ ⬊
;; ⇘ ↘ ⟱ ⤋ ↡ ↧ ⇩ ⬇ ▾  ⇲ ➘ ➴ ᐁ ᐯ ᗐ ￫ ⬥ ᗒ ᐉ ➥ ➻
(setq org-ellipsis " ➽")


;; 设置默认不要展开，全部折叠
(setq org-startup-folded t)

;; 设置 bullet list,可以用UTF-8 Miscellaneous Symbols自己定义
(setq org-bullets-bullet-list '("☀" "☳" "☯" "☰" "★" "☷" "☭" "⚙" "✤"))
;;(setq org-ellipsis "⤵")

;;;code执行免应答 Y/n
(setq org-confirm-babel-evaluate nil)

;;;预览图像
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;;;agenda 模式配置
(setq org-agenda-start-on-weekday 1) ;我喜欢一周以周一做开始
(setq org-agenda-files (list "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org"
                             "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/learn.org"
                             ))

(setq org-agenda-text-search-extra-files
      (list "~/Library/Mobile Documents/com~apple~CloudDocs/org-doc/log.org"
            "/Users/mahaiqiang/git/redcreation/xh-live-training/hc-meeting/doc.org"
            ))


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


;;marvin end ================================================================================

(provide 'init-locales)
;;; init-locales.el ends here
