(cond ((>= emacs-major-version 23)

;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;;;; 種々雑多な設定
;; Official Emacs 用の設定（inline_patch をあててあります）
; (setq default-input-method "MacOSX")
;; 全角記号類「！”＃＄％＆’（）＝〜｜｀『＋＊』＜＞？＿」を入力できるようにする（Mac Emacs では不要）
; (mac-add-key-passed-to-system 'shift)

;; バックアップファイルを作らないようにする

(setq make-backup-files nil)
;; 括弧の対応関係をハイライト表示
(show-paren-mode nil)
;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
; (tool-bar-mode 0)
;; スタートアップ画面を表示しないようにする
;;(setq inhibit-startup-message t)
;; 行間隔を少し広げる
; (set-default 'line-spacing 4)
;; ウィンドウ（フレーム）のサイズ設定する
; (setq default-frame-alist
; 	'((width . 100) (height . 60)))
;; 背景を透過させる
(set-frame-parameter nil 'alpha '(90 70))
;; マウス・スクロールを滑らかにする（Mac Emacs 専用）
(setq mac-mouse-wheel-smooth-scroll t)
;; カーソルの色を設定
; (set-cursor-color "DarkGray")


;; キーの設定（ある程度 Mac 標準に準拠させる）
(setq mac-command-key-is-meta nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key [?\s-c] 'kill-ring-save)
(global-set-key [?\s-v] 'yank)
(global-set-key [?\s-x] 'kill-region)
(global-set-key [?\s-z] 'undo)
(global-set-key [?\s-s] 'save-buffer)
(global-set-key [?\s-q] 'save-buffers-kill-terminal)
(global-set-key [?\s-f] 'isearch-forward)
(global-set-key [?\s-g] 'isearch-repeat-forward)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help)

;; フォントの設定
;; 出典：http://sakito.jp/emacs/emacs23.html
(create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
	'unicode
	(font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
	nil
	'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))

;; 行番号の設定（F5 キーで表示・非表示を切り替え）
;; 出典：調査中
(require 'linum)
(global-linum-mode 0)
(global-set-key [f5] 'linum-mode)
(setq linum-format 
	(lambda (line) (propertize (format 
		(let ((w (length (number-to-string 
			(count-lines (point-min) (point-max))
		)))) (concat "%" (number-to-string w) "d "))
	line) 'face 'linum)))
(setq linum-format "%5d ")


;;以下 TeX 関連の設定例です。全てオプショナルです。
;; *tex-shell* バッファを別ウィンドウに出力する
; (setq special-display-buffer-names
; 	'("*tex-shell*"))

;; *tex-shell* バッファの大きさ（高さ）を調整するためのキーの設定
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-~") 'shrink-window)

;; tex ファイルを dvi → ps ファイルに変換する設定 [control]+[cf]
;; 通常は (setq latex-run-command "platex") などと設定します。
;; 次のように設定しておくと、platex と dvips を一気に実行できます。
; (setq latex-run-command "F=*; platex -interaction=nonstopmode $F && dvips ${F%.tex}")

;; dvi ファイルを ps → pdf ファイルに変換する設定 [control]+[cp]
;; 通常は (setq tex-dvi-print-command "dvips -f * | lpr") などと設定します。
; 次のように設定しておくと、dvips と ps2pdf を一気に実行できます。
; (setq tex-dvi-print-command "F=*; dvips $F && ps2pdf ${F%dvi}ps")

;; 使用頻度の低いコマンド一覧 [control]+[cc] <TAB>
(defvar tex-compile-commands
	'(("platex %f" "%f" "%r.dvi")
	("open /Applications/Preview.app %r.pdf &" "%r.pdf")
	("gv %r.ps &" "%r.ps")
	("gv %r.pdf &" "%r.pdf")
	("jbibtex -kanji=utf8 %r" "%r.aux" "%r.bbl")
	("mendex -U %r" "%r.idx" "%r.ind")
	("dvipdfmx -p b5 %r" "%r.dvi" "%r.pdf")
	("dvipdfmx -p a4 %r" "%r.dvi" "%r.pdf")
	("dvipdfmx -p a3 %r" "%r.dvi" "%r.pdf")
	("dvips -tb5 -Phiragino %r.dvi" "%r.dvi" "%r.ps")
	("dvips -ta4 -Phiragino %r.dvi" "%r.dvi" "%r.ps")
	("dvips -ta3 -Phiragino %r.dvi" "%r.dvi" "%r.ps")
	("ps2pdf %r.ps" "%r.ps" "%r.pdf"))
	"List of commands for `tex-compile'.
Each element should be of the form (FORMAT IN OUT) where
FORMAT is an expression that evaluates to a string that can contain
  - `%r' the main file name without extension.
  - `%f' the main file name.
IN can be either a string (with the same % escapes in it) indicating
  the name of the input file, or t to indicate that the input is all
  the TeX files of the document, or nil if we don't know.
OUT describes the output file and is either a %-escaped string
  or nil to indicate that there is no output file.")

;; PS ファイルを gv で開く [control]+[cw]
(setq tex-ps-preview-command "/Applications/Ghostscript.app/gv")
(global-set-key "\C-c\C-w" 'tex-ps-preview)
(defun tex-ps-preview ()
	"Preview the .ps file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-ps-preview-command'."
	(interactive)
	(let ((preview-file-name-ps (tex-append tex-print-file ".ps"))
		test-name)
	(if (and (not (equal (current-buffer) tex-last-buffer-texed))
			(buffer-file-name)
			;; Check that this buffer's printed file is up to date.
			(file-newer-than-file-p
			(setq test-name (tex-append (buffer-file-name) ".ps"))
			(buffer-file-name)))
		(setq preview-file-name-ps test-name))
	(if (not (file-exists-p preview-file-name-ps))
		(error "No appropriate `.ps' file could be found")
	(if (tex-shell-running)
		(tex-kill-job)
		(tex-start-shell))
	(tex-send-command tex-ps-preview-command preview-file-name-ps)
		t)))

;; PDF ファイルを Preview.app で開く [control]+[cv]
(setq tex-pdf-preview-command "open -a /Applications/Preview.app")
(global-set-key "\C-c\C-v" 'tex-pdf-preview)
(defun tex-pdf-preview ()
	"Preview the . pdf file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-pdf-preview-command'."
	(interactive)
	(let ((preview-file-name-pdf (tex-append tex-print-file ".pdf"))
		test-name)
	(if (and (not (equal (current-buffer) tex-last-buffer-texed))
			(buffer-file-name)
			;; Check that this buffer's printed file is up to date.
			(file-newer-than-file-p
			(setq test-name (tex-append (buffer-file-name) ".ps"))
			(buffer-file-name)))
		(setq preview-file-name-pdf test-name))
	(if (not (file-exists-p preview-file-name-pdf))
		(error "No appropriate `. pdf' file could be found")
	(if (tex-shell-running)
		(tex-kill-job)
		(tex-start-shell))
	(tex-send-command tex-pdf-preview-command preview-file-name-pdf)
		t)))

;; TeX ファイルに対して、スペルチェックを実行する。
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
	'(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(add-hook 'tex-mode-hook
	'(lambda () (flyspell-mode)))


(if window-system (progn
    (set-background-color "Black")
    (set-foreground-color "LightGray")
    (set-cursor-color "Gray")
    ))

(define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す

))

;init24.el
(global-linum-mode t)
;変数global-linum-modeの値を真にする(有効にする）


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; Auto Complete
;;
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; -------------------------------------------------------------------------
;; @ minimap
(require 'minimap)

;; ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)
;; Rinari
(require 'rinari)

;;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))

