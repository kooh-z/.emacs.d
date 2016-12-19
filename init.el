(cond ((>= emacs-major-version 23)
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)
(show-paren-mode nil)
(set-frame-parameter nil 'alpha '(90 70))
(setq mac-mouse-wheel-smooth-scroll t)
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
(create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
	'unicode
	(font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
	nil
	'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
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
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-~") 'shrink-window)
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

(global-linum-mode t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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

(require 'minimap)
(minimap-mode 1)

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

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "C-m") 'ac-complete)

(set-frame-parameter nil 'fullscreen 'maximized)

;; sublimeテーマ
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (sublime-themes ruby-electric ruby-block rinari rhtml-mode rbenv rake rails-log-mode neotree minimap go-mode flycheck-pos-tip enh-ruby-mode auto-highlight-symbol auto-complete-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'spolsky t)
(setq inhibit-startup-message t)

;; ツールバー非表示
;;(tool-bar-mode 0)
;; メニューバー非表示
(menu-bar-mode 0)
;; スクロールバー非表示
;;(scroll-bar-mode 0)

(global-set-key [f8] 'neotree-toggle)
;; 隠しファイルをデフォルトで表示
(setq neo-show-hidden-files t)

;; neotree でファイルを新規作成した後、自動的にファイルを開く
(setq neo-create-file-auto-open t)

;; delete-other-window で neotree ウィンドウを消さない
(setq neo-persist-show t)

;; キーバインドをシンプルにする
(setq neo-keymap-style 'concise)

;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)
