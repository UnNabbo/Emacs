;;; Initialization
(package-initialize)
(setq custom-file (expand-file-name "../custom.el" user-emacs-directory))
(load-file custom-file)

;;; REMOVE TO DEBUG INIT FILE
(setq-default message-log-max nil)

;(kill-buffer "*Messages*")
(setq make-backup-files nil)


(load-file  (expand-file-name "../src/basic.el" user-emacs-directory))
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c M-q") 'rc/unfill-paragraph)
(global-set-key (kbd "C-,") 'rc/duplicate-line)
(global-set-key (kbd "C-x p s") 'rc/rgrep-selected)
(global-set-key (kbd "C-x p d") 'rc/insert-timestamp)

(load-relative "src/misc.el")
;(global-set-key (kbd "<tab>") 'indent-or-dabbrev-expand)
(global-set-key (kbd "C-ò") 'jump-to-next-char)

(load-relative "src/ui.el")
(global-set-key (kbd "C-c t") 'switch-theme)

(setq compile-command "jai first.jai -")
(load-relative "src/compile.el")
(global-set-key (kbd "M-m") 'compile-in-root-without-asking)
(global-set-key (kbd "M-<f1>") 'compile-in-root-without-asking)
(global-set-key (kbd "M-<f2>") (lambda () (interactive) (compile-in-root-without-asking "debug")))
(global-set-key (kbd "M-<f3>") (lambda () (interactive) (compile-in-root-without-asking "release")))
(global-set-key (kbd "M-<f4>") (lambda () (interactive) (compile-in-root-without-asking "debug attach_debugger")))

(load-relative "src/modules/jump.el")

(load-relative "src/modules/drag.el")
(define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
(define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)

(load-relative "src/modules/ido_smex.el")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(load-relative "src/modules/imenu.el")
(global-set-key (kbd "C-c d") 'definition-list-buffer)

(load-relative "src/modules/markdown.el")


(load-relative "src/modules/treesit.el")

;; (load-relative "src/modules/topsy.el")
;; (rc/require 'rg)
;; (rc/require 'swiper)
;; (keymap-global-set "C-s" #'swiper-isearch)

(require 'which-key)
(setq which-key-sort-order 'which-key-prefix-then-key-order)
(which-key-mode)

(load-relative "src/modules/projectile_ext.el")
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load-relative "src/modules/autocomplete.el")

(rc/require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(rc/require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-p") 'ace-swap-window)

(rc/require 'undo-fu-session)
(undo-fu-session-global-mode)

(rc/require 'vundo)

(load-relative "src/modes/jai-mode.el")
(load-relative "src/modes/jai-ts-mode.el")
(load-relative "src/jai_tweaks.el")
(define-key jai-ts-mode-map (kbd "C-M-a")   'jai-ts-mode--prev-defun)
(define-key jai-ts-mode-map (kbd "C-M-e")   'jai-ts-mode--next-defun)
(define-key jai-ts-mode-map (kbd "C-M-S-l") 'jai-ts-mode--align-struct)
(define-key jai-ts-mode-map (kbd "C-M-l")   'align-regexp)
(define-key jai-ts-mode-map (kbd "M-i")   'jai/if0)

(rc/require 'glsl-mode)

(load-relative "src/c-extension.el")
(with-eval-after-load 'cc-mode
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key map (kbd "C-c C-f") 'find-corresponding-file)
    (define-key map (kbd "M-i") 'if0)))


(global-unset-key [mouse-2])
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-n") (lambda () (interactive) (duplicate-line) (next-line)))
(global-set-key (kbd "M-RET") (lambda () (interactive) (push-mark) (ffap)))


(load-relative "src/emacs_bug.el")

;TODO make this stick to every ts-mode
(defconst jai-operators
  '("." ":" "+" "-" "*" "/" 
    "%" "=" "+=" "-=" "*=" "/=" "%="
    "==" "!=" ">=" "<=" "&&" "||" "!" "&" "|" "^" "~" "<<" ">>" "<" ">"))

(font-lock-add-keywords 'python-ts-mode
						`((,(regexp-opt jai-operators) 0 'font-lock-operator-face )))

(setq exec-path (add-to-list 'exec-path "C:/Program Files/Gow/bin"))
(setenv "PATH" (concat "C:\\Program Files\\Gow\\bin;" (getenv "PATH"))) 
