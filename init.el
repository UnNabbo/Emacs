;;; Initialization
(package-initialize)

(setq custom-file (expand-file-name "../custom.el" user-emacs-directory))
(load-file custom-file)

;;; REMOVE TO DEBUG INIT FILE
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(load-file  (expand-file-name "../src/basic.el" user-emacs-directory))
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i m") 'imenu)
(global-set-key (kbd "C-c M-q") 'rc/unfill-paragraph)
(global-set-key (kbd "C-,") 'rc/duplicate-line)

(load-relative "src/misc.el")
(global-set-key (kbd "<tab>") 'indent-or-dabbrev-expand)

(load-relative "src/ui.el")
(global-set-key (kbd "C-c t") 'switch-theme)

(load-relative "src/compile.el")
(global-set-key (kbd "M-m") 'compile-in-root-without-asking)
(global-set-key (kbd "M-<f1>") 'compile-in-root-without-asking)
(global-set-key (kbd "M-<f2>") (lambda () (interactive) (compile-in-root-without-asking "debug")))
(global-set-key (kbd "M-<f3>") (lambda () (interactive) (compile-in-root-without-asking "release")))
(global-set-key (kbd "M-<f4>") (lambda () (interactive) (compile-in-root-without-asking "debug attach_debugger")))

(load-relative "src/modules/jump.el")

(load-relative "src/modules/drag.el")

(load-relative "src/modules/ido_smex.el")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(load-relative "src/modules/imenu.el")
(global-set-key (kbd "C-c d") 'definition-list-buffer)

(load-relative "src/modules/markdown.el")

(load-relative "src/modules/treesit.el")

(load-relative "src/modes/jai-ts-mode.el")
(load-relative "src/jai_tweaks.el")
(define-key jai-ts-mode-map (kbd "C-M-a") 'jai-ts-mode--prev-defun)
(define-key jai-ts-mode-map (kbd "C-M-e") 'jai-ts-mode--next-defun)
(define-key jai-ts-mode-map (kbd "C-M-l") 'align-regexp)
(define-key jai-ts-mode-map (kbd "C-M-S-l") 'jai-ts-mode--align-struct)

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

(global-unset-key [mouse-2])
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-n") (lambda () (interactive) (duplicate-line) (next-line)))

