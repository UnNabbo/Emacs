(load-relative "corfu.el")
(load-relative "corfu-history.el")

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  ;(corfu-preselect 'directory) ;; Always preselect the prompt
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-prefix 1)         ; Start completing immediately
  (corfu-auto-delay 0)        ; No delay before showing completions
  (corfu-count 0)
  (corfu-preselect 'prompt)    ; Always preselect first candidate
  :config
  (advice-add #'corfu--message :override #'ignore)
  ;; Override the popup display function to do nothing
  (advice-add #'corfu--show :override #'ignore)
  ;; Ensure candidates are still processed (silent backend)
  (setq corfu--candidates nil)  ; Prevent visual clutter
  
  :bind  ; Use TAB for cycling, default is `corfu-complete'.
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

(keymap-unset corfu-map "<up>")
(keymap-unset corfu-map "<down>")



(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-prefix)) ; Only prefix matching
  (orderless-style-dispatchers nil) ; Disable special pattern dispatchers
  :config
  ;; Set Orderless as the only completion style
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides nil))

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 99)
       (cons 'orderless-literal-prefix word)))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

(add-hook 'corfu-mode-hook
          (lambda ()
            (setq-local completion-styles '(orderless-fast basic)
                        completion-category-overrides nil
                        completion-category-defaults nil)))

(add-hook 'corfu-mode-hook
          (lambda ()
            (setq-local completion-styles '(orderless-prefix)
                        completion-category-overrides nil
                        completion-category-defaults nil)))

(rc/require 'cape)
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

