(load-relative "corfu.el")
(load-relative "corfu-history.el")
(rc/require 'cape)

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  ;(corfu-preselect 'directory) ;; Always preselect the prompt
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-prefix 0)         ; Start completing immediately
  (corfu-auto-delay 0.0)        ; No delay before showing completions
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



(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; ...

)

(keymap-unset corfu-map "<up>")
(keymap-unset corfu-map "<down>")


(setq-default indent-tabs-mode t
              tab-width 4)
