(load-relative "corfu.el")
(load-relative "corfu-history.el")
(defun my-project-buffers ()
  "Return a list of buffers belonging to the current project, with filtering."
  (when-let ((project (project-current)))
    (cl-remove-if-not
     (lambda (buffer)
       (when-let ((file (buffer-file-name buffer)))
         (and (project-file-p project file)
              (my-allowed-file-p file)     ; Add your custom filtering
              (not (my-excluded-file-p file)))))
     (buffer-list))))

(defun my-allowed-file-p (file)
  "Check if FILE should be included based on extension."
  (let ((ext (file-name-extension file)))
    (member ext '("jai"))))

(defun my-excluded-file-p (file)
  "Check if FILE should be excluded based on path."
  (string-match-p "/\\(out\\|dist\\|build\\)/" file))

;; Configure cape to only search in project buffers
(setq cape-dabbrev-check-other-buffers 'my-project-buffers)
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
  (setq completion-at-point-functions
        (list
		 ;#'cape-dabbrev
		 #'my-cape-dabbrev-from-project-files
		 #'cape-file
		 #'cape-keyword))
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

(rc/require 'cape)
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

