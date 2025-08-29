(add-to-list 'default-frame-alist `(font . "Iosevka-18"))

(setq visible-bell 1)
(setq inhibit-startup-message t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(delete-selection-mode 1)
(display-time)
(setq scroll-step 3)

;;; Theme
(rc/require-theme 'naysayer)
(rc/require-theme 'gruber-darker)
;; (rc/require 'ef-themes)
;; (use-package ef-themes
;;   :init (load-theme 'ef-bio);;(load-theme 'ef-spring t)
;;         (set-face-italic-p 'italic nil)
;;         (set-face-bold-p 'bold nil)
;;         (set-face-background 'default "#052525"))


(defvar my-themes '(naysayer gruber-darker)
  "List of themes to toggle between.")

(defvar my-current-theme 'naysayer
  "Currently active theme.")

(defun switch-theme ()
  "Completely reset faces and switch between the themes defined in `my-themes`."
  (interactive)
  ;; Reset all custom face attributes
  (mapc #'face-remap-remove-relative face-remapping-alist)
  (setq face-remapping-alist nil)

  ;; Disable all currently enabled themes
  (mapc #'disable-theme custom-enabled-themes)

  ;; Get the next theme in the list, cycling back if needed
  (let ((next-theme (car (or (cdr (member my-current-theme my-themes)) my-themes))))
    (load-theme next-theme t)
    (setq my-current-theme next-theme)
    (message "Switched to theme: %s" next-theme)))


