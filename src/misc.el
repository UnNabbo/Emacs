(defun indent-or-dabbrev-expand ()
  (interactive)
  (let ((indent-pos (save-excursion (back-to-indentation) (point))))
    (if (<= (point) indent-pos)
        (indent-for-tab-command)
      (dabbrev-expand nil))))

(setq-default indent-tabs-mode t
              tab-width 4)

(setq fixme-modes '(c++-ts-mode c-ts-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)

