(defun indent-or-dabbrev-expand ()
  (interactive)
  (let ((indent-pos (save-excursion (back-to-indentation) (point))))
    (if (<= (point) indent-pos)
        (indent-for-tab-command)
      (dabbrev-expand nil))))

(setq-default indent-tabs-mode t
              tab-width 4)

(setq fixme-modes '(c++-ts-mode jai-ts-mode c-ts-mode emacs-lisp-mode))
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


(defun jump-to-next-char (c &optional count)
  "Select text from point to the next occurrence of character C.
With optional COUNT, skip COUNT matches of C."
  (interactive "c: \np")
  (let ((start (point)))
    ;; Adjust count if point is already on the character
    (when (string= (string c) (buffer-substring (point) (min (point-max) (+ 1 (point)))))
      (setq count (1+ count)))
    (when (search-forward (string c) nil t count)
      ;; Set mark at original point and leave point after found char
      (set-mark start)
      (goto-char (match-beginning 0))
      (activate-mark))))


(setq-default indent-tabs-mode 'only)
(advice-add 'indent-to :around
  (lambda (orig-fun column &rest args)
    (when (eq indent-tabs-mode 'only)
      (setq column (* tab-width (round column tab-width))))
    (apply orig-fun column args)))
