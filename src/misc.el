
(defun indent-or-dabbrev-expand ()
  (interactive)
  (let ((indent-pos (save-excursion (back-to-indentation) (point))))
    (if (<= (point) indent-pos)
        (indent-for-tab-command)
      (dabbrev-expand nil))))

(setq-default indent-tabs-mode t
              tab-width 4)
