(setq ac-auto-show-menu nil)

(defun indent-or-company-complete ()
  (interactive)
  (let ((indent-pos (save-excursion (back-to-indentation) (point))))
    (if (<= (point) indent-pos)
        (indent-for-tab-command)
      (company-complete))))


(setq-default indent-tabs-mode t
              tab-width 4)
