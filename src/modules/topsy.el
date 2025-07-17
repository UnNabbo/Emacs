(rc/require 'topsy)

(add-hook 'prog-mode-hook #'topsy-mode)
(add-hook 'magit-section-mode-hook #'topsy-mode)

(defun topsy--jai-beginning-of-defun ()
  "Return the line moved to by `jai-ts-mode--prev-defun'."
  (when (> (window-start) 1)
    (save-excursion
      (goto-char (window-start))
      (jai-ts-mode--prev-defun)
      (font-lock-ensure (point) (pos-eol))
      (buffer-substring (point) (pos-eol)))))

(add-to-list 'topsy-mode-functions '(jai-ts-mode . topsy--jai-beginning-of-defun))
