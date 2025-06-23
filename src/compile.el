(require 'compile)
(require 'ansi-color)

(setq compilation-ask-about-save nil)

(defvar root-directory (or default-directory ".") "The directory to run compile commands from.")

(defun set-root (directory)
  (interactive "DDirectory to save: ")
  (setq root-directory (expand-file-name directory))
  (message "Saved root directory: %s" root-directory))

(defun compile-in-root ()
  "Run compile in the saved directory, restoring the previous default directory."
  (interactive)
  (if root-directory
      (let ((original-directory default-directory))
        (setq default-directory root-directory)
        (unwind-protect
            (call-interactively 'compile)
          (setq default-directory original-directory)))
    (message "No compile directory saved. Use save-compile-directory to set one.")))

(defun compile-in-root-without-asking (&optional extra-args)
  "Compile in `root-directory` without asking.
If EXTRA-ARGS is provided, append it to `compile-command`."
  (interactive)
  (if root-directory
      (let ((original-directory default-directory)
            (original-compile-command compile-command))
        (unwind-protect
            (progn
              (setq compile-command (or compile-command "build.bat"))
              (when (and extra-args (not (string-empty-p extra-args)))
                (setq compile-command (concat compile-command " " extra-args)))
              (setq default-directory root-directory)
              (compile compile-command))
          ;; Restore original state
          (setq compile-command original-compile-command)
          (setq default-directory original-directory)))
    (message "No compile directory saved. Use set-root to set one.")))

