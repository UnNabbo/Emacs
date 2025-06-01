(rc/require 'imenu)
(rc/require 'cc-mode)

(defun definition-list-extract-function-signature (name)
  "Extract the function name and arguments, supporting various signature formats."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "%s[ 	]*::[ 	]*\(?:inline[ 	]*\)?(.*?)[ 	]*[;]" (regexp-quote name)) nil t)
      (match-string 0))))

(defvar definition-list-keywords '("Class" "Struct" "Interface" "Type" "Function")
  "List of keywords to expand in the definition list.")

(defun definition-list-buffer ()
  "Rescan imenu and display a buffer listing all imenu indices with function names and arguments.
   Entries matching `definition-list-keywords` are highlighted."
  (interactive)
  (imenu--menubar-select imenu--rescan-item)
  (let* ((index-alist (imenu--make-index-alist t))
         (index-alist
          (cl-mapcan (lambda (entry)
                       (if (and (consp entry) (member (car entry) definition-list-keywords))
                           (mapcar (lambda (subentry)
                                     (cons (propertize (or (definition-list-extract-function-signature (car subentry))
                                                           (car subentry))
                                                       'face 'font-lock-type-face)
                                           (cdr subentry)))
                                   (cdr entry))
                         (list (cons (or (definition-list-extract-function-signature (car entry)) (car entry))
                                     (cdr entry)))))
                     index-alist)))
    (with-current-buffer (get-buffer-create "*Definition List*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry index-alist)
          (unless (string= (car entry) "*Rescan*")
            (insert (car entry) "\n")))
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "RET") 'definition-list-jump)
        (setq-local definition-list-index index-alist)
        (setq truncate-lines t buffer-read-only t)))
    (switch-to-buffer "*Definition List*")))


(defun definition-list-jump ()
  "Jump to the selected definition list entry."
  (interactive)
  (let* ((entry-name (thing-at-point 'line t))
         (entry (assoc (string-trim entry-name) definition-list-index)))
    (when (and entry (cdr entry))
      (switch-to-buffer (marker-buffer (cdr entry)))
      (goto-char (cdr entry))
      (kill-buffer "*Definition List*"))))
