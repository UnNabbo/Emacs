(custom-set-faces '(font-lock-number-face ((t (:foreground "Yellow")))))
(custom-set-faces '(font-lock-operator-face ((t (:foreground "#bd2c2d")))))

(defun jai/if0 (start end)
  "Wraps the selected region with #if 0 and #endif."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert "\n}")
    (goto-char start)
    (insert "#if false{\n")))

(defconst jai-operators
  '("." ":" "+" "-" "*" "/" 
    "%" "=" "+=" "-=" "*=" "/=" "%="
    "==" "!=" ">=" "<=" "&&" "||" "!" "&" "|" "^" "~" "<<" ">>" "<" ">"))

(font-lock-add-keywords 'jai-ts-mode
						`((,(regexp-opt jai-operators) 0 'font-lock-operator-face )))


(defun treesit-enabled-p ()
  "Checks if the current buffer has a treesit parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-at (point))))

;; Add language node types that are considered declarations:
(setq declaration-node-types '("procedure_declaration" "variable_declaration" "struct_declaration" "function_declaration"))

(defun string-contains-any-substring-p (haystack targets)
  "Check if HAYSTACK string contains any string from the TARGETS list.

HAYSTACK is the string to search within.
TARGETS is a list of strings to search for.

Search is case-sensitive by default (respects `case-fold-search`).
Target strings are treated literally (regex metacharacters are quoted).

Returns t if any string in TARGETS is found as a substring within HAYSTACK,
nil otherwise."
  (seq-some
   (lambda (target-string) (string-match-p (regexp-quote target-string) haystack))
   targets))

(defun cp/check-inspect-name-against-declarations ()
  "Calls treesit-inspect-node-at-point and then checks if the
internal variable treesit--inspect-name exactly matches any type
in a predefined list."
  (interactive)
  (when (treesit-enabled-p)
    (call-interactively #'treesit-inspect-node-at-point)
    (if (boundp 'treesit--inspect-name)
        (string-contains-any-substring-p treesit--inspect-name declaration-node-types))))

(defun cp/go-to-def-or-ref ()
  (interactive)
  (let ((cur (line-number-at-pos))
        (cur-pt (point)))
    (if (cp/check-inspect-name-against-declarations)
        (call-interactively #'xref-find-references)
      (call-interactively #'xref-find-definitions))))

