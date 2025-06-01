;;; jai-mode.el --- Major mode for JAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023  Kristoffer Grönlund

;; Author: Kristoffer Grönlund <k@ziran.se>
;; Maintainer: Kristoffer Grönlund <k@ziran.se>
;; URL: https://github.com/krig/jai-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mdoe for JAI
;;

;;; Code:
(require 'rx)
(require 'js)
(require 'compile)

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst jai-builtins
  '("it" "it_index"))

(defconst jai-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "code_of" "initializer_of" "size_of" "type_of" "cast"  "type_info"
    "null" "true" "false" "xx" "context" "operator" "push_context" "is_constant"
    "enum_flags" "union" "interface"))

(defconst jai-typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "f32" "f64" "float"
    "float32" "float64" "string" "void"
    "bool" "Type"))

(defvar jai-struct-names '()
  "List of struct names found in the buffer.")

(defun jai-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun jai-keywords-rx (keywords)
  "build keyword regexp"
  (jai-wrap-word-rx (regexp-opt keywords t)))

(defconst jai-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst jai-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))


(defconst jai-func-declaration-rx
  (rx (group (1+ word))  ; Capture the type name
      (0+ space)         ; Allow spaces
      "::"               ; Double colon
      (0+ space)         ; Allow spaces
      (or "(" (and (1+ word) (0+ space) "=>"))  ; Allow these keywords
  )
  "Regex to match Jai type declarations, excluding cases where '(' appears after '::'.")


(defface jai-operator-face '((t (:foreground "#bd2c2d")))
  "Face for C/C++ operators.")

(defface jai-uppercase-face '((t (:foreground "#96a6c8")))
  "Face for uppercase words in C/C++.")

(defface jai-number-face '((t (:foreground "Yellow" :weight bold)))
  "Face for numerical constants in C/C++.")

(defconst jai-font-lock-defaults
  `(;; Keywords
    (,(jai-keywords-rx jai-keywords) 1 font-lock-keyword-face)

    ;; Single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(jai-keywords-rx jai-builtins) 1 font-lock-variable-name-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At notes
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)
	;; Funcx
	
    ;; Numbers
    (,(jai-wrap-word-rx jai-number-rx) . 'jai-number-face)

    ;; Built-in types
    (,(jai-keywords-rx jai-typenames) 1 font-lock-type-face)


	("\\b\\([A-Z_][A-Z0-9_]+\\)\\b" 1 'jai-uppercase-face)
	
	;; Function declarations
    (,jai-func-declaration-rx 1 font-lock-function-name-face t)
	("\\<\\(\\w+\\)\\s-*(" 1 font-lock-function-name-face)
	
    ;; Hat and dollar types
    (,jai-hat-type-rx 1 font-lock-type-face)
    (,jai-dollar-type-rx 1 font-lock-type-face)

	(,(regexp-opt '("." "+" "-" "*" "/" "%" ":" "=" "+=" "-=" "*=" "/=" "%="
                    "==" "!=" ">=" "<=" "&&" "||" "!" "&" "|" "^" "~" "<<" ">>" "<" ">") t) . 'jai-operator-face)

    ;; Other constants
    ("---" . font-lock-constant-face))

	;; Operators
  )

(defvar jai-highlight-rules '()
  "List of highlight rules, each containing a regex and a face.")

(defvar jai-last-highlight-keywords '()
  "List of keywords used for the last highlighting update, keyed by rule.")

(defvar jai-refresh-timer nil
  "Timer for debouncing highlight refresh.")

(defun jai-extract-names (regex &optional limit)
  "Extract names from the buffer using the provided regex. Optionally limit search region."
  (let ((new-names '()))
    (save-excursion
      (goto-char (if limit (max (point-min) (- (point) limit)) (point-min)))
      (while (re-search-forward regex nil t)
        (push (match-string 1) new-names)))
    new-names))

(defun jai-generate-regex (names)
  "Generate a regex for dynamically detected names."
  (when names
    (concat "\\<" (regexp-opt names t) "\\>")))

(defun jai-update-highlighting (rule new-regex face)
  "Update font-lock highlighting for a specific rule."
  (let ((last-keywords (cdr (assoc rule jai-last-highlight-keywords))))
    (when last-keywords
      (font-lock-remove-keywords nil last-keywords)
      (setq jai-last-highlight-keywords (assoc-delete-all rule jai-last-highlight-keywords))))
  (when new-regex
    (let ((new-keywords `((,new-regex . ,face))))
      (font-lock-add-keywords nil new-keywords)
      (push (cons rule new-keywords) jai-last-highlight-keywords)))
  (when (fboundp 'jit-lock-refontify)
    (jit-lock-refontify))) ;; More efficient than font-lock-flush

(defun jai-schedule-refresh ()
  "Schedule a refresh with a short delay to avoid excessive updates."
  (when jai-refresh-timer
    (cancel-timer jai-refresh-timer))
  (setq jai-refresh-timer
        (run-with-idle-timer 0.1 nil #'jai-refresh-highlighting)))

(defun jai-refresh-highlighting ()
  "Refresh highlighting efficiently."
  (dolist (rule jai-highlight-rules)
    (let* ((regex (car rule))
           (face (cdr rule))
           (names (jai-extract-names regex 10000)) ;; Limit scanning range
           (new-regex (jai-generate-regex names)))
      (jai-update-highlighting rule new-regex face))))

(defun jai-trigger-refresh (_beg _end _len)
  "Trigger a refresh with debouncing when text is modified."
  (jai-schedule-refresh))

(defun jai-enable-dynamic-highlighting ()
  "Enable real-time name highlighting."
  (dolist (rule jai-highlight-rules)
    (let* ((regex (car rule))
           (face (cdr rule))
           (names (jai-extract-names regex 10000)) ;; Limit scanning range
           (new-regex (jai-generate-regex names)))
      (jai-update-highlighting rule new-regex face)))
  (add-hook 'after-change-functions #'jai-trigger-refresh nil t))

(defun jai-add-highlight-rule (regex face)
  "Add a new highlight rule with the given regex and face."
  (add-to-list 'jai-highlight-rules (cons regex face))
  (jai-enable-dynamic-highlighting))

(add-hook 'jai-mode-hook #'jai-enable-dynamic-highlighting)

(defconst jai-type-declaration-rx
  (rx (group (1+ word))  ; Capture the type name
      (0+ space)         ; Allow spaces
      "::"               ; Double colon
      (0+ space)         ; Allow spaces
      (or "struct" "enum" "enum_flags" "union")  ; Allow these keywords
  )
  "Regex to match Jai type declarations, excluding cases where '(' appears after '::'.")
(jai-add-highlight-rule jai-type-declaration-rx 'font-lock-type-face)

(jai-add-highlight-rule "\\(\\w+\\)\\s-*::\\s-*[*A-Z0-9\"[]" 'font-lock-function-name-face)


;(jai-add-highlight-rule "\\<[a-zA-Z_]+\\>[[:space:]]*:[[:space:]]*\\(?:\\[[^]]+\\][[:space:]]*\\)?\\<\\([a-zA-Z0-9_]+\\)\\>[[:space:]]*;" 'font-lock-type-face)
(jai-add-highlight-rule "\\<[a-zA-Z_]+\\>[[:space:]]*:[[:space:]]*\\(?:\\[[^]]+\\][[:space:]]*\\)?\\<\\([*]?[a-zA-Z0-9_]+\\)\\>[[:space:]]*[;),]" 'font-lock-type-face)




;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun ()
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (when (jai-line-is-defun)
    (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'jai-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; imenu hookup
(add-hook 'jai-mode-hook
          (lambda ()
            (setq imenu-generic-expression
                  '(("Type" "^\\(.*:*.*\\) : " 1)
                    ("Function" "^\\(.*\\) :: " 1)
                    ("Struct" "^\\(.*\\) *:: *\\(struct\\)\\(.*\\){" 1)))))

;; NOTE: taken from the scala-indent package and modified for Jai.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun jai--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun jai--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'jai--indent-on-parentheses))

;;;###autoload
(define-derived-mode jai-mode jai-parent-mode "Jai"
  :syntax-table jai-mode-syntax-table
  :group 'jai
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "//")
  (setq-local block-comment-start "/*")
  (setq-local block-comment-end "*/")
  (setq-local indent-line-function 'js-indent-line)
  (setq-local font-lock-defaults '(jai-font-lock-defaults))
  (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
  (setq-local end-of-defun-function 'jai-end-of-defun)

  ;; add indent functionality to some characters
  (jai--add-self-insert-hooks)

  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(setq compilation-error-regexp-alist-alist "")
(defconst jai--error-regexp
  "\\([^ \n:]+.*\.jai\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(jai ,jai--error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'jai compilation-error-regexp-alist)

(provide 'jai-mode)
;;; jai-mode.el ends here




