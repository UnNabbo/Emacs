;; Define custom keywords



(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
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



(defvar my-c-custom-keywords
  '("internal" "global" "local_presist"
    "defer" "for_each" "for_range"
    "thread_local" "case_break" "case_through" "case_default" "StaticAssert")
  "Additional user-defined C/C++ keywords.")

;; Define standard C/C++ keywords
(defvar my-c-keywords
  '("if" "else" "while" "for" "switch" "catch" "return" "throw" "sizeof" "typeid" "decltype")
  "C/C++ keywords that are typically used with parentheses ().")

;; Combine all keywords into a single list
(defvar my-c-all-keywords (append my-c-custom-keywords my-c-keywords)
  "Combined list of all C/C++ keywords.")

;; Define faces for highlighting
(defface my-c-operator-face '((t (:foreground "#bd2c2d")))
  "Face for C/C++ operators.")

(defface my-c-uppercase-face '((t (:foreground "#96a6c8")))
  "Face for uppercase words in C/C++.")

(defface my-c-number-face '((t (:foreground "Yellow" :weight bold)))
  "Face for numerical constants in C/C++.")
;; Main highlighting function
(defun my-c-highlight-syntax ()
  "Highlight functions, keywords, operators, uppercase words, and numerical constants in C/C++."
  ;; Remove default type highlighting for custom keywords
  (font-lock-remove-keywords
   nil `((,(regexp-opt my-c-all-keywords 'words) . font-lock-type-face)))
  ;; Add custom highlighting rules
  (font-lock-add-keywords
   nil `(
         ;; Highlight custom keywords
         (,(regexp-opt my-c-all-keywords 'words) . font-lock-keyword-face)
         ;; Highlight operators
         (,(regexp-opt '("." "+" "-" "*" "/" "%" "=" "+=" "-=" "*=" "/=" "%="
                         "==" "!=" ">=" "<=" "&&" "||" "!" "&" "|" "^" "~" "<<" ">>" "<" ">") t) . 'my-c-operator-face)
		 ;; Highlight words in < > as types if they are alone or separated by commas
         ("<\\([^<>,\n]+\\),\\([^<>,\n]+\\)>" 1 'font-lock-type-face)
         ("<\\([^<>,\n]+\\)>" 1 'font-lock-type-face)
         ;; Highlight templated function calls (e.g., foo<type>(args))
         ("\\<\\(\\w+\\)\\s-*<[^<>]+>\\s-*(" 1 font-lock-function-name-face)
         ;; Highlight function names (only if they are not in all-keywords)
         ("\\<\\(\\w+\\)\\s-*(" 1 (unless (member (match-string 1) my-c-all-keywords)
                               font-lock-function-name-face))
         ;; Highlight uppercase words
         ("\\b\\([A-Z_][A-Z0-9_]+\\)\\b" 1 'my-c-uppercase-face)
         ;; Highlight numerical constants
         ("\\b\\(0x[0-9a-fA-F]+\\|0b[01]+\\|[0-9]+\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?[fF]?\\)\\b"
          . 'my-c-number-face))))

;; Function to add a new keyword
(defun c-add-keyword (keyword)
  "Add a C/C++ keyword and refresh highlighting."
  (interactive "sEnter new C/C++ keyword: ")
  (unless (member keyword my-c-custom-keywords)
    (push keyword my-c-custom-keywords))
  (setq my-c-all-keywords (append my-c-custom-keywords my-c-keywords)) ; Update all-keywords
  (font-lock-flush)
  (when (derived-mode-p 'c-mode 'c++-mode) (funcall major-mode)))

;; Add hooks for C and C++ modes
(add-hook 'c-mode-hook #'my-c-highlight-syntax)
(add-hook 'c++-mode-hook #'my-c-highlight-syntax)

;; Use tabs for indentation in C and C++ modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq indent-tabs-mode t)  ; Use tabs for indentation
            (setq tab-width 4)     ; Set tab width to 4 spaces (or your preferred width)
            (setq c-basic-offset 4)))  ; Set indentation offset to match tab width


;;; C++ Utilities
(defun find-corresponding-file ()
  "Switch between a header file (.h) and its corresponding source file (.c/.cpp), or vice versa."
  (interactive)
  (let* ((file (buffer-file-name))
         (base-name (file-name-base file))
         (directory (file-name-directory file))
         (header-extension ".h")
         (source-extensions '(".c" ".cpp"))
         (corresponding-file nil))
    (catch 'found
      ;; If we're in a header file, look for the corresponding source file (.c or .cpp)
      (if (equal (file-name-extension file) "h") ;; Check if it's a header file
          (dolist (ext source-extensions)
            (let ((possible-source (concat directory base-name ext)))
              (when (file-exists-p possible-source)
                (setq corresponding-file possible-source)
                (throw 'found corresponding-file)))))
      
      ;; If we're in a source file (.c or .cpp), look for the corresponding header file (.h)
      (if (member (file-name-extension file) '("c" "cpp")) ;; Check if it's a source file
          (let ((possible-header (concat directory base-name header-extension)))
            (when (file-exists-p possible-header)
              (setq corresponding-file possible-header)
              (throw 'found corresponding-file)))))
    
    ;; Open the found corresponding file, or show a message if not found
    (if corresponding-file
        (let ((compilation-window (get-buffer-window "*compilation*")))
          (if compilation-window
              (with-selected-window compilation-window
                (find-file corresponding-file))
            (if (one-window-p)
                (find-file corresponding-file)
              (other-window 1)
              (find-file corresponding-file))))
      (message "No corresponding file found."))))

(defun if0 (start end)
  "Wraps the selected region with #if 0 and #endif."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert "\n#endif")
    (goto-char start)
    (insert "#if 0\n")))


(add-to-list 'compilation-error-regexp-alist 'casey-devenv)
(add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
													 "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
													 2 3 nil (4)))

