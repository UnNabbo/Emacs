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

;; (with-eval-after-load 'dumb-jump
;;   ;; File recognition
;;   (add-to-list 'dumb-jump-language-file-exts
;;                '(:language "jai" :ext "jai" :agtype "jai" :rgtype "jai"))
  
;;   ;; Regex rules
;;   (add-to-list 'dumb-jump-find-rules
;;                '(:language "jai"
;;                  :type "regex"
;;                  :supports ("ag" "grep" "rg")
;;                  :regex "\\_<\\(fn\\|function\\)\\s-+\\(\\_<.+?\\_>\\)"
;;                  :capture 2))
  
;;   (setq dumb-jump-find-rules
;;         (append dumb-jump-find-rules
;;                 '(;; Function definitions
;;                   (:type "regex" :language "jai"
;;                    :regex "fn\\s-+\\([[:word:]]+\\)" :capture 1)
;;                   ;; Variables
;;                   (:type "regex" :language "jai"
;;                    :regex "\\(?:let\\|var\\)\\s-+\\([[:word:]]+\\)" :capture 1)))))

;; ;; XRef integration
;; (add-hook 'jai-ts-mode-hook
;;           (lambda ()
;;             (setq-local xref-backend-functions
;;                         (list (lambda ()
;;                                 (when (bound-and-true-p jai-ts-mode)
;;                                   #'dumb-jump-xref-backend))))))

;; ;; Keybindings
;; (add-hook 'jai-ts-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-.") 'xref-find-definitions)
;;             (local-set-key (kbd "M-?") 'xref-find-references)))
(require 'xref)
(require 'treesit)
(require 'project)

(defun jai-ts--symbol-at-point ()
  (thing-at-point 'symbol t))

(defun jai-ts--jai-files-in-project ()
  "List all .jai files under the project root."
  (let ((root (jai-ts--project-root)))
    (directory-files-recursively root "\\.jai\\'")))

(defun jai-ts--treesit-parse-file (file)
  "Return root Tree-sitter node of FILE, or nil if not parsable."
  (with-temp-buffer
    (insert-file-contents file)
    (when (treesit-ready-p 'jai)
      (treesit-parser-create 'jai)
      (treesit-buffer-root-node))))

(defun jai-ts--collect-nodes-by-type (node type)
  "Recursively collect all nodes of TYPE under NODE."
  (let (results)
    (when (string= (treesit-node-type node) type)
      (push node results))
    (dolist (child (treesit-node-children node))
      (setq results (nconc results (jai-ts--collect-nodes-by-type child type))))
    results))

(defun jai-ts--collect-nodes-in-file (file types)
  "Return all nodes of given TYPES in FILE."
  (when-let ((root (jai-ts--treesit-parse-file file)))
    (cl-loop for type in types
             nconc (jai-ts--collect-nodes-by-type root type))))

(defun jai-ts--find-project-root ()
  "Find the nearest parent directory matching D:/DEV/Project_Name pattern."
  (let ((current-dir (file-name-directory (buffer-file-name))))
    (while (and current-dir
                (not (string-match-p "\\`D:/DEV/[^/]+/?\\'" current-dir)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (when (and current-dir (string-match-p "\\`D:/DEV/[^/]+/?\\'" current-dir))
      (file-name-as-directory current-dir))))

(defun jai-ts--find-all-jai-files ()
  "Find all .jai files in project, scanning upward to project root and down through subdirs."
  (let* ((start-dir (file-name-directory (buffer-file-name)))
         (project-root (jai-ts--find-project-root))
         (files nil))
    
    (if project-root
        (progn
          (while (and start-dir
                      (not (string= start-dir project-root)))
            (setq files (nconc files (directory-files-recursively start-dir "\\.jai\\'")))
            (setq start-dir (file-name-directory (directory-file-name start-dir))))
          
          (setq files (nconc files (directory-files-recursively project-root "\\.jai\\'"))))
      
      (setq files (directory-files-recursively start-dir "\\.jai\\'")))
    
    (delete-dups files)))

(defun jai-ts--find-definitions-in-buffer (buffer identifier)
  "Find definitions of IDENTIFIER in BUFFER using treesit."
  (with-current-buffer buffer
    (when (treesit-parser-list)
      (let* ((root (treesit-buffer-root-node))
             (nodes (nconc (jai-ts--collect-nodes-by-type root "procedure_declaration")
                          (jai-ts--collect-nodes-by-type root "const_declaration")
                          (jai-ts--collect-nodes-by-type root "struct_declaration")
                          (jai-ts--collect-nodes-by-type root "enum_declaration"))))
        (cl-loop for node in nodes
                 for name-node = (or (treesit-node-child-by-field-name node "name")
                                   (treesit-node-child node 0))
                 when (and name-node
                           (string= (treesit-node-text name-node t) identifier))
                 collect (xref-make (format "%s %s in %s"
                                          (pcase (treesit-node-type node)
                                            ("procedure_declaration" "Procedure")
                                            ("const_declaration" "Constant")
                                            ("struct_declaration" "Struct")
                                            ("enum_declaration" "Enum")
                                            (_ "Unknown"))
                                          identifier
                                          (buffer-name buffer))
                                  (xref-make-buffer-location
                                   buffer
                                   (treesit-node-start name-node))))))))

(defun jai-ts--find-definitions (identifier)
  "Find all definitions of IDENTIFIER in project."
  (let ((files (jai-ts--find-all-jai-files))
        (results nil))
    (dolist (file files)
      (when-let ((buffer (find-file-noselect file :nowarn)))
        (setq results (nconc results (jai-ts--find-definitions-in-buffer buffer identifier)))))
    (or results (signal 'xref-no-matches 
                      (list "No definitions found for" identifier)))))

(defun jai-ts-xref-backend () 'jai-ts)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql jai-ts)))
  (jai-ts--symbol-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql jai-ts)) identifier)
  (jai-ts--find-definitions identifier))

(add-hook 'jai-ts-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'jai-ts-xref-backend nil t)))
