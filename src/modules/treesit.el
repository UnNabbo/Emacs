(require 'treesit)

(setq treesit-language-install-dir (expand-file-name "~/.emacs.d/tree-sitter/"))

(setq treesit-language-source-alist
      '((jai . ("https://github.com/constantitus/tree-sitter-jai"))))
