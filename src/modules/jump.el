(rc/require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq dumb-jump-prefer-searcher 'rg)
(setq dumb-jump-force-searcher 'rg)
(setq dumb-jump-rg-search-args "--pcre2 --type-add \"jai:*.jai\"")
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

