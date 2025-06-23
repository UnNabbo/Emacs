(load-relative  "dumb-jump.el")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq dumb-jump-prefer-searcher 'rg)
(setq dumb-jump-force-searcher 'rg)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

