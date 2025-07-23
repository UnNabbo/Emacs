(rc/require 'ivy)
(rc/require 'ripgrep)

(load-relative "projectile.el")
(projectile-mode +1)
(add-to-list 'projectile-globally-ignored-directories "*modules")

(setq projectile-completion-system 'ivy)
;(setq projectile-indexing-method 'native) 
(defvar my/projectile-rg-ignore-modules nil)
