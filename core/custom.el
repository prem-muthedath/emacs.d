;; ---------------------------- system customizations -----------------------------------
;; custom-set-variables and custom-faces (if any) defined here
;; --------------------------------------------------------------------------------------
;; CUSTOM-SET-VARABLES -- SOME NOTES:
;;  1. visual-fill-column-mode:
;;     -- see https://github.com/joostkremers/visual-fill-column 
;;     -- to avoid mangled (left) fringes:
;;          (visual-fill-column-fringes-outside-margins nil)
;;     -- to avoid problems with vertical window splits, as suggested at github link:
;;          (split-window-preferred-function (quote visual-fill-column-split-window-sensibly))
;;  2. emacs fullscreen at startup:
;;     -- what exactly is emacs fullscreen?
;;          https://goo.gl/NtWEsx (gnu.org) explains fullscreen, as well as difference
;;          between (fullscreen . maximized) vs. (fullscreen . fullboth)
;;     -- how to set up emacs fullscreen using custom-set-variables?
;;          see "configuring full screen mode" @ https://goo.gl/9JeSDN (emacswiki.org)
;;     -- code source:
;;          MODIFIED CODE FROM emacswiki LINK -- CHANGED (fullscreen . maximized) to
;;          (fullscreen . fullboth) AS (fullscreen . maximized) STILL SHOWS TITLE BAR
;;     -- other links:
;;          see /u/ antonio @ https://goo.gl/XbmfJL (emacs.stackexchange) for another
;;          way to do this; see also /u/ scott weldon, /u/ dan at same link

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote auto))
 '(haskell-tags-on-save t)
 '(initial-frame-alist (quote ((fullscreen . fullboth))))
 '(split-window-preferred-function (quote visual-fill-column-split-window-sensibly))
 '(visual-fill-column-fringes-outside-margins nil))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; --------------------------------------------------------------------------------------
