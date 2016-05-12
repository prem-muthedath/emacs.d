;; **************************** prem's solarized-dark settings *****************************
;; this file contains tunings and custom-set-faces for solarized-dark theme
;;
;; ---------------------------------- solarized tunings ------------------------------------
;; for solarized, first we need a whole set of tunings (below) for good display;
;; these tunings should be set BEFORE loading solarized theme
;; see https://github.com/bbatsov/solarized-emacs

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; use less bolding
(setq solarized-use-less-bold t)

;; use more italics
(setq solarized-use-more-italic t)

;; use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

;; put the underline below font bottomline, instead of below baseline
(setq x-underline-at-descent-line t)
;; -----------------------------------------------------------------------------------------

;; ----------------------- load solarized theme  -------------------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))
;; -----------------------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "WhiteSmoke"
                         :inverse-video nil :box nil :strike-through nil :overline nil
                         :underline nil :slant normal :weight thin :height 110
                         :width normal :foundry "nil" :family "Menlo"))))
 '(flycheck-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(flycheck-fringe-warning ((t (:background "#002b36" :foreground "yellow" :weight thin))))
 '(flycheck-warning ((t (:underline "yellow" :weight thin))))
 '(font-lock-comment-face ((t (:foreground "chocolate2"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "chocolate2"))))
 '(font-lock-constant-face ((t (:foreground "#268bd2" :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "LightBlue3" :weight thin))))
 '(font-lock-keyword-face ((t (:foreground "green1"))))
 '(font-lock-string-face ((t (:foreground "turquoise"))))
 '(font-lock-type-face ((t (:foreground "DeepSkyBlue1" :weight thin))))
 '(font-lock-variable-name-face ((t (:foreground "khaki"))))
 '(ghc-face-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(ghc-face-hole ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(ghc-face-warn ((t (:underline "yellow" :weight thin))))
 '(haskell-pragma-face ((t (:foreground "DeepSkyBlue1" :weight thin))))
 '(haskell-error-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(haskell-hole-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(haskell-operator-face ((t (:foreground "DarkSeaGreen1"))))
 '(haskell-warning-face ((t (:underline "yellow" :weight thin)))))
;; -----------------------------------------------------------------------------------------
