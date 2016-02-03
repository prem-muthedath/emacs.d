;; this file contains instructions & code for loading themes -- both built-in & custom (from melpa)
;; when you need to load a theme, copy appropriate sections from this file to your init.el

;; ------------------- loading a built-in theme -- such as manoj-dark -------------------
;; 1. use M-x load-theme RET TAB to see a list of all themes in your emacs
;; 2. this list may contain both built-in & custom themes
;; 3. so how do you identify the built-in themes?
;; 4. built-in theme -- any theme that you are not explicitly downloading in your init.el  
;; 5. get the exact name of the built-in theme you want from that list
;; 6. if it doesn't exist already, add below line of code to your init.el;
;;    then, replace manoj-dark, or whatever theme is there in its place, with your built-in theme
(load-theme 'manoj-dark t)

;; -------- loading a custom-theme (solarized-dark, sanityinc-tomorrow-blue, etc.) ------
;; 1. first, make sure that, in your init.el, you are downloading (from melpa)
;;    & installing (locally) the custom theme package you want to load
;; 2. next, load your init.el (through a emacs restart)
;; 3. use M-x load-theme RET TAB to see a list of all themes in your emacs
;; 4. in that list, spot the themes associated with the custom theme package you have downloaded
;; 5. get the exact name of the custom theme you want to load from that list
;; 6. see "how to load a custom theme" section to load this custom theme from your init.el

;; ------------------- solarized tunings  ----------------
;; for solarized, first we need a whole set of tunings (below) for good display
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

;; ------------------- how to load a custom theme ----------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
;; if it doesn't exist already, add the below line of code to your init.el;
;; then, replace solarized-dark, or whatever theme is there in its place, with your custom theme
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))
