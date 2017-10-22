;; --------------------------------- themes & faces -------------------------------------
;; in this set up, we load a custom theme, with perhaps a bunch of manual face
;; customizations
;; --------------------------------------------------------------------------------------

;; --------- loading a custom-theme (solarized-dark, sanityinc-tomorrow-blue, etc.) -----
;; 1. check if the required custom theme package is listed in the my-packages variable
;;    (see above); if not, add the new custom theme package to the my-packages variable
;; 2. re-load init.el (through a emacs restart)
;; 3. use M-x load-theme RET TAB to see a list of all themes in the emacs
;; 4. in that list, spot themes associated with the custom theme package in step 1
;; 5. get the exact name of the custom theme you want to load from that list
;; 6. see "how to load a custom theme from init.el" section for next steps

;; ----------------------- how to load a custom theme from init.el ----------------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ https://goo.gl/GNgq7r (stackoverflow)
;; see fix from /u/ Xinan @ https://goo.gl/2VAzTU (emacs.stackexchange)
;; (a) to be modular, over here, we create a seperate theme file for each custom theme
;; (b) each theme file has code to load the custom theme + any manual face customizations
;; (c) to load a custom theme, you just load your theme file from init.el
;; (d) here are the steps to do all of that:
;;     1. if it doesn't exist already, add the below line of code to your theme file:

;;            (add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))

;;     2. in that line of code, replace the existing theme -- solarized-dark or
;;        whatever theme is there in its place -- with the your custom theme
;;     3. next, in your init.el, have the following line of code to load your theme file:

;;            (load "your-theme-file-name")

;;     4. in that line of code, replace the existing theme file name with
;;        your theme file name
;;     5. save the theme file & init.el; restart emacs -- the new theme
;;        should now be in effect
;; --------------------------------------------------------------------------------------

(load "prem-solarized-dark")


;; ---------------------- face customizations -- general guidelines ---------------------
;; face customizations done through custom-set-faces, usually in the theme file

;; see /u/ Harvey, customizing fonts, @ https://goo.gl/46CP6b (emacs.stackexchange)
;;  1. select some code, & type M-x customize-face RET default RET, choose white for
;;     foreground color, & click "apply all changes" -> makes general font white
;;  2. select some comment, type M-x customize-face RET & choose forground color
;;    "light slate grey" & click "apply all changes" -> makes comments "light slate grey"
;;  3. you could do 1 & 2 in an another way as well: select some code, then
;;     M-x customize-face RET RET, & then choose foreground colors for default font,
;;     font-lock-comment-face, & anything else you wish; then click "apply all changes"
;;     button at the top
;;  4. or if you M-x customize-face RET TAB, emacs will list (in another buffer) all
;;     items -- such as font-lock-comment-face, font-lock-function-name, etc -- you can
;;     modify. you can click on an item and then hit RET, which will put you on a screen
;;     where you can edit & save the item you clicked
;;  5. choose highlight line (hl-line) color as follows:
;;     M-x customize-face RET hl-line, pick a color, & apply all changes
;;     see /u/ juanleon @ https://goo.gl/ADz6Ni (stackoverflow)
;; -------------------------------------------------------------------------------------
