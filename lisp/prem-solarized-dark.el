;; **************************** prem's solarized-dark settings *****************************
;; this file contains tunings and custom-set-faces for solarized-dark theme
;;
;; ---------------------------------- solarized tunings ------------------------------------
;; for solarized, first we need a some of tunings (below) for good display;

;; NOTE:
;; we had a lot more tunings in the previous edition, when i had a whole
;; bunch of face customizations in custom-set-faces.

;; BUT now i have decided to go with solarized-dark out-of-the-box, except
;; for the default font.  with this change (in this file), i have also eliminated
;  most of the previous tunings, because they didn't offer much.

;; these tunings should be set BEFORE loading solarized theme
;; see https://github.com/bbatsov/solarized-emacs

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; use less bolding
(setq solarized-use-less-bold t)

;; put the underline below font bottomline, instead of below baseline
(setq x-underline-at-descent-line t)
;; -----------------------------------------------------------------------------------------

;; ----------------------- load solarized theme  -------------------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))
;; -----------------------------------------------------------------------------------------

;; i have decided to go with solarized-dark pretty much out-of-the-box
;; i have changed only the default font to Courier
;; i have eliminated all of the previous custom face settings
(custom-set-faces
   '(default ((t (:family "Courier")))))
