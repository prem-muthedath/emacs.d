;; ****************************** prem's emacs.d/init.el*********************************
;; for an example of a well-organized emacs set up,
;; see https://github.com/camdez/emacs.d
;;
;; ------------------------------- packages & environment -------------------------------
(setq debug-on-error t)  ;; debug on error

;; add load path for emacs
;; see https://www.gnu.org/software/emacs/manual/html_node/eintr/Loading-Files.html
;; see https://github.com/camdez/emacs.d/blob/master/init.el
;; see http://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path "~/.emacs.d/lisp")


;; first, initialize all packages from MELPA stable
;; see "how to install packages using ELPA, MELPA"
;; link -- http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)


;; load packages (in init)
;;
;; NOTE:
;; emacs automatically calls package-initialize (whether or not it is in init.el), but only
;; AFTER first loading init.el. but if init.el itself contains code that depends on a call
;; to package-initialize, then you have to EXPLICITLY call package-initialize in init.el.
;;
;; we've such a case here, as the code below for installing packages uses package-installed-p,
;; which uses a list of installed packages that package-initialize fills.
;;
;; /u/ lunaryom @ https://goo.gl/VPt9z6 (stackoverflow) says package-initialize also fills
;; list of installed packages used by package-installed-p, so call package-initialize before
;; package-installed-p
;;
;; for an excellent explanation, see also /u/ tarsius @ 
;; https://goo.gl/MKNBCB (emacs.stackexchange)

(package-initialize)


;; list packages for installation
;; see https://github.com/purcell/color-theme-sanityinc-tomorrow
(defvar my-packages '(paredit
                      exec-path-from-shell
                      magit
                      visual-fill-column
                      haskell-mode
                      flycheck
                      flycheck-haskell
                      solarized-theme
                      color-theme-sanityinc-solarized
                      color-theme-sanityinc-tomorrow
                      zenburn-theme))


;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))


;; set PATH same as shell --> very critical!!
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; fix for "ls does not --dired" OS X error, seen while building imenu index (see below)
;; from /u/ crippledlambda @ https://goo.gl/GE8Y2v (stackoverflow)
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))



;; ----------------------------------- set modes ----------------------------------------
;; to make things modular, we have a seperate file for each mode
;; to load a mode, you just load the file for that mode

;; load emacs-lisp-mode
(load "prem-emacs-lisp-mode")


;; load hasekll-mode
(load "prem-haskell-mode")



;; ---------------------------- emacs editor settings -----------------------------------
;; ----  see http://homepages.inf.ed.ac.uk/s0243221/emacs/
;; --------------------------------------------------------------------------------------
;; highlight current line
(global-hl-line-mode 1)


;; add current column highlighter
;; col-highlight.el is in load-path
;; see https://www.emacswiki.org/emacs/download/col-highlight.el

(require 'col-highlight)

;; PREM hack -- added below piece of code in solarized.el
;; below code sets column highlight face -- the same face solarized.el sets for hl-line

;;;;; PREM: ADDED COLUMN HIGHLIGHT FACE
;;;;; column-highlight-mode
;;     `(col-highlight ((,class (:background ,base02))))
;;     `(col-highlight-face ((,class (:background ,base02))))

;; use M-x column-highlight-mode (C-x c h) to toggle (current) column highlighting
;; to continuously highlight current column, uncomment below line of code  
;;(column-highlight-mode 1)


;; get rid of the ugly scroll bar!!
;; see /u/ GJStein @ https://goo.gl/HJWv69 (emacs.stackexchange)
(scroll-bar-mode -1)


;; see /u/ NikkiA @ https://goo.gl/1gsqsr (stackoverflow)
;; NOTE: visual-fill-column-mode (see below) uses fill-column
(setq-default fill-column 100)


;; enable line wrap -- use visual-line-mode + visual-fill-column-mode
;; what we need:
;;   1. set visual-line-mode globally (i.e., for everything) -- i can't see
;;      a clear plus for visual-line-mode, except that it avoids arrow marks, etc.
;;      but we'll go with it anyway.
;;   2. set visual-fill-column-mode just for prog-mode and text-mode -- for code and
;;      and text, having an automatic visual wrap @ fill-column is needed, but for
;;      other stuff -- such as completion-list-mode, magit, etc -- it becomes pesky
;;
;; first, we'll activate visual-line-mode globally
;; for visual-line-mode, see /u/ jeff spaulding, /u/ JeanPierre @
;; https://emacs.stackexchange.com/questions/19629/word-wrap-line-option-by-default
(global-visual-line-mode t)

;; for visual-fill-column-mode, see:
;;   http://emacshorrors.com/posts/longlines-mode.html
;;   https://github.com/joostkremers/visual-fill-column
;;
;; the usual approach (see emacshorrors.com) is to have visual-fill-column-mode
;; wherever visual-line-mode is active:
;;
;;    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
;;
;; but we need visual-fill-column-mode only for code and text -- how to get this done?
;;    key ideas from:
;;         1. /u/ holt, /u/ lindydancer @ https://goo.gl/Kqg42e (emacs.stackexchange)
;;         2. GREAT introduction to hooks -- see https://goo.gl/YcTvMq (gnu.org)
;;
;;    for general ideas (with code) on how to AUTOMATICALLY DISABLE a GLOBAL minor mode
;;    for a specific major mode, see:
;;         (NOTE: THESE DIDN'T WORK FOR THIS PROBLEM, BUT THEY COULD ELSEWHERE)
;;         1. see /u/ phils, /u/ djangoliv @ https://goo.gl/qVs6gv (emacs.stackexchange)
;;         2. see /u/ phils @ https://goo.gl/9XD7Sp (stackoverflow)
;;
;;    the approach is simple (see "key idea 1"):
;;       -- we first create a customized hook, my-visual-fill-column-mode-hook, to activate
;;          visual-fill-column-mode
;;       -- we then add that hook to just prog-mode-hook and text-mode-hook
;;       -- with this done, visual-fill-column-mode becomes active ONLY for code and text --
;;          and for nothing else
;;
;;    NOTE: you still can turn off visual-fill-column-mode for a specific code/text buffer
;;          -- just run the toggle command: M-x visual-fill-column-mode  (C-x v f)

(defun my-visual-fill-column-mode-hook ()
  "customized hook for enabling visual-fill-column-mode"
  (visual-fill-column-mode 1))

(add-hook 'prog-mode-hook 'my-visual-fill-column-mode-hook)
(add-hook 'text-mode-hook 'my-visual-fill-column-mode-hook)


;; set indent size
(setq standard-indent 2)


;; line-by-line scrolling
(setq scroll-step 1)


;; turn off tab character
(setq-default indent-tabs-mode nil)


;; enable line & column numbering
;; see /u/ Noufal Ibrahim on line numbering @ https://goo.gl/qvAa8G (stackoverflow)
(global-linum-mode t)
(column-number-mode 1)


;; turn off end-of-buffer beep
;; see /u/ phils @ https://goo.gl/CZia6J (stackoverflow)
(setq visible-bell 1)


;; set up show-parenthesis mode
;; see http://emacs-fu.blogspot.in/2009/01/balancing-your-parentheses.html
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'expression)


;; imenu -- lists (only) top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ https://goo.gl/tBFe5z (camdez.com)
;; set up imenu to automatically rescan buffer contents to reflect new jump targets
(setq imenu-auto-rescan t)


;; enable which-function-mode
;; see http://emacsredux.com/blog/2014/04/05/which-function-mode/
(which-function-mode 1)
(setq which-func-unknown "n/a")


;; enable winner mode -- see /u/ phils @ https://goo.gl/nzEY4C (stackoverflow)
;; NOTE: to just unsplit a window, use C-x 0 (see /u/ remi @ same link above)
(winner-mode 1) ;"C-c <left>" and "C-c <right>" undo and re-do window changes.


;; set the layout definition at startup
;; first, either programmaticaly or manually (only once), inhibit the startup screen
;; see https://goo.gl/PDDZu4 (stackoverflow)
;; code from /u/ joshz; exact manual steps from /u/ zack marrapese
;; next, call my-start-up-layout (see below)
;; see /u/ nsukami _ @ https://goo.gl/YQQYdn (emacs.stackexchange)
;; code (i have modified a bit) from /u/ nsukami _
;; buffer-list code (modified) from /u/ trey jackson @ https://goo.gl/KQwV4B (so)
(defun my-startup-layout ()
  "customize windows layout at startup"
  (interactive)
  (setq inhibit-startup-screen t)   ;; inhibit welcome screen
  (delete-other-windows)
  (split-window-horizontally)       ;; -> |
  (find-file "~/.emacs.d/init.el")
  (next-multiframe-window)
  (find-file
   (expand-file-name "~/software-development/code/patience-diff/patience-diff.hs"))
  (split-window-vertically)         ;; -> __
  (next-multiframe-window)
  (switch-to-buffer (list-buffers-noselect)))  ;; buffer list window

;; execute the layout, but only AFTER init!
(add-hook 'after-init-hook (lambda () (my-startup-layout)))



;; -------------------------------- global key bindings ---------------------------------

;; imenu key sequence -- see https://www.emacswiki.org/emacs/ImenuMode
;; for global kbd, see /u/ Bozhidar Batsov @ https://goo.gl/dc59Kq (stackoverflow)
(global-set-key (kbd "C-x C-i") 'imenu)


;; key binding for dired mode default directory
;; see https://www.emacswiki.org/emacs/DiredMode
(global-set-key (kbd "S-<f1>")  ;; shift-<f1>
  (lambda ()
    (interactive)
    (dired "~/software-development/code/haskell-stuff")))


;; key binding to switch to completion buffer
;; for minibuffer completion, you can also use M-v
;; see /u/ keith flower @ https://goo.gl/uHABCw (stackoverflow)
(define-key global-map (kbd "C-x t") 'switch-to-completions)


;; key binding for magit status
;; see https://magit.vc/manual/magit.html#Installing-from-an-Elpa-Archive
(global-set-key (kbd "C-x g") 'magit-status)


;; key binding to browse url -- key idea (modified) from /u/ DoMiNeLa10, /u/ xuhdev @
;; https://emacs.stackexchange.com/questions/29117/how-can-i-open-an-url-file-in-emacs-dired
(global-set-key (kbd "<C-return>") 'browse-url)


;; key binding to toggle visual-fill-column-mode
(global-set-key (kbd "C-x v f") 'visual-fill-column-mode)


;; key binding to toggle column-highlight-mode
(global-set-key (kbd "C-x c h") 'column-highlight-mode)


;; key binding to comment region (active/inactive)
;; for comment/uncomment lines/regions, see https://goo.gl/kvg7Cz (gnu.org)
;; M-; -- GREAT for comment/uncomment line/region (active)
;; M-j -- GREAT for newline + indent + comment -- see https://goo.gl/hevbxM (emacs.redux)
(global-set-key (kbd "C-x c r") 'comment-region)


;; key binding for process list -- see https://goo.gl/Tj92wC (gnu.org)
(global-set-key (kbd "C-x p l") 'list-processes)


;; text alignment -- bind align-regexp to C-x a r
;; see https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code
(global-set-key (kbd "C-x a r") 'align-regexp)



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

;; CUSTOM-SET-VARABLES -- SOME NOTES:
;;  1. visual-fill-column-mode:
;;     -- see https://github.com/joostkremers/visual-fill-column 
;;     -- to avoid mangled (left) fringes:
;;          (visual-fill-column-fringes-outside-margins nil)
;;     -- to avoid problems with vertical window splits, as suggested at github link:
;;          (split-window-preferred-function (quote visual-fill-column-split-window-sensibly))
;;  2. emacs fullscreen at startup:
;;     -- what exactly is emacs fullscreen?
;;          https://goo.gl/NtWEsx (gnu.org) explains what exactly is fullscreen, as well as
;;          difference between (fullscreen . maximized) vs. (fullscreen . fullboth)
;;     -- how to set up emacs fullscreen using custom-set-variables?
;;          see "configuring full screen mode" @ https://www.emacswiki.org/emacs/FullScreen
;;     -- code source:
;;          I  MODIFIED THE CODE FROM THE emacswiki LINK -- CHANGED (fullscreen . maximized) to
;;          (fullscreen . fullboth) AS (fullscreen . maximized) STILL SHOWS THE TITLE BAR
;;     -- other links:
;;          see /u/ antonio @ https://goo.gl/XbmfJL (emacs.stackexchange) for another
;;          way to set fullscreen at start up; see also /u/ scott weldon, /u/ dan at same link

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

;; -----------------------------------------------------------------------------------------
