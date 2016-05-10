;; ****************************** prem's emacs.d/init.el ************************************
;; for an example of a well-organized emacs set up, see https://github.com/camdez/emacs.d
;;
;; ------------------------------- packages & environment ----------------------------------
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
;; package-initialize also fills list of installed packages, used by package-installed-p
;; so /u/ lunaryom says -- call package-initialize before package-installed-p
;; see http://stackoverflow.com/questions/26116882/error-package-el-not-yet-initialized
(package-initialize)


;; list packages for installation
;; see https://github.com/purcell/color-theme-sanityinc-tomorrow
;; ghc-mod -- see http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
;; flycheck hdevtools
(defvar my-packages '(paredit
                      exec-path-from-shell
                      solarized-theme
                      color-theme-sanityinc-tomorrow
                      zenburn-theme
                      ghc
                      haskell-mode
                      flycheck-hdevtools))


;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))


;; set PATH same as shell --> very critical!!
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; fix for "ls does not --dired" OS X error, seen while building imenu index (see below)
;; from /u/ crippledlambda @ http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))



;; ----------------------------------- set modes -------------------------------------------
;; to make things modular, we have a seperate file for each mode
;; to load a mode, you just load the file for that mode

;; load emacs-lisp-mode
(load "prem-emacs-lisp-mode")


;; load hasekll-mode
(load "prem-haskell-mode")



;; ---------------------------- emacs editor settings --------------------------------------
;; ----  see http://homepages.inf.ed.ac.uk/s0243221/emacs/
;; -----------------------------------------------------------------------------------------
;; highlight current line
(global-hl-line-mode 1)


;; set indent size
(setq standard-indent 2)


;; line-by-line scrolling
(setq scroll-step 1)


;; turn off tab character
(setq-default indent-tabs-mode nil)


;; enable line & column numbering
;; see /u/ Noufal Ibrahim on line numbering
;; @ http://stackoverflow.com/questions/2034470/how-do-i-enable-line-numbers-on-the-left-everytime-i-open-emacs
(global-linum-mode t)
(column-number-mode 1)


;; turn off line wrapping
;; see http://www.emacswiki.org/emacs/LineWrap, http://www.emacswiki.org/emacs/TruncateLines
(set-default 'truncate-lines t)


;; turn off end-of-buffer beep
;; see /u/ phils @ http://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)


;; set up show-parenthesis mode
(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;; imenu -- lists (only) top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ http://camdez.com/blog/2013/11/28/emacs-rapid-buffer-navigation-with-imenu/
;; set up imenu to automatically rescan buffer contents to reflect new jump targets
(setq imenu-auto-rescan t)


;; enable which-function-mode
(which-function-mode 1)
(setq which-func-unknown "n/a") ;; see http://emacsredux.com/blog/2014/04/05/which-function-mode/


;; disable erase-buffer (as default)
;; to disable, replaced nil with t in the erase-buffer *enabling* code
;; listed @ http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled t)


;; set the layout definition at startup
;; first, either programmaticaly or manually (only once), inhibit the startup screen
;; see http://stackoverflow.com/questions/744672/unable-to-hide-welcome-screen-in-emacs
;; code from /u/ joshz; exact manual steps from /u/ zack marrapese
;; next, call my-start-up-layout (see below)
;; see http://emacs.stackexchange.com/questions/822/how-to-setup-default-windows-at-startup
;; code (i have modified a bit) from /u/ nsukami _
;; buffer-list code (modified) from /u/ trey jackson @
;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
(defun my-startup-layout ()
  (interactive)
  (setq inhibit-startup-screen t)   ;; inhibit welcome screen
  (delete-other-windows)
  (split-window-horizontally)       ;; -> |
  (find-file "~/.emacs.d/init.el")
  (next-multiframe-window)
  (find-file (expand-file-name "~/software-development/code/haskell-stuff/ghc-mod-test.hs"))
  (split-window-vertically)
  (next-multiframe-window)
  (switch-to-buffer (list-buffers-noselect)))  ;; buffer list window

;; execute the layout, but only AFTER init!
(add-hook 'after-init-hook (lambda () (my-startup-layout)))


;; enable fill column indicator
;; see http://www.emacswiki.org/emacs/FillColumnIndicator
;; used here to see if lines have > 92 chars, which makes reading difficult in emacs
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-column 92)   ;; marker @ column 92


;; text alignment -- bind align-regexp to C-x a r
;; see https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code
(global-set-key (kbd "C-x a r") 'align-regexp)



;; --------------------------------- themes & faces ----------------------------------------
;; in this set up, we load a custom theme, with perhaps a bunch of manual face
;; customizations
;; -----------------------------------------------------------------------------------------

;; --------- loading a custom-theme (solarized-dark, sanityinc-tomorrow-blue, etc.) ---------
;; 1. check if the required custom theme package is listed in the my-packages variable
;;    (see above); if not, add the new custom theme package to the my-packages variable
;; 2. re-load init.el (through a emacs restart)
;; 3. use M-x load-theme RET TAB to see a list of all themes in the emacs
;; 4. in that list, spot themes associated with the custom theme package in step 1
;; 5. get the exact name of the custom theme you want to load from that list
;; 6. see "how to load a custom theme from init.el" section for next steps

;; ----------------------- how to load a custom theme from init.el -------------------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
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
;; -----------------------------------------------------------------------------------------

(load "prem-solarized-dark")


;; ---------------------- face customizations -- general guidelines -----------------------
;; face customizations done through custom-set-faces, usually in the theme file

 ;; see /u/ Harvey, customizing fonts, @ http://emacs.stackexchange.com/questions/2501/how-can-i-set-default-font-in-emacs
 ;; 1. select some code, & type M-x customize-face RET default RET, choose white for
 ;;    foreground color, & click "apply all changes" -> makes general font white
 ;; 2. select some comment, type M-x customize-face RET & choose forground color
 ;;    "light slate grey" & click "apply all changes" -> makes comments "light slate grey"
 ;; 3. you could do 1 & 2 in an another way as well: select some code, then
 ;;    M-x customize-face RET RET, & then choose foreground colors for default font,
 ;;    font-lock-comment-face, & anything else you wish; then click "apply all changes"
 ;;    button at the top
 ;; 4. or if you M-x customize-face RET TAB, emacs will list (in another buffer) all items
 ;;    -- such as font-lock-comment-face, font-lock-function-name, etc -- you can modify.
 ;;    you can click on an item and then hit RET, which will put you on a screen where you
 ;;    can edit & save the item you clicked
 ;; 5. choose highlight line (hl-line) color as follows:
 ;;    M-x customize-face RET hl-line, pick a color, & apply all changes
 ;;    see /u/ juanleon @ http://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
;; -----------------------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen nil)
 '(show-paren-mode t))


;; ---------------------- mode-line which-func display customization ------------------------
;; we customize which-func display for both mode-line active/inactive. why?
;; -- which-func-format defines a single face -- which-func
;; -- emacs, by default, uses this which-func face for display
;; -- but this face doesn't work for both active & inactive mode-line
;; -- a clear face in active mode-line becomes blurred in inactive mode-line
;;
;; so, we need:
;; -- either 2 faces -- one for mode-line active; one for mode-line inactive
;; -- or a single face that appears clear for both active & inactive mode-line
;;
;; first, how do things work at the moment?
;; -- which-func-format variable (in which-func.el) defines the which-func face
;; -- mode-line-format refers (indirectly) to which-func-format
;; -- emacs looks up which-func face (through mode-line-format) from which-func-format
;; -- emacs uses the retrieved face to display which-func in mode-line
;; -- but if look-up fails/=none, emacs wil use the default face for mode-line items
;; -- for mode-line items, emacs uses seperate faces for mode-line active & inactive
;; -- this makes mode-line items clearly visible when mode-line is active or inactive
;;
;; -- references -- ckeck out these links to understand how to customize mode-line
;; --   customizing mode-line -- http://emacs-fu.blogspot.in/2011/08/customizing-mode-line.html
;; --   customizing mode-line -- http://amitp.blogspot.in/2011/08/emacs-custom-mode-line.html
;; --   which-func.el -- http://web.mit.edu/Emacs/source/emacs-23.1/lisp/progmodes/which-func.el
;; --   customizing which-func-format -- http://www.lunaryorn.com/2014/09/13/boosting-which-func-mode.html
;; --   current buffer -- https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
;; --   buffer for selected-window -- /u/ Francesco
;; --   @ http://emacs.stackexchange.com/questions/2959/how-to-know-my-buffers-visible-focused-status
;;
;; solution: original which-func-format, with just face set to mode-line-emphasis (BEST)
;; --    simple: reuses most of the original which-func-format that uses :propertize
;; --    but with face set to mode-line-emphasis, instead of (the original) which-func
;; --    mode-line-emphasis face works well for both active as well as inactive mode-line
(setq-default
 which-func-format
 `("["
   (:propertize which-func-current
                local-map ,which-func-keymap
                face mode-line-emphasis)
   "]"))

;; -----------------------------------------------------------------------------------------

