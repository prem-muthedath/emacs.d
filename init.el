;; ***************** prem's emacs.d/init.el *******************

;; for an example of a well-organized emacs set up,
;; see https://github.com/camdez/emacs.d
;; ************************************************************
;; first, initialize all packages from below site
;; see "how to install packages using ELPA, MELPA"
;; link -- http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

;; list packages for installation
(defvar my-packages '(paredit		     
                      exec-path-from-shell
                      solarized-theme
                      color-theme-sanityinc-tomorrow        ;; see https://github.com/purcell/color-theme-sanityinc-tomorrow
                      haskell-mode))

;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; set PATH same as shell --> very critical!!
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; fix for "ls does not --dired" OS X error, seen when building imenu index (see below)
;; from /u/ crippledlambda @ http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))


;; --------- emacs-lisp-mode settings---------------------
;; -------------------------------------------------------
;; set paredit mode 
;; syntax from @ https://github.com/camdez/emacs.d/blob/master/core/modes.el
;; note -- camdez link doesn't set paredit for emacs-lisp, a mistake, but for clojure
;; see also paredit auto-activation @ http://www.emacswiki.org/emacs-test/ParEdit
;; tested matching brackets & C-M-f, C-M-b (see http://www.braveclojure.com/basic-emacs/)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; set up imenu for easy function & other top-level definitions search
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)


;; --------- emacs editor settings -----------------------
;; ----  see http://homepages.inf.ed.ac.uk/s0243221/emacs/
;; -------------------------------------------------------                  
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

;; turn off end-of-buffer beep
;; see /u/ phils @ http://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)

;; set up show-parenthesis mode
(show-paren-mode 1)

;; imenu -- lists (only) top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ http://camdez.com/blog/2013/11/28/emacs-rapid-buffer-navigation-with-imenu/
;; set up imenu to automatically rescan buffer contents to reflect new jump targets
(setq imenu-auto-rescan t)

;; disable erase-buffer (as default)
;; to disable, replaced nil with t in the erase-buffer *enabling* code
;; listed @ http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled t)


;; ------------------ themes & faces ---------------------
;; in this set up, we use the default theme that comes with
;; emacs, with a bunch of manual face customizations
;; -------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes t)
 '(show-paren-mode t))

;; see /u/ Harvey, customizing fonts, @ http://emacs.stackexchange.com/questions/2501/how-can-i-set-default-font-in-emacs
;; 1. select some code, & type -- M-x customize-face RET default RET, choose white for foreground color,
;;    & click "apply all changes" -> makes general font white
;; 2. select some comment, type M-x customize-face RET & choose forground color "light slate grey"
;;    & click "apply all changes" -> makes comments "light slate grey"
;; 3. you could do 1 & 2 in an another way as well: select some code, then M-x customize-face RET RET,
;;    & then choose foreground colors for default font, font-lock-comment-face, and anything else you wish.
;;    and then click apply all changes button at the top
;; 4. or if you M-x customize-face RET TAB, emacs will list (in another buffer) all items -- such as
;;    font-lock-comment-face, font-lock-function-name, etc -- you can modify.  you can click on an item
;;    and then hit RET, which will then take you to a screen where you can edit the item you clicked.
;; 5. choose highlight line (hl-line) color as follows: M-x customize-face RET hl-line, pick a color, & apply all changes
;;    see /u/ juanleon @ http://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "cyan4"))))
 '(font-lock-constant-face ((t (:foreground "Magenta"))))
 '(font-lock-keyword-face ((t (:foreground "Blue"))))
 '(font-lock-string-face ((t (:foreground "dodger blue" :width ultra-condensed))))
 '(font-lock-type-face ((t (:foreground "red"))))
 '(font-lock-variable-name-face ((t (:foreground "blue violet")))))
;; --------------------------------------------------------
