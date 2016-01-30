;; ***************** Prem's .emacs.d/init.el ******************

;; for an excellent way to organize one's emacs set up,
;; see https://github.com/camdez/emacs.d
;; ************************************************************
;; first, initialize all packages from below site
;; see "how to install packages using ELPA, MELPA"
;; link -- http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

;; list the packages for installation
(defvar my-packages '(paredit		     
                      exec-path-from-shell
                      solarized-theme))

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

;; set up show-parenthesis mode
(show-paren-mode 1)

;; set paredit mode 
;; syntax from @ https://github.com/camdez/emacs.d/blob/master/core/modes.el
;; note -- camdez link doesn't set paredit for emacs-lisp, a mistake, but for clojure
;; see also paredit auto-activation @ http://www.emacswiki.org/emacs-test/ParEdit
;; tested matching brackets & C-M-f, C-M-b (see http://www.braveclojure.com/basic-emacs/)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; set up imenu for easy function search --> an  emacs feature
;; note -- imenu only lists top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ http://camdez.com/blog/2013/11/28/emacs-rapid-buffer-navigation-with-imenu/ 
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

;; ----  prem's emac editor settings ---------------------
;; ---   see http://homepages.inf.ed.ac.uk/s0243221/emacs/ 
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
;; see /u/phils @ http://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)

;; load custom theme
;;(load-theme 'manoj-dark t) -- we don't use this anymore; instead we use solarized theme from melpa
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(show-paren-mode t))

;; see /u/ Harvey, customizing fonts, @ http://emacs.stackexchange.com/questions/2501/how-can-i-set-default-font-in-emacs
;; 1. select some code, & type -- M-x customize-face RET default RET, choose white for foreground color,
;;    & click "apply all changes" -> makes general font white
;; 2. select some comment, type M-x customize-face RET & choose forground color "light slate grey"
;;    & click "apply all changes" -> makes comments "light slate grey"
;; 3. you could do 1 & 2 in an another way as well: select some code, then M-x customize-face RET RET,
;;    & then choose foreground colors for default font, font-lock-comment-face, and anything else you wish.
;;    and then click apply all changes button at the top
;; 4. choose highlight line (hl-line) color as follows: M-x customize-face RET hl-line, pick a color, & apply all changes
;;    see /u/ juanleon @ http://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "gray85" :inverse-video nil :box nil
                         :strike-through nil :overline nil :underline nil :slant normal :weight normal
                         :height 120 :width normal :foundry "nil" :family "Menlo"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "light slate gray" :slant normal))))
 '(font-lock-comment-face ((t (:foreground "light slate gray"))))
 '(hl-line ((t (:background "dark slate gray")))))
;; --------------------------------------------------------
(put 'erase-buffer 'disabled t)
