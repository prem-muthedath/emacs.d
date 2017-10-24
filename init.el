;; ****************************** prem's emacs.d/init.el*********************************
;; for a well-organized emacs set up, see https://github.com/camdez/emacs.d
;; ------------------------------------------------------ -------------------------------

(setq debug-on-error t)  ;; debug on error


;; define basic paths -- ideas straight from camdez!
(defvar prem/emacs-dir
  (file-name-directory (or load-file-name
                           buffer-file-name))
  "The root directory of my emacs configuration.")

(defvar prem/core-dir (expand-file-name "core" prem/emacs-dir)
  "Directory containing core configuration files.")

(defvar prem/lib-dir (expand-file-name "lib" prem/emacs-dir)
  "Directory containing external emacs libraries.")

(defvar prem/custom-file (expand-file-name "custom.el" prem/core-dir)
  "File containing my custom settings.")


;; set load path for emacs
;; see https://www.gnu.org/software/emacs/manual/html_node/eintr/Loading-Files.html
;; see https://github.com/camdez/emacs.d/blob/master/init.el
;; see http://www.emacswiki.org/emacs/LoadPath
(mapc (lambda (path)
        (add-to-list 'load-path path))
      (list prem/core-dir prem/lib-dir))


;; fix for "ls does not --dired" OS X error, seen while building imenu index
;; code from /u/ crippledlambda @ https://goo.gl/GE8Y2v (stackoverflow)
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))



;; install/load packages:
;; first, initialize all packages from MELPA stable
;;   -- see "how to install packages using ELPA, MELPA" @
;;      http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)


;; next, install/load packages (in init):
;;
;; NOTE:
;; emacs automatically calls package-initialize (whether or not it is in init.el),
;; but only AFTER first loading init.el. but if init.el itself contains code that
;; depends on a call to package-initialize, then you have to EXPLICITLY call
;; package-initialize in init.el.
;;
;; we've such a case here, as the code below for installing packages uses
;; package-installed-p, which uses a list of installed packages that
;; package-initialize fills.
;;
;; /u/ lunaryom @ https://goo.gl/VPt9z6 (stackoverflow) says: "package-initialize
;; also fills list of installed packages used by package-installed-p, so call
;; package-initialize before package-installed-p"
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
;; NOTE: this code should come AFTER install/load packages section; else, emacs
;;       will not find the function (exec-path-from-shell-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; core -- see camdez @ github
;; preserve the load order!!
(load-library "definitions")         ;; global variables, functions -- load FIRST!
(load-library "themes")              ;; look & feel
(load-library "modes")               ;; file editing modes
(load-library "configuration")       ;; basic editor settings; see also custom.el
(load-library "keys")                ;; global key bindings


;; system customizations -- see camdez @ github
(setq custom-file prem/custom-file)
(load custom-file 'no-error)
;; --------------------------------------------------------------------------------------
