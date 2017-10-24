;; ---------------------------- global variables and functions --------------------------
;;  this file contains COMMON variable and function definitions used globally
;;  LOAD THIS FILE FIRST, BEFORE LOADING ANYTHING ELSE, INCLUDING themes.el
;; --------------------------------------------------------------------------------------

;; some useful/common paths
(defvar prem/code-dir (expand-file-name "../software-development/code" prem/emacs-dir)
  "The root directory containing code.")

(defvar prem/haskell-dir (expand-file-name "haskell-stuff" prem/code-dir)
  "The root directory containing haskell projects.")



(defun map-key-bindings (key-map key-bindings)
  "Defines key-bindings for a key-map. 

The argument \"key-map\" is the (unquoted) map -- for example, global-map -- where 
you want to define the key bindings.
  
The argument \"key-bindings\" is a quoted list literal, with each element a list of 
just two items --  a key and its binding. 

Example \"key-bindings\": '((key-1 binding-1) (key-2 binding-2) ... )"
  ;; mapc refactoring code from camdez @ https://goo.gl/fjv7Jj (github)
  (mapc (lambda (key-binding)
          (let ((key (car key-binding))
		 (command (cadr key-binding)))
	     (define-key key-map (kbd key) command)))
        key-bindings))


