;; ---------------------------- global variables and functions --------------------------
;;  this file contains COMMON variable and function definitions used globally
;;  LOAD THIS FILE FIRST, BEFORE LOADING ANYTHING ELSE, INCLUDING themes.el
;; --------------------------------------------------------------------------------------

;; some useful/common paths
(defvar prem/code-dir (expand-file-name "../software-development/code" prem/emacs-dir)
  "The root directory containing code.")

(defvar prem/haskell-dir (expand-file-name "haskell-stuff" prem/code-dir)
  "The root directory containing haskell projects.")


;; NOTES:
;;  -- mapc refactoring code from camdez @ https://goo.gl/fjv7Jj (github)
;;  -- see below for some examples of how to invoke this code
;;  -- we use #' here because this marks a symbol as a function for byte-compilation
;;     the compiler can then warn about missing function definitions etc.,
;;     helping to catch mis-spelled function names, etc., at compile time,
;;     rather than at runtime
;;  -- but init.el is rarely byte-compiled, so you can, if you wish, go
;;     with the cleaner syntax that doesn't use #'

;;     (map-key-bindings global-map '(("C-x v 1" . magit-status)))
;;     (map-key-bindings global-map `(("C-x v 1"  .  ,#'magit-status)))
;;     (map-key-bindings global-map `(,(cons "C-x v 1"  #'magit-status)))
;;     (map-key-bindings global-map (list (cons "C-x v 1"  #'magit-status)))

;;     (my-xx-1 global-map '(("C-x v 2" . #'magit-status))) -- THIS IS USELESS, WHY?
;;       -- compiler will NOT issue any warnings if you use #' within a quoted
;;          nested expression.  instead, you can use #' within a back-quote

;;     (defun my-xx (&rest args)
;;       (message "my-xx: %S" args))

;;     (my-xx global-map '(("C-x" . #'foobarbaz)))   ;; -- NO COMPILER WARNINGS

;;     (my-xx global-map `(("C-x" . ,#'foobarbaz)))  ;; -- COMPILER WARNS
(defun map-key-bindings (key-map key-bindings)
  "Defines key-bindings for a key-map. 

The argument \"key-map\" is the (unquoted) map -- for example, global-map -- where 
you want to define the key bindings.
  
The argument \"key-bindings\" is a list, with each element a cons cell 
--  a key and its binding. 

Example \"key-bindings\": '((key-1 . binding-1) (key-2 . binding-2) ... )"
  (mapc #'(lambda (key-binding)
            (let ((key (car key-binding))
                  (command (cdr key-binding)))
               (define-key key-map (kbd key) command)))
        key-bindings))
;; --------------------------------------------------------------------------------------
