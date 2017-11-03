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
;;  -- we use #' here because this marks a symbol as a function for byte-compilation;
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
;;       -- (read "'((\"C-x v2\" . #'magit-status))") gives:
;;
;;              '(("C-x v2" function magit-status)) -- not an alist anymore
;;
;;          instead of:
;;
;;              '(("C-x v2" . (function magit-status)))
;;
;;       -- (eval (read "'((\"C-x v2\" . #'magit-status))")) gives:
;;
;;              (("C-x v2" function magit-status))  -- not an alist anymore
;;
;;          instead of:
;;
;;              (("C-x v2" . (function magit-status)))
;;
;;       -- we end up with an expression that no longer has (function magit-status)
;;       -- the compiler therefore will NOT warn about any missing functions
;;          if you use #' within a quoted nested expression
;;       -- so don't use #' within quoted expressions -- both '(..) and `(..)
;;       -- you can, however, use #' within `(..) if you write ,#'
;;       -- example from /u/ raeburn @ irc #gnu chat:

;;     (defun my-xx (&rest args)
;;       (message "my-xx: %S" args))

;;     (my-xx global-map '(("C-x" . #'foobarbaz)))   ;; -- NO COMPILER WARNINGS

;;     (my-xx global-map `(("C-x" . ,#'foobarbaz)))  ;; -- COMPILER WARNS

(defun map-key-bindings (key-map key-bindings)
  "Defines key-bindings for a key-map. 

The argument \"key-map\" is the (unquoted) map -- for example, global-map -- where 
you want to define the key bindings.
  
The argument \"key-bindings\" is an alist -- a list of cons cells -- each cons cell of
of the form (key . binding).

Example \"key-bindings\": '((key-1 . binding-1) (key-2 . binding-2) ... )"
  (mapc #'(lambda (key-binding)
            (let ((key (car key-binding))
                  (command (cdr key-binding)))
               (define-key key-map (kbd key) command)))
        key-bindings))
;; --------------------------------------------------------------------------------------
