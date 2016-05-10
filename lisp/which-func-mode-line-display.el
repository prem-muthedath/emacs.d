;; ---------------------- mode-line which-func display customization ------------------------
;; we customize which-func display for both mode-line active/inactive. why?
;; -- which-func-format defines a single face -- which-func
;; -- emacs, by default, uses this which-func face for display
;; -- but this face doesn't work for both active & inactive mode-line for some themes
;; -- a clear face in active mode-line becomes blurred in inactive mode-line
;;
;; so, we need (for some themes, but not all):
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
