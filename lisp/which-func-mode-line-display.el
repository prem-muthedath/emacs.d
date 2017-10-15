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
;; --   customizing mode-line -- https://goo.gl/VzhzsD (emacs.fu)
;; --   customizing mode-line -- https://goo.gl/4QXH38 (amitp)
;; --   which-func.el -- https://goo.gl/KBKVx1 (web.mit)
;; --   customizing which-func-format -- https://goo.gl/rEsX78 (lunaryorn.com)
;; --   current buffer -- https://goo.gl/NWqgsB (gnu.org)
;; --   buffer for selected-window -- /u/ Francesco @
;; --   https://goo.gl/6rgAoq (emacs.stackexchange)
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
