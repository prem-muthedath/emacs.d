;; ---------------------------- prem's emacs-lisp-mode settings ----------------------------
;; set paredit mode
;; syntax from @ https://github.com/camdez/emacs.d/blob/master/core/modes.el
;; note -- camdez sets paredit for clojure but not for emacs-lisp (a miss?)
;; see paredit auto-activation @ http://www.emacswiki.org/emacs-test/ParEdit
;; tested matching brackets & C-M-f, C-M-b (see http://www.braveclojure.com/basic-emacs/)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)


;; set up imenu for easy function & other top-level definitions search
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)

;; -----------------------------------------------------------------------------------------
