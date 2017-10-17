;; ---------------------------- prem's emacs-lisp-mode settings --------------------------
;; set paredit mode
;; syntax from @ https://github.com/camdez/emacs.d/blob/master/core/modes.el
;; note -- camdez sets paredit for clojure but not for emacs-lisp (a miss?)
;; see paredit auto-activation @ http://www.emacswiki.org/emacs-test/ParEdit
;; tested matching brackets & C-M-f, C-M-b (see http://www.braveclojure.com/basic-emacs/)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; both paredit RET-indent and scratch buffer lisp evaluation use C-j key binding
;; to avoid conflict, replace C-j for paredit with a key of your choice
;; see /u/ kyle meyer (NOTE: syntax, however, is INCOMPLETE!) @ https://goo.gl/FpWGEx
;; for the exact syntax, see /u/ joon @ https://goo.gl/s3wBuu
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-j") nil)
     (define-key paredit-mode-map (kbd "C-l") 'paredit-newline)))

;; set up imenu for easy function & other top-level definitions search
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)

;; ---------------------------------------------------------------------------------------
