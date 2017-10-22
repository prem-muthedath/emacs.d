;; ---------------------------- emacs editor settings -----------------------------------
;; ----  see http://homepages.inf.ed.ac.uk/s0243221/emacs/
;; --------------------------------------------------------------------------------------
;; highlight current line
(global-hl-line-mode 1)


;; add current column highlighter
;; col-highlight.el is in load-path
;; see https://www.emacswiki.org/emacs/download/col-highlight.el

(require 'col-highlight)


;; PREM hack -- added below piece of code in solarized.el
;; below code sets column highlight face -- the same face solarized.el sets for hl-line

;;;;; PREM: ADDED COLUMN HIGHLIGHT FACE
;;;;; column-highlight-mode
;;     `(col-highlight ((,class (:background ,base02))))
;;     `(col-highlight-face ((,class (:background ,base02))))

;; use M-x column-highlight-mode (C-x c h) to toggle (current) column highlighting
;; to continuously highlight current column, uncomment below line of code  
;;(column-highlight-mode 1)


;; get rid of the ugly scroll bar!!
;; see /u/ GJStein @ https://goo.gl/HJWv69 (emacs.stackexchange)
(scroll-bar-mode -1)


;; see /u/ NikkiA @ https://goo.gl/1gsqsr (stackoverflow)
;; NOTE: visual-fill-column-mode (see below) uses fill-column
(setq-default fill-column 100)


;; enable line wrap -- use visual-line-mode + visual-fill-column-mode
;; what we need:
;;   1. set visual-line-mode globally (i.e., for everything) -- i can't see
;;      a clear plus for visual-line-mode, except that it avoids arrow marks, etc.
;;      but we'll go with it anyway.
;;   2. set visual-fill-column-mode just for prog-mode and text-mode -- for code and
;;      and text, having an automatic visual wrap @ fill-column is needed, but for
;;      other stuff -- such as completion-list-mode, magit, etc -- it becomes pesky
;;
;; first, we'll activate visual-line-mode globally
;; for visual-line-mode, see /u/ jeff spaulding, /u/ JeanPierre @
;; https://emacs.stackexchange.com/questions/19629/word-wrap-line-option-by-default
(global-visual-line-mode t)

;; for visual-fill-column-mode, see:
;;   http://emacshorrors.com/posts/longlines-mode.html
;;   https://github.com/joostkremers/visual-fill-column
;;
;; the usual approach (see emacshorrors.com) is to have visual-fill-column-mode
;; wherever visual-line-mode is active:
;;
;;    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
;;
;; but we need visual-fill-column-mode only for code and text -- how to get this done?
;;    key ideas from:
;;         1. /u/ holt, /u/ lindydancer @ https://goo.gl/Kqg42e (emacs.stackexchange)
;;         2. GREAT introduction to hooks -- see https://goo.gl/YcTvMq (gnu.org)
;;
;;    for general ideas (with code) on how to AUTOMATICALLY DISABLE a GLOBAL minor mode
;;    for a specific major mode, see:
;;         (NOTE: THESE DIDN'T WORK FOR THIS PROBLEM, BUT THEY COULD ELSEWHERE)
;;         1. see /u/ phils, /u/ djangoliv @ https://goo.gl/qVs6gv (emacs.stackexchange)
;;         2. see /u/ phils @ https://goo.gl/9XD7Sp (stackoverflow)
;;
;;    the approach is simple (see "key idea 1"):
;;       -- we first create a customized hook, my-visual-fill-column-mode-hook, to activate
;;          visual-fill-column-mode
;;       -- we then add that hook to just prog-mode-hook and text-mode-hook
;;       -- with this done, visual-fill-column-mode becomes active ONLY for code and text --
;;          and for nothing else
;;
;;    NOTE: you still can turn off visual-fill-column-mode for a specific code/text buffer
;;          -- just run the toggle command: M-x visual-fill-column-mode  (C-x v f)

(defun my-visual-fill-column-mode-hook ()
  "customized hook for enabling visual-fill-column-mode"
  (visual-fill-column-mode 1))

(add-hook 'prog-mode-hook 'my-visual-fill-column-mode-hook)
(add-hook 'text-mode-hook 'my-visual-fill-column-mode-hook)


;; customize visual-fill-colum-mode for column-highlight-mode
;;   -- visual-fill-column-mode and column-highlight-mode don't go well together
;;   -- when visual-fill-column-mode is active, if we turn on column-highlight-mode,
;;      the left fringe and line numbers disappear/appear randomly during cursor moves
;;   -- to avoid this, whenever column-highlight-mode is on, the below code turns off
;;      visual-fill-column-mode, and vice versa
;;   -- code is my hack, but see also /u/ phils @ https://goo.gl/k5ARFf
;;      (emacs.stackexchange) for other ideas for similiar problems
(add-hook 'column-highlight-mode-hook (lambda ()
                                        (if column-highlight-mode
                                            (visual-fill-column-mode 0)
                                          (my-visual-fill-column-mode-hook))))


;; set indent size
(setq standard-indent 2)


;; line-by-line scrolling
(setq scroll-step 1)


;; turn off tab character
(setq-default indent-tabs-mode nil)


;; enable line & column numbering
;; see /u/ Noufal Ibrahim on line numbering @ https://goo.gl/qvAa8G (stackoverflow)
(global-linum-mode t)
(column-number-mode 1)


;; turn off end-of-buffer beep
;; see /u/ phils @ https://goo.gl/CZia6J (stackoverflow)
(setq visible-bell 1)


;; set up show-parenthesis mode
;; see http://emacs-fu.blogspot.in/2009/01/balancing-your-parentheses.html
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'expression)


;; imenu -- lists (only) top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ https://goo.gl/tBFe5z (camdez.com)
;; set up imenu to automatically rescan buffer contents to reflect new jump targets
(setq imenu-auto-rescan t)


;; enable which-function-mode
;; see http://emacsredux.com/blog/2014/04/05/which-function-mode/
(which-function-mode)
(setq which-func-unknown "n/a")


;; enable winner mode -- see /u/ phils @ https://goo.gl/nzEY4C (stackoverflow)
;; NOTE: to just unsplit a window, use C-x 0 (see /u/ remi @ same link above)
(winner-mode 1) ;"C-c <left>" and "C-c <right>" undo and re-do window changes.


;; set the layout definition at startup
;; first, either programmaticaly or manually (only once), inhibit the startup screen
;; see https://goo.gl/PDDZu4 (stackoverflow)
;; code from /u/ joshz; exact manual steps from /u/ zack marrapese
;; next, call my-start-up-layout (see below)
;; see /u/ nsukami _ @ https://goo.gl/YQQYdn (emacs.stackexchange)
;; code (i have modified a bit) from /u/ nsukami _
;; buffer-list code (modified) from /u/ trey jackson @ https://goo.gl/KQwV4B (so)
(defun my-startup-layout ()
  "customize windows layout at startup"
  (interactive)
  (setq inhibit-startup-screen t)   ;; inhibit welcome screen
  (delete-other-windows)
  (split-window-horizontally)       ;; -> |
  (find-file
   (expand-file-name "init.el" prem/emacs-dir))
  (next-multiframe-window)
  (find-file
   (expand-file-name "patience-diff/patience-diff.hs" prem/code-dir))
  (split-window-vertically)         ;; -> __
  (next-multiframe-window)
  (switch-to-buffer (list-buffers-noselect)))  ;; buffer list window

;; execute the layout, but only AFTER init!
(add-hook 'after-init-hook (lambda () (my-startup-layout)))

;; -----------------------------------------------------------------------------------------
