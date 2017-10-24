;; -------------------------------- global key bindings ---------------------------------

;; NOTES:
;;   1.  mapc refactoring idea from camdez @ https://goo.gl/fjv7Jj (github)
;;   2.  dired: shift-<f1> -> see https://www.emacswiki.org/emacs/DiredMode
;;   3.  imenu: see https://www.emacswiki.org/emacs/ImenuMode
;;   4.  switch to completion buffer:
;;         -- see /u/ keith flower @ https://goo.gl/uHABCw (stackoverflow)
;;         -- for minibuffer completion, you can also use M-v
;;   5.  magit status:
;;         -- see https://magit.vc/manual/magit.html#Installing-from-an-Elpa-Archive
;;   6.  browse-url:
;;         -- key idea (modified) from /u/ DoMiNeLa10, /u/ xuhdev @
;;            https://goo.gl/ALqPKs (emacs.stackexchange)
;;   7.  visual-fill-column-mode, column-highlight-mode: toggle commands
;;   8.  comment-region:
;;         -- for comment/uncomment lines/regions, see https://goo.gl/kvg7Cz (gnu.org)
;;         -- M-; -- GREAT for comment/uncomment line/region (active)
;;         -- M-j -- GREAT for newline + indent + comment -- see
;;                   https://goo.gl/hevbxM (emacs.redux)
;;         -- to uncomment region, use C-u C-x c r or M-; -- see /u/ squidly,
;;            /u/ aaron hall @ https://goo.gl/NbLgqs (stackoverflow)
;;   9.  process list: see https://goo.gl/Tj92wC (gnu.org)
;;   10. text alignment: see https://goo.gl/NejP3t (github.com/haskell)
(mapc (lambda (binding)
        (let ((key (car binding))
              (command (cadr binding)))
          (define-key global-map (kbd key) command))
        )
      '(("S-<f1>"          (lambda ()
                             (interactive)
                             (dired (expand-file-name "haskell-stuff" prem/code-dir))))
        ("C-x <return> i"  imenu)
        ("C-x t"           switch-to-completions)
        ("C-x g"           magit-status)
        ("<C-return>"      browse-url)
        ("C-x v f"         visual-fill-column-mode)
        ("C-x c h"         column-highlight-mode)
        ("C-x c r"         comment-region)
        ("C-x p l"         list-processes)
        ("C-x a r"         align-regexp)))

;; --------------------------------------------------------------------------------------
