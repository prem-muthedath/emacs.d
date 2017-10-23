;; -------------------------------- global key bindings ---------------------------------

;; key binding for dired mode default directory
;; see https://www.emacswiki.org/emacs/DiredMode
(global-set-key (kbd "S-<f1>")  ;; shift-<f1>
  (lambda ()
    (interactive)
    (dired "~/software-development/code/haskell-stuff")))


;; key binding to switch to completion buffer
;; for minibuffer completion, you can also use M-v
;; see /u/ keith flower @ https://goo.gl/uHABCw (stackoverflow)
(define-key global-map (kbd "C-x t") 'switch-to-completions)


;; key binding for magit status
;; see https://magit.vc/manual/magit.html#Installing-from-an-Elpa-Archive
(global-set-key (kbd "C-x g") 'magit-status)


;; key binding to browse url -- key idea (modified) from /u/ DoMiNeLa10, /u/ xuhdev @
;; https://goo.gl/ALqPKs (emacs.stackexchange)
(global-set-key (kbd "<C-return>") 'browse-url)


;; key binding to toggle visual-fill-column-mode
(global-set-key (kbd "C-x v f") 'visual-fill-column-mode)


;; key binding to toggle column-highlight-mode
(global-set-key (kbd "C-x c h") 'column-highlight-mode)


;; key binding to comment region (active/inactive)
;; for comment/uncomment lines/regions, see https://goo.gl/kvg7Cz (gnu.org)
;; M-; -- GREAT for comment/uncomment line/region (active)
;; M-j -- GREAT for newline + indent + comment -- see https://goo.gl/hevbxM (emacs.redux)
(global-set-key (kbd "C-x c r") 'comment-region)


;; key binding for process list -- see https://goo.gl/Tj92wC (gnu.org)
(global-set-key (kbd "C-x p l") 'list-processes)


;; text alignment -- bind align-regexp to C-x a r
;; see https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code
(global-set-key (kbd "C-x a r") 'align-regexp)

;; --------------------------------------------------------------------------------------
