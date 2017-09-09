;; ---------------------------- prem's haskell-mode settings -------------------------------
;; see http://haskell.github.io/haskell-mode/manual/latest/index.html#Top
;; see https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; ghc-mod full details @ http://www.mew.org/~kazu/proj/ghc-mod/en/
;; ghc-mod emacs set-up @ http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
;; note -- no paredit for haskell-mode, as key bindings conflict with ghc-mod
;; -----------------------------------------------------------------------------------------
;; setting path for executables -- cabal, ghc-mod, hdevtools, hoogle, etc
;; NOTE: not needed at the moment, as ~/Library/Haskell/bin is on PATH


;; initialize ghc-mod each time you open a haskell file
;; enable flycheck-mode but ONLY for haskell-mode!
;; NOTE -- to enable flycheck-mode for ALL languages, use instead:
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   see http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (flycheck-mode) (ghc-init)))

;; CHANGE OF HEART!
;; ghc-mod, in my experience, is crap -- it doesn't work most of the time, 
;; and even when it does work, it is erratic at best
;; so for now, we must skip ghc-mod, but we'll add flycheck and flycheck-haskell
;; code source for flycheck-haskell: https://blog.urbanslug.com/posts/2015-04-13-Emacs-setup-for-haskell.html
(add-hook 'haskell-mode-hook (lambda () (flycheck-mode)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; turn on haskell indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; add declaration scan using imenu
;; see "declaration scanning" section in haskell-mode manual
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)


;; enable speedbar support
;; code incomplete in haskell-mode manual "declaration scanning" section
;; code from /u/ janoChen @
;; http://askubuntu.com/questions/23989/cant-see-php-files-in-emacs-speedbar
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".hs"))


;; bind (haskell-mode) haskell-compile to C-c C-o
;; using C-c C-o, as ghc-mod uses C-c C-c
;; ghc -Wall -ferror-spans -fforce-recomp -c
;; see haskell-compile-command () in haskell-compile.el
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))


;; haskell-mode hoogle -- set C-c C-h key binding
;; see https://wiki.haskell.org/Hoogle
;; ghc-mod uses hoogle as default, but i want hoogle in haskell-mode itself
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle))


;; haskell-mode hayoo -- set C-C C-y binding
;; haskell-hoogle.el has haskell-hayoo function for hayoo search
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-y") 'haskell-hayoo))


;; haskell-mode -- set f8 key binding to navigate to imports
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))


;; generate tags for top-level definitions
;; see https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Tags
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find))


;; set haskell-mode alignment rules
;; core code @ https://github.com/haskell/haskell-mode/wiki/Indentation#basic-indentation
;; core code + binding to align command @
;; https://github.com/PierreR/spacemacs/commit/1601aff8f893694f1cc2122d65217f072a2c87d6
(with-eval-after-load 'align
  (add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode)))))

;; for haskell-mode (only), bind align to M-[
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "M-[") 'align))

;; -----------------------------------------------------------------------------------------
