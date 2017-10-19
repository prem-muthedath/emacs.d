;; ---------------------------- prem's haskell-mode settings -----------------------------
;; see http://haskell.github.io/haskell-mode/manual/latest/index.html#Top
;; see https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;;
;; for basic sandbox usage (for installing binaries like hasktags, etc), see
;; http://bob.ippoli.to/archives/2013/01/11/getting-started-with-haskell/
;;
;; note -- not using paredit for haskell-mode, as key bindings clashed with ghc-mod
;; ---------------------------------------------------------------------------------------
;; setting path for executables -- cabal, ghc-mod, hdevtools, hoogle, etc
;; NOTE: not needed at the moment, as ~/.local/bin and ~/.cabal/bin are on PATH


;; CHANGE OF HEART!
;; ghc-mod, in my experience, is crap -- it doesn't work most of the time, 
;; and even when it does work, it is erratic at best
;; if you wish to use ghc-mod in future, you should:
;;    - initialize ghc-mod each time you open a haskell file:
;;          (autoload 'ghc-init "ghc" nil t)
;;          (autoload 'ghc-debug "ghc" nil t)
;;    - add ghc-init to haskell-mode-hook:
;;          (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;;
;; for now, we must skip ghc-mod, but we'll add flycheck and flycheck-haskell

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook
          (lambda ()
            ;; enable flycheck-mode but ONLY for haskell-mode!
            ;; NOTE -- to enable flycheck-mode for ALL languages, use instead:
            ;;   (add-hook 'after-init-hook #'global-flycheck-mode)
            ;;   see http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart           
            (flycheck-mode)
            (interactive-haskell-mode)
            (turn-on-haskell-indentation)
            ;; declaration scan using imenu; see haskell-mode manual
            (haskell-decl-scan-mode)))


(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
     ;; ghc -Wall -ferror-spans -fforce-recomp -c
     ;; see haskell-compile-command () in haskell-compile.el
     (define-key haskell-mode-map (kbd "C-c h c") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
     ;; haskell-hoogle.el has haskell-hayoo function for hayoo search
     (define-key haskell-mode-map (kbd "C-c C-y") 'haskell-hayoo)
     (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
     ;; generate tags for top-level definitions -- see haskell-mode manual
     (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
     ;; for haskell-mode (only), bind align to M-[
     (define-key haskell-mode-map (kbd "M-[") 'align)))


;; code source for flycheck-haskell:
;; https://blog.urbanslug.com/posts/2015-04-13-Emacs-setup-for-haskell.html
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;; basic code from /u/ mark @ https://goo.gl/6oK2DQ (stackoverflow)
;; slightly modified to have same structure as flycheck-haskell-setup code above
(eval-after-load 'interactive-haskell-mode
  (add-hook 'interactive-haskell-mode-hook
            (lambda ()  (define-key (current-local-map) (kbd "<tab>") 'dabbrev-expand))))


;; set haskell-mode alignment rules
;; core code @ https://github.com/haskell/haskell-mode/wiki/Indentation#basic-indentation
;; core code + binding to align command @ https://goo.gl/s39tMB (PierreR github) 
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
               (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list    ;; Prem added: $ alignment
             '(haskell-dollar
               (regexp . "\\(\\s-+\\)\\(\\$\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode)))))

;; ------------------------------------------------------------------------------------------
