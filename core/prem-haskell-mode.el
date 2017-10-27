;; ---------------------------- prem's haskell-mode settings ----------------------------
;; see http://haskell.github.io/haskell-mode/manual/latest/index.html#Top
;; see https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;;
;; for basic sandbox usage (for installing binaries like hasktags, etc), see
;; http://bob.ippoli.to/archives/2013/01/11/getting-started-with-haskell/
;;
;; note -- not using paredit for haskell-mode, as key bindings clashed with ghc-mod
;; --------------------------------------------------------------------------------------
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


;; set up hooks for haskell-mode:
;; -- no longer using lambda () for hooks because old values remain after editing
;; -- plus, to avoid toggle effects, now passing explicit argument to mode functions
;; -- see /u/ lindydancer, /u/ phils @ https://goo.gl/gsY7fp for more details
(defun my-haskell-mode-hook-function ()
  "Customized hook function for haskell-mode."
  ;; enable flycheck-mode but ONLY for haskell-mode!
  ;; NOTE -- to enable flycheck-mode for ALL languages, use instead:
  ;;   (add-hook 'after-init-hook #'global-flycheck-mode)
  ;;   see http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart
  (flycheck-mode +1)
  (interactive-haskell-mode +1)
  (haskell-indentation-mode +1)
  ;; declaration scan using imenu; see haskell-mode manual
  (haskell-decl-scan-mode +1))

(add-hook 'haskell-mode-hook #'my-haskell-mode-hook-function)


;; set haskell-mode key bindings
;; NOTES:
;;   1. haskell-compile:
;;        -- ghc -Wall -ferror-spans -fforce-recomp -c
;;        -- see haskell-compile-command () in haskell-compile.el
;;   2. hayoo search -- see haskell-hayoo () in haskell-hoogle.el
(with-eval-after-load "haskell-mode"
  (map-key-bindings haskell-mode-map
                    '(("C-c C-l"  haskell-process-load-or-reload)
                      ("C-`"      haskell-interactive-bring)
                      ("C-c C-t"  haskell-process-do-type)
                      ("C-c C-i"  haskell-process-do-info)
                      ("C-c C-c"  haskell-process-cabal-build)
                      ("C-c C-k"  haskell-interactive-mode-clear)
                      ("C-c c"    haskell-process-cabal)
                      ("C-c h c"  haskell-compile)
                      ("C-c C-h"  haskell-hoogle)
                      ("C-c C-y"  haskell-hayoo)
                      ("<f8>"     haskell-navigate-imports)
                      ("M-."      haskell-mode-jump-to-def-or-tag)
                      ("M-["      align))))


;; set flycheck hooks -- code source for flycheck-haskell:
;; https://blog.urbanslug.com/posts/2015-04-13-Emacs-setup-for-haskell.html
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;; set interactive-haskell-mode hooks
;; basic code (modified) from /u/ mark @ https://goo.gl/6oK2DQ (stackoverflow)
;; NOTE:
;;   1. interactive-haskell-mode -- a minor mode -- is defined in haskell.el
;;   2. normally, we expect a mode to be defined in a file named after the mode
;;   3. but this is not the case with interactive-haskell-mode
;;   4. in fact, there is no file named interactive-haskell-mode.el
;;   5. because of this, if we write code such as:
;;
;;        (with-eval-after-load "interactive-haskell-mode"
;;          (add-hook 'interactive-haskell-mode-hook
;;                    #'my-interactive-haskell-mode-hooks))
;;
;;      emacs will not find any file called interactive-haskell-mode.el,
;;      (and emacs will not complain either!!) and therefore will altogether skip
;;      executing rest of the code under the with-eval-after-load block. the code
;;      therefore will never set the intended key binding
;;   6. to fix the issue, i used C-h f interactive-haskell-mode to get the mode's
;;      source file name, and then changed the code to:
;;
;;        (with-eval-after-load "haskell"
;;          (add-hook 'interactive-haskell-mode-hook
;;                    #'my-interactive-haskell-mode-hooks))
;;
;;   7. see /u/ legoscia @ https://goo.gl/4cjpq2 for other great ways (w/o hooks)
;;      to define key-bindings using with-eval-after-load. he also mentions use of
;;      C-h f mode-name to get the module name (which i had found earlier -- see 6)
;;   8. people have faced problems setting key bindings with eval-after-load,
;;      but i've not seen the expalanation outlined here -- perhaps their issues
;;      are > complex than just an incorrect file name.  btw, people've given
;;      various reasons (NOTE -- i've not tested these):
;;        -- see /u/ michael heerdagen @ https://goo.gl/h4RsQU (google groups)
;;        -- see /u/ balu, /u/ sds, /u/ sanityinc @ https://goo.gl/BCMY1p (so)
;;   9. using with-eval-after-load, or eval-after-load, to define
;;      key bindings etc. has 1 plus: code executed only once (see /u/ sanityinc)
;;   10.but if for some reason, with-eval-after-load doesn't work, you can
;;      use hooks to get the job done, but hooks execute once for every buffer
;;      the mode is enabled -- hooks're per-buffer configuration --
;;      see /u/ sanityinc.  here's the hook-based soln for the problem here:
;;
;;           (add-hook 'interactive-haskell-mode-hook
;;                     #'my-interactive-haskell-mode-hooks)
;;
;;   11.see /u/ jesse @ https://goo.gl/4cjpq2 on using hooks to set key-bindings
;;   12.for execution order of eval-after-load and hooks, see /u/ phils @ 
;;      https://goo.gl/35HF5A (stackoverflow) for an excellent explanation

(defun my-interactive-haskell-mode-hook-function ()
  "Customized hook function for interactive-haskell-mode."
  (define-key (current-local-map) (kbd "<tab>") #'dabbrev-expand))


(with-eval-after-load "haskell"  ;; interactive-haskell-mode defined in haskell.el
  (add-hook 'interactive-haskell-mode-hook
            #'my-interactive-haskell-mode-hook-function))



;; set haskell-mode alignment rules
;; core code @ https://goo.gl/NejP3t (github.com/haskell)
;; core code + binding to align command @ https://goo.gl/s39tMB (PierreR github)
;; mapc refactoring idea from camdez @ github
(with-eval-after-load 'align
  (mapc (lambda (rule)
          (add-to-list 'align-rules-list rule))
        '((haskell-types
           (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode)))
          (haskell-assignment
           (regexp . "\\(\\s-+\\)=\\s-+")
           (modes quote (haskell-mode literate-haskell-mode)))
          (haskell-arrows
           (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode)))
          (haskell-left-arrows
           (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode)))
          (haskell-dollar
           (regexp . "\\(\\s-+\\)\\(\\$\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode))))))

;; --------------------------------------------------------------------------------------

