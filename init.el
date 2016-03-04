;; ****************************** prem's emacs.d/init.el ************************************
;; for an example of a well-organized emacs set up, see https://github.com/camdez/emacs.d
;;
;; ------------------------------- packages & environment ----------------------------------
(setq debug-on-error t)  ;; debug on error


;; first, initialize all packages from MELPA stable
;; see "how to install packages using ELPA, MELPA"
;; link -- http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; load packages (in init)
;; package-initialize also fills list of installed packages, used by package-installed-p
;; so /u/ lunaryom says -- call package-initialize before package-installed-p
;; see http://stackoverflow.com/questions/26116882/error-package-el-not-yet-initialized
(package-initialize)


;; list packages for installation
;; see https://github.com/purcell/color-theme-sanityinc-tomorrow
;; ghc-mod -- see http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
;; flycheck hdevtools
(defvar my-packages '(paredit
                      exec-path-from-shell
                      solarized-theme
                      color-theme-sanityinc-tomorrow
                      haskell-mode
                      ghc
                      flycheck-hdevtools
                      zenburn-theme))


;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))


;; set PATH same as shell --> very critical!!
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; add load path for emacs
;; see http://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path "~/.emacs.d/lisp/")


;; fix for "ls does not --dired" OS X error, seen while building imenu index (see below)
;; from /u/ crippledlambda @ http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))



;; ------------------------------ emacs-lisp-mode settings ---------------------------------
;; set paredit mode
;; syntax from @ https://github.com/camdez/emacs.d/blob/master/core/modes.el
;; note -- camdez sets paredit for clojure but not for emacs-lisp (a miss?)
;; see paredit auto-activation @ http://www.emacswiki.org/emacs-test/ParEdit
;; tested matching brackets & C-M-f, C-M-b (see http://www.braveclojure.com/basic-emacs/)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)


;; set up imenu for easy function & other top-level definitions search
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)



;; ------------------------------ haskell-mode settings ------------------------------------
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
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (flycheck-mode) (ghc-init)))


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



;; ---------------------------- emacs editor settings --------------------------------------
;; ----  see http://homepages.inf.ed.ac.uk/s0243221/emacs/
;; -----------------------------------------------------------------------------------------
;; highlight current line
(global-hl-line-mode 1)


;; set indent size
(setq standard-indent 2)


;; line-by-line scrolling
(setq scroll-step 1)


;; turn off tab character
(setq-default indent-tabs-mode nil)


;; enable line & column numbering
;; see /u/ Noufal Ibrahim on line numbering
;; @ http://stackoverflow.com/questions/2034470/how-do-i-enable-line-numbers-on-the-left-everytime-i-open-emacs
(global-linum-mode t)
(column-number-mode 1)


;; turn off line wrapping
;; see http://www.emacswiki.org/emacs/LineWrap, http://www.emacswiki.org/emacs/TruncateLines
(set-default 'truncate-lines t)


;; turn off end-of-buffer beep
;; see /u/ phils @ http://stackoverflow.com/questions/10545437/how-to-disable-the-beep-in-emacs-on-windows
(setq visible-bell 1)


;; set up show-parenthesis mode
(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;; imenu -- lists (only) top-level definitions -- defun, defvar, etc.
;; see M-x imenu usage @ http://camdez.com/blog/2013/11/28/emacs-rapid-buffer-navigation-with-imenu/
;; set up imenu to automatically rescan buffer contents to reflect new jump targets
(setq imenu-auto-rescan t)


;; enable which-function-mode
(which-function-mode 1)
(setq which-func-unknown "n/a") ;; see http://emacsredux.com/blog/2014/04/05/which-function-mode/


;; disable erase-buffer (as default)
;; to disable, replaced nil with t in the erase-buffer *enabling* code
;; listed @ http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled t)


;; set the layout definition at startup
;; first, either programmaticaly or manually (only once), inhibit the startup screen
;; see http://stackoverflow.com/questions/744672/unable-to-hide-welcome-screen-in-emacs
;; code from /u/ joshz; exact manual steps from /u/ zack marrapese
;; next, call my-start-up-layout (see below)
;; see http://emacs.stackexchange.com/questions/822/how-to-setup-default-windows-at-startup
;; code (i have modified a bit) from /u/ nsukami _
;; buffer-list code (modified) from /u/ trey jackson @
;; http://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior
(defun my-startup-layout ()
  (interactive)
  (setq inhibit-startup-screen t)   ;; inhibit welcome screen
  (delete-other-windows)
  (split-window-horizontally)       ;; -> |
  (find-file "~/.emacs.d/init.el")
  (next-multiframe-window)
  (find-file (expand-file-name "~/software-development/code/haskell-stuff/ghc-mod-test.hs"))
  (split-window-vertically)
  (next-multiframe-window)
  (switch-to-buffer (list-buffers-noselect)))  ;; buffer list window

;; execute the layout, but only AFTER init!
(add-hook 'after-init-hook (lambda () (my-startup-layout)))


;; enable fill column indicator
;; see http://www.emacswiki.org/emacs/FillColumnIndicator
;; used here to see if lines have > 92 chars, which makes reading difficult in emacs
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-column 92)   ;; marker @ column 92


;; text alignment -- bind align-regexp to C-x a r
;; see https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code
(global-set-key (kbd "C-x a r") 'align-regexp)



;; --------------------------------- themes & faces ----------------------------------------
;; in this set up, we use the custom theme solarized-dark, with a bunch of manual face
;; customizations
;; -----------------------------------------------------------------------------------------

;; --------- loading a custom-theme (solarized-dark, sanityinc-tomorrow-blue, etc.) ---------
;; 1. check if the required custom theme package is listed in the my-packages variable
;;    (see above); if not, add the new custom theme package to the my-packages variable
;; 2. re-load init.el (through a emacs restart)
;; 3. use M-x load-theme RET TAB to see a list of all themes in the emacs
;; 4. in that list, spot themes associated with the custom theme package in step 1
;; 5. get the exact name of the custom theme you want to load from that list
;; 6. see "how to load a custom theme from init.el" section for next steps

;; ---------------------------------- solarized tunings ------------------------------------
;; for solarized, first we need a whole set of tunings (below) for good display;
;; these tunings should be set BEFORE loading solarized theme
;; see https://github.com/bbatsov/solarized-emacs

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; use less bolding
(setq solarized-use-less-bold t)

;; use more italics
(setq solarized-use-more-italic t)

;; use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

;; put the underline below font bottomline, instead of below baseline
(setq x-underline-at-descent-line t)
;; -----------------------------------------------------------------------------------------

;; ----------------------- how to load a custom theme from init.el -------------------------
;; loading a custom color theme is tricky --
;; see issue from /u/ Ryan @ http://stackoverflow.com/questions/15555309/emacs-for-windows-error-loading-color-theme
;; see fix from /u/ Xinan @ http://emacs.stackexchange.com/questions/2797/emacs-wont-load-theme-on-startup
;; (a) if it doesn't exist already, add the below line of code to init.el
;; (b) in that line of code, replace the existing theme -- solarized-dark or whatever theme
;;     is there in its place -- with the new custom theme
;; (c) save the init.el and restart emacs -- the new theme should now be in effect
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark t)))
;; -----------------------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen nil)
 '(show-paren-mode t))


;; ---------------------- mode-line which-func display customization ------------------------
;; we customize which-func display for both mode-line active/inactive. why?
;; -- which-func-format defines a single face -- which-func
;; -- emacs, by default, uses this which-func face for display
;; -- but this face doesn't work for both active & inactive mode-line
;; -- a clear face in active mode-line becomes blurred in inactive mode-line
;;
;; so, we need:
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
;; so how do we solve this problem?
;; -- broadly, we have 3 solutions
;; -- all solutions require us to customize whch-func-format
;; -- references -- solutions use ideas/code from these links, so take a look at them
;; --   customizing mode-line -- http://emacs-fu.blogspot.in/2011/08/customizing-mode-line.html
;; --   customizing mode-line -- http://amitp.blogspot.in/2011/08/emacs-custom-mode-line.html
;; --   which-func.el -- http://web.mit.edu/Emacs/source/emacs-23.1/lisp/progmodes/which-func.el
;; --   customizing which-func-format -- http://www.lunaryorn.com/2014/09/13/boosting-which-func-mode.html
;; --   current buffer -- https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
;; --   buffer for selected-window -- /u/ Francesco
;; --   @ http://emacs.stackexchange.com/questions/2959/how-to-know-my-buffers-visible-focused-status
;;
;;
;; solution 1: which-func-format without which-func face (WORKS BUT NOT FULLY)
;; -- this will force emacs to display which-func with its default face for mode-line
;; -- +: works!! which-func nicely displayed in both mode-line active & inactive
;; -- -: can't make which-func bold -- bold makes which-func clear when mode-line active 
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize which-func-current
;;                 local-map ,which-func-keymap)
;;    "]"))
;;
;;
;; solution 2: which-func-format with 2 faces -- 1 for mode-line active & 1 for inactive
;; -- this solution has a number of variants, including some that don't work
;; -- however, i have listed them all, so one can know how we got to pick the best
;;
;;
;; solution 2A: add an if-condition with :eval for face value (ERRORS)
;; -- :eval requires that we have a quote in front of faces
;; -- +: correctly & clearly displays function name
;; -- -:
;; --    but emacs doen't pick up the specified face -- instead, it uses the default
;; --    returns "invalid face attribute :eval ..."
;; --    REASON -- within :propertize, face needs an actual face, not an elisp expression
;; --              only :propertize's 1st argument -- a string -- can have (:eval ...)
;; --              SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize which-func-current
;;                 local-map ,which-func-keymap
;;                 face (:eval (if (eq (current-buffer)(window-buffer))
;;                                 'mode-line-emphasis
;;                               'mode-line-inactive)))
;;    "]"))
;;
;;
;; solution 2B: same as 2A but without :eval and with actual faces (ERRORS)
;; -- +: gets the correct name & displays clearly when mode-line inactive
;; -- -:
;; --    but which-func face & background MISMATCH with rest of the mode-line active
;; --    emacs seems to use mode-line-inactive face for both active & inactive
;; --    unsure from where emacs gets the face, but certainly not from the if-expression
;; --    emacs throws "invalid face reference: window-buffer, current-buffer, eq, if"
;; --    this error implies that emacs doesn't recognize the if-expression
;; --    REASON -- within :propertize, face needs an actual face, not an elisp expression
;; --              only :propertize's 1st argument -- a string -- can have (:eval ...)
;; --              SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize which-func-current
;;                 local-map ,which-func-keymap
;;                 face (if (eq (current-buffer)(window-buffer))
;;                          mode-line-emphasis
;;                        mode-line-inactive))
;;    "]"))
;;
;;
;; solution 2C: same as 2B but with face keyword written as 'face (WORKS BUT INCORRECT)
;; -- NOTE: as we have 'face, the face values within if-condition should begin with quote
;; -- no real reason why i did this, except to see what happens (hackery!)
;; --  + : emacs throws no errors -- it displays the function name in the modeline
;; --  -:
;; --     but emacs doesn't pick the face from the if-condition
;; --     instead, it uses the same face it uses for other items in the modeline
;; --     REASON -- in :propertize, 'face is wrong -- it should be face
;; --               we have 'face instead of face, so emacs doesn't find the face keyword
;; --               since the if-condition is associated with 'face, we get no errors
;; --               emacs ends up using its default face for mode-line items
;; --               SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; --     default face fine for both active & inactive, but can't make which-func bold
;; --     so we get the same drawback as solution 2A
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize which-func-current
;;                 'local-map ,which-func-keymap
;;                 'face (if (eq (current-buffer)(window-buffer))
;;                          'mode-line-emphasis
;;                        'mode-line-inactive))
;;    "]"))
;;
;;
;; solution 2D: ~ solution 2C, but which-func-current inlined (without :eval)  (USELESS)
;; -- an experiment to see what happens in :propertize if we inlined which-func-current
;; -- +: throws no errors (but it should have)!!
;; -- -: function name in mode-line blank, so this solution is useless
;; --    REASON: 2 problems, esentialy due to improper arguments in :propetize
;; --            SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; --            (1) 1st arg to :propertize is either a string or (:eval (elisp expression)) 
;; --              here, :propertize needs (:eval ...) to evaluate the inlined expression
;; --              without (:eval ...) the inlined expression is unevaluated
;; --              therefore, we do not get the function name
;; --              but i am not sure why emacs didn't throw an error
;; --            (2) another problem -- in :propertize, 'face is wrong -- it should be face
;; --              see emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; --              since we have 'face, ~ solution 2C, emacs doesn't find the face keyword 
;; --              so emacs ends up using the default face for mode-line
;; --              emacs skips the if-condition (associated with 'face), so we get no errors
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize (replace-regexp-in-string "%" "%%"
;;                                           (or
;;                                            (gethash
;;                                             (selected-window) which-func-table)
;;                                            which-func-unknown))
;;                 'local-map ,which-func-keymap
;;                 'face (if (eq (current-buffer)(window-buffer))
;;                          mode-line-emphasis
;;                        mode-line-inactive))
;;    "]"))


;; solution 2E: ~ solution 2D, but with a function as 1st argument to :propertize (ERRORS)
;; -- again, an experiment to see what happens in :propertize if the 1st arg is a function
;; -- also, replaced 'face in solution 2D with face
;; -- +: the function get-current() gets called, and emacs gets the function name
;; --    NOTE: without the :eval, get-current() never gets called
;; --    REASON: 1st arg to :propertize is either a string or (:eval (elisp expression))
;; --    SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; --    NOTE also that we've 2 :eval here --
;; --         1st :eval calls get-current(), which returns which-func-current
;; --         2nd :eval, inside which-func-current, evaluates the expression
;; --             returned from which-func-current
;; -- -: which-func face & background MISMATCH with rest of the mode-line active
;; --    emacs seems to use mode-line-inactive face for both active & inactive
;; --    unsure from where emacs gets the face, but certainly not from the if-expression
;; --    throws "invalid face reference: window-buffer, current-buffer, eq, if"
;; --    REASON -- within :propertize, face needs an actual face, not an elisp expression
;; --              only :propertize's 1st argument -- a string -- can have (:eval ...)
;; --              SEE emacs-fu.blogspot, amitp.blogspot,lunaryon links in references
;; (defun get-current ()
;;   which-func-current)

;; (setq-default
;;  which-func-format
;;  `("["
;;    (:propertize (:eval (get-current))  ;; without :eval, get-current() never gets called
;;                 local-map ,which-func-keymap
;;                 face (if (eq (current-buffer)(window-buffer))
;;                          mode-line-emphasis
;;                        mode-line-inactive))
;;    "]"))


;; solution 2F: ~ solution 2E, but replace :propertize with propertize (WORKS, BUT NOT BEST)
;; -- solutions 2A - 2E exhausted all ideas with :propertize, and we got none satisfactory
;; -- so here we experiment with propertize instead
;; -- what so special about propertize?
;; --   propertize, different from :propertize, is a function (implemented in c)
;; --   so calling propertize is a function call
;; --   propertize can take elisp expressions as arguments
;; --   but propertize can not take (:eval ...) as an argument
;; --   propertize takes 'face as keyword, and its values should also begin with quote
;; --   within which-func-format, you should wrap propertize within an :eval
;; --   REASON -- :eval will indicate to the elisp interpreter to actually call propertize
;; --   without the :eval, propertize, inside which-func-format, will never be called
;; --   SEE amitp.blogspot, emacs-fu.blogspot for good examples of propertize use
;; -- just like solution 2E, the 1st argument to propertize is get-current()
;; -- but which-func-current, returned by get-current() in solution 2E, contains an :eval
;; -- so you can not return which-func-current as an argument to propertize
;; -- solved this problem here with copy-paste of which-func-current code in get-current()
;; -- +: works like a charm
;; --    emacs displays which-func in mode-line-emphasis when mode-line active
;; --    emacs displays which-func clearly (& well-blended) when mode-line inactive
;; --    not 100% sure what face emacs is using when mode-line inactive
;; -- -: manually inlined which-func-current -- code duplication!
;; --    get-current() adds an extra layer of indirection -- is it needed, really?
;; --    considerable changes to which-func-format -- is there an easier way?
;; --    it seems current-buffer = wndow-buffer whenever mode-line is updated
;; --    so is the if-condition really ever needed or used?
;; (defun get-current ()
;;   (replace-regexp-in-string "%" "%%"
;;                            (or
;;                             (gethash
;;                              (selected-window)
;;                              which-func-table)
;;                             which-func-unknown)))

;; (setq-default
;;  which-func-format
;;  `("["
;;    (:eval (propertize
;;            (get-current)
;;            'local-map ,'which-func-keymap
;;            'face (if (eq (current-buffer) (window-buffer))
;;                      'mode-line-emphasis
;;                    'mode-line-inactive)))
;;    "]"))


;; solution 2G: ~ 2F, but we pass which-func-current as an argument to propertize (ERROR)
;; -- we well know (solution 2F comments) that which-func-current contains an :eval
;; -- and we know (solution 2F comments) that propertize can not tkae :eval as argument
;; -- but we tried this option anyway as an experiment to see what error emacs will throw
;; -- -: emacs throws error -- wrong-type argument -- stringp vs (:eval (replace-regex ...)
;; --    emacs needs a string for propertize, but which-current-func gives (:eval ...)
;; --    emacs does not display any function name in the mode-line
;; --    not sure what face emacs is using, but perhaps not the ones in the code
;; --    it seems current-buffer = wndow-buffer whenever mode-line is updated
;; --    so is the if-condition really ever needed or used?
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:eval (propertize which-func-current
;;                       'local-map ,'which-func-keymap
;;                       'face (if (eq (current-buffer) (window-buffer))
;;                                 'mode-line-emphasis
;;                               'mode-line-inactive)))
;;    "]"))


;; solution 2H: ~ 2G, but we instead inline which-fun-current (WORKS! BUT NOT THE BEST) 
;; -- +: works! -- emacs picks the face from code, and there are no errors
;; --    uses only mode-line-emphasis, but merges well in both active & inactive modeline
;; -- -: inlining which-func-current duplicates code
;; --    also, considerable changes to which-func-format
;; --    one puzzle -- the if-condition seems like it will always be true
;; --    because current-buffer = wndow-buffer whenever mode-line is updated (??)
;; --    if so, is the if-condition really ever needed or used?
;; --    can we get rid off the if-condition & have mode-line-emphasis for 'face?
;; --    if so, we can have a much simpler solution:
;; --    -- use the original which-func-format, with just mode-line-emphasis face
;; (setq-default
;;  which-func-format
;;  `("["
;;    (:eval (propertize
;;            (replace-regexp-in-string "%" "%%"
;;                                      (or
;;                                       (gethash
;;                                        (selected-window)
;;                                        which-func-table)
;;                                       which-func-unknown))
;;            'local-map ,'which-func-keymap
;;            'face (if (eq (current-buffer) (window-buffer))
;;                      'mode-line-emphasis
;;                    'mode-line-inactive)))
;;    "]"))


;; solution 3: original which-func-format, with just face set to mode-line-emphasis (BEST)
;; -- +: works!! addresses all concerns in solution 2H comments
;; --    simple: reuses most of the original which-func-format that uses :propertize
;; --    but with face set to mode-line-emphasis, instead of (the original) which-func
;; --    no if-condition (see solutions 2A - 2H)
(setq-default
 which-func-format
 `("["
   (:propertize which-func-current
                local-map ,which-func-keymap
                face mode-line-emphasis)
   "]"))


 ;; see /u/ Harvey, customizing fonts, @ http://emacs.stackexchange.com/questions/2501/how-can-i-set-default-font-in-emacs
 ;; 1. select some code, & type M-x customize-face RET default RET, choose white for
 ;;    foreground color, & click "apply all changes" -> makes general font white
 ;; 2. select some comment, type M-x customize-face RET & choose forground color
 ;;    "light slate grey" & click "apply all changes" -> makes comments "light slate grey"
 ;; 3. you could do 1 & 2 in an another way as well: select some code, then
 ;;    M-x customize-face RET RET, & then choose foreground colors for default font,
 ;;    font-lock-comment-face, & anything else you wish; then click "apply all changes"
 ;;    button at the top
 ;; 4. or if you M-x customize-face RET TAB, emacs will list (in another buffer) all items
 ;;    -- such as font-lock-comment-face, font-lock-function-name, etc -- you can modify.
 ;;    you can click on an item and then hit RET, which will put you on a screen where you
 ;;    can edit & save the item you clicked
 ;; 5. choose highlight line (hl-line) color as follows:
 ;;    M-x customize-face RET hl-line, pick a color, & apply all changes
 ;;    see /u/ juanleon @ http://stackoverflow.com/questions/17701576/changing-highlight-line-color-in-emacs


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "gray85"
                         :inverse-video nil :box nil :strike-through nil :overline nil
                         :underline nil :slant normal :weight thin :height 110
                         :width normal :foundry "nil" :family "Menlo"))))
 '(flycheck-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(flycheck-fringe-warning ((t (:background "#002b36" :foreground "yellow" :weight thin))))
 '(flycheck-warning ((t (:underline "yellow" :weight thin))))
 '(font-lock-comment-face ((t (:foreground "LightCyan4"))))
 '(font-lock-constant-face ((t (:foreground "#268bd2" :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "LightBlue3" :weight thin))))
 '(font-lock-keyword-face ((t (:foreground "green1"))))
 '(font-lock-string-face ((t (:foreground "forest green"))))
 '(font-lock-type-face ((t (:foreground "DeepSkyBlue1" :weight thin))))
 '(font-lock-variable-name-face ((t (:foreground "khaki"))))
 '(ghc-face-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(ghc-face-hole ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(ghc-face-warn ((t (:underline "yellow" :weight thin))))
 '(haskell-error-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(haskell-hole-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
 '(haskell-operator-face ((t (:foreground "DarkSeaGreen1"))))
 '(haskell-warning-face ((t (:underline "yellow" :weight thin)))))
;; -----------------------------------------------------------------------------------------


;; some code used to understand/debug how which-func face can be set
;; mode-line -- :foreground "#002b36" :weight 'bold
;; mode-line-inactive -- :foreground "#839496"

;; (defun my-which-face ()
;;   (print (current-buffer))
;;   (print "\n--window-list--n")
;;   (print-elements-of-list (window-list))
;;   (print "\n--selected-window--\n")
;;   (print (selected-window))
;;   (dolist (p (window-list))
;;     (print "\n--window--\n")
;;     (print p)
;;     (print "\n--mode-line-format\n")
;;     (print-elements-of-list mode-line-format)
;;     (print "\n--misc info--\n")
;;     (print-elements-of-list mode-line-misc-info)
;;     (print "\n--which-format--\n")
;;     (print-elements-of-list which-func-format)
;;     (print "\n--which-func-keymap--\n")
;;     (print-elements-of-list which-func-keymap)
;;     (print "\n--which-func-current--\n")
;;     (print-elements-of-list which-func-current)
;;     (if (eq p (selected-window))
;;         (set-face-attribute 'which-func nil :foreground  "#002b36"  :weight 'bold)
;;        (set-face-attribute 'which-func nil :foreground "#839496"))))

;; (defun print-elements-of-list (list)
;;   "Print each element of LIST on a line of its own."
;;   (while list
;;     (print (car list))
;;     (setq list (cdr list))))

;; (add-hook 'window-setup-hook (lambda () (set-which-func-face)))

;; (defun set-which-func-face ()
;;  (set-face-attribute 'which-func nil :inherit 'mode-line :weight 'bold))

;;  '(font-lock-comment-face ((t (:foreground "#969896"))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "gray85"
;;                          :inverse-video nil :box nil :strike-through nil :overline nil
;;                          :underline nil :slant normal :weight thin :height 110
;;                          :width normal :foundry "nil" :family "Menlo"))))
;;  '(flycheck-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
;;  '(flycheck-fringe-warning ((t (:background "#002b36" :foreground "yellow" :weight thin))))
;;  '(flycheck-warning ((t (:underline "yellow" :weight thin))))
;;  '(font-lock-comment-face ((t (:foreground "LightCyan4"))))
;;  '(font-lock-constant-face ((t (:foreground "#268bd2" :weight normal))))
;;  '(font-lock-function-name-face ((t (:foreground "LightBlue3" :weight thin))))
;;  '(font-lock-keyword-face ((t (:foreground "green1"))))
;;  '(font-lock-string-face ((t (:foreground "forest green"))))
;;  '(font-lock-type-face ((t (:foreground "DeepSkyBlue1" :weight thin))))
;;  '(font-lock-variable-name-face ((t (:foreground "khaki"))))
;;  '(ghc-face-error ((t (:box (:line-width 2 :color "Red") :weight thin))))
;;  '(ghc-face-hole ((t (:box (:line-width 2 :color "Red") :weight thin))))
;;  '(ghc-face-warn ((t (:underline "yellow" :weight thin))))
;;  '(haskell-error-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
;;  '(haskell-hole-face ((t (:box (:line-width 2 :color "Red") :weight thin))))
;;  '(haskell-operator-face ((t (:foreground "DarkSeaGreen1"))))
;;  '(haskell-warning-face ((t (:underline "yellow" :weight thin)))))
;; -----------------------------------------------------------------------------------------
