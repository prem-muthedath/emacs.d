;; ---------------------------- prem's ielm-mode settings --------------------------------
;;                 this file contains customizations to IELM
;; ---------------------------------------------------------------------------------------

;; IELM output customization:
;;   -- `IELM', on `<return>', uses `pp-to-string' to output in `reader' syntax
;;   -- '(foo 'bar) is output as (foo 'bar)
;;   -- but i want `IELM' to output in lisp syntax instead
;;   -- that is, '(foo 'bar) should, on `<return>', give (foo (quote bar))
;;
;; ANALYSIS:
;;   -- `IELM' `<return>' is bound to `ielm-return' function in `ielm.el'
;;   -- `ielm-return' call eventually invokes `ielm-eval-input' -- the function
;;      that actually evaluates the input and formats the output
;;   -- `ielm-eval-input' uses `pp-to-string' to format output in `reader' syntax,
;;      which is the very thing we want to change.
;;   -- unfortunately, this `pp-to-string' call is a single line of code buried in
;;      `ielm-eval-input', and there is no way to tell or configure `ielm-eval-input'
;;      (say, using a flag or parameter) to use, for example, `prin1-to-string',
;;      instead of `pp-to-string', to get what we want
;;
;; SOLUTION:
;;   -- well, since we don't see an easy way out with IELM code, our next options
;;      include looking at existing Emacs commands for expression evaluation that
;;      output in lisp syntax, such as M-: and C-x C-e
;;   -- both M-: -- `eval-expression' -- as well as C-x C-e -- `eval-last-sexp' --
;;      will output in list syntax, i.e., ((foo (quote bar)))
;;   -- however, they're both inconvenient:
;;       -- to use M-:, you've to copy the expression from `IELM' into the minibuffer
;;       -- moreover, M-: outputs in the echo area, not in `IELM'
;;       -- likewise, C-x C-e, by default, ouputs in the echo area
;;       -- C-u C-x C-e will output in `IELM' but, awkwardly, right next to the
;;          input in `IELM', so on `<return>', `IELM' reports an invalid input
;;   -- well, clearly, M-: and C-x C-e don't give us what we want, and since we
;;      can't configure `IELM' code either, we can bypass `IELM' code altogether,
;;      and write our own code for evaluating the input and formatting the output
;;      
;;   -- ITERATION 1 -> in this design:
;;      -- we include our customizations as an ielm-mode-hook
;;      -- our code is a quick hack:
;;          -- it uses `eval-last-sexp' to evaluate the elisp expression
;;          -- formats the output in the same way that `ielm-eval-input' does,
;;             but using `prin1' -- which outputs in lisp syntax -- instead of
;;             `pp-to-string'
;;          -- for process input and output, it uses the same code (copied from
;;             `ielm-send-input' and `ielm-eval-input') in `ielm.el'
;;          -- it works for the "usual" stuff, but doesn't handle:
;;             -- multi-line input
;;             -- empty input
;;             -- error conditions
;;          -- we bind it to `s-<return>', instead of `<return>', so that
;;             users still have the option to use the `IELM' code if our code fails 
;;
;;
(defun my-ielm-mode-return (arg)
  "Evaluate the elisp expression after the prompt -- using `eval-last-sexp' instead of
`ielm-eval-input' -- and format the result -- using `prin1' instead of `pp'."
  ;; this is the key function -- a hack -- that solves the problem by
  ;; using `prin1' to format the output, instead of `pp-to-string', since `prin1'
  ;; outputs in lisp syntax.
  ;;
  ;; but this hack only works for one-line elisp expressions typed at the prompt.
  ;; it altogther ignores all multiline-input logic in `ielm-return',
  ;; as well as all error conditions. it also doesn't handle empty inputs.
  ;;
  ;; still, at a high level, it mimics the control flow in an `ielm-return' call:
  ;;   -- send input to the process     -- using `comint-send-input'
  ;;   -- evaluate the elisp expression -- using `eval-last-sexp'
  ;;   -- format the output             -- using `prin1' instead of `pp-to-string'
  ;;   -- send output to the process    -- using `comint-output-filter'
  ;;
  ;; sources:
  ;;   -- how to use eval-last-sexp in a function?
  ;;       -- see /u/ slitvinov @ https://goo.gl/qKxrdx (stackoverflow)
  ;;       -- see /u/ youngfrog @ https://goo.gl/Ypi4oQ (emacs.stackexchange)
  ;;   -- for `ielm-return', `ielm-send-input', `ielm-eval-input'
  ;;       -- see `ielm.el'
  ;;       -- see `ielm.el' @ https://goo.gl/E4BPck (MIT)
  (interactive "P")
  (comint-send-input)	;; copied from `ielm-send-input'
  (move-end-of-line 1)
  (setq my-output (eval-last-sexp arg))
  (setq my-output (concat (format "%S\n" my-output)  ;; logic from `ielm-eval-input'
                          ielm-prompt-internal))
  (comint-output-filter (ielm-process) my-output))   ;; copied from `ielm-eval-input'


(defun my-ielm-mode-hook-function ()
  (define-key ielm-map (kbd "s-<return>") #'my-ielm-mode-return)) 


(with-eval-after-load "ielm"
  (add-hook 'ielm-mode-hook #'my-ielm-mode-hook-function))

;; ---------------------------------------------------------------------------------------
