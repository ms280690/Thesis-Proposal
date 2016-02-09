;;;###autoload
(define-derived-mode mehul-thesis-mode latex-mode "LaTeX Thesis"
  "Major mode for editing files of input for Mehul Solanki's thesis.
Derived from `latex-mode' (which see) .

Special commands:
\\{latex-mode-map}
\\{mehul-mode-map}

Mode variables:
latex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].

Entering Latex Thesis mode runs the hook `text-mode-hook', 
then `tex-mode-hook', 
then `latex-mode-hook',
and finally `mehul-thesis-mode-hook'.  When the special
subshell is initiated, `tex-shell-hook' is run."
  (add-hook 'completion-at-point-functions
            'latex-complete-data nil 'local)
  (setq-local outline-regexp latex-outline-regexp)
  (setq-local outline-level 'latex-outline-level)
  (setq-local forward-sexp-function 'latex-forward-sexp)
  (setq-local skeleton-end-hook nil))

(add-hook
 'mehul-thesis-mode-hook
 'mehul-thesis-mode-standard-setup)

(defun mehul-thesis-mode-standard-setup ()
  (interactive)
  (set-fill-column 115)
  (setq-local tex-start-options "-shell-escape ")
  (unless (fboundp 'latex-dgc-toggle-progLang)
    (let
        ((default-directory
           (concat "/Users/casper/University/Supervision/"
                   "ByStudent/Mehul/Thesis-Proposal/")))
      (load-file "keys.el")
      (go-keys)))
  (setq-local sentence-end-double-space nil)
  (turn-on-font-lock))
