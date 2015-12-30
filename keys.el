(require 'tex-mode)
(require 'tex-italics)
(require 'skeleton)
(require 'one-sentence-regions)

(load-library "TeX/one-sentence-regions")

(define-skeleton insert-endnote
  "Insert an endnote item"
  nil "\\" "endnote{%" \n _ "\n}" \n)


(define-skeleton insert-code-library
  "Insert a codeLibrary item"
  nil "\\" "codeLibrary{" _ "}")


(define-skeleton insert-meta-syntactic-variable
  "Insert a \metaSyntacticVariable item"
  nil "\\" "mSV{" _ "}")

(defun latex-dgc-toggle-progLang (arg)
  (interactive "*P")
  (latex-dgc-toggle-block "\\progLang" arg))



(define-skeleton insert-crossout
  "insert \\xxx{ ... } ."
  nil "\\xxx" "{" _ "}")


(define-skeleton insert-more
  "insert \\yyy{ ... } ."
  nil "\\yyy" "{" _ "}" "{" - "}")

(defun go-keys ()
  (interactive)
  (let ((map (lookup-key latex-mode-map (kbd "C-c"))))
    (define-key map (kbd "C") 'insert-code-library)
    (define-key map (kbd "e") 'insert-endnote)
    (define-key map (kbd "S") 'insert-meta-syntactic-variable)
    (define-key map (kbd "p") 'latex-dgc-toggle-progLang))
  (define-key latex-mode-map (kbd "M-&") 'another-one-sentence-regions)
  (define-key latex-mode-map (kbd "s-<backspace>")   'insert-crossout)
  (define-key latex-mode-map (kbd "S-s-<backspace>") 'insert-more))
