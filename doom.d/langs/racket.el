;; racket
;; for formatting sometimes need to set manually:
;; (setq format-all-formatters '(("Racket" raco-fmt)))
(defun evil-racket-send-last-sexp ()
  (interactive)
  (save-excursion
    (forward-char)
    (call-interactively #'racket-send-last-sexp)))

(defun racket-send-list-at-point ()
  "eval within surrounding parens"
  (interactive)
  (save-excursion
    (evil-next-close-paren)
    (call-interactively #'evil-racket-send-last-sexp)))

(map! :map racket-mode-map
      "C-\\" #'racket-insert-lambda
      "M-w" #'evil-racket-send-last-sexp
      "M-e" #'racket-send-list-at-point
      :nvi "M-d" #'racket-send-definition)
