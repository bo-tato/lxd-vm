;;; langs/fennel.el -*- lexical-binding: t; -*-

(defun fennel-eval-sexp ()
  (interactive)
  (save-excursion
    (unless (eq ?\s (char-after))
            (evil-cp-forward-sexp))
    (lisp-eval-last-sexp)))

(defun fennel-eval-list ()
  (interactive)
  (save-excursion
    (unless (eq ?\( (char-after))
      (evil-previous-open-paren))
    (call-interactively #'fennel-eval-sexp)))

;; sometimes buggy?
;; comint comint-proc-query uses set-buffer
(defun my/fennel-doc (thing)
  (fennel-show-documentation thing)
  t)

;; TODO eldoc isn't working for now
;; it got moved to fennel-proto-repl which isn't working with 1.3.0?

;; sometimes buggy?
(defun my/fennel-definition (thing)
  (fennel-find-definition thing))

(set-lookup-handlers! 'fennel-mode
  :documentation '(my/fennel-doc t)
  :definition 'my/fennel-definition)

(set-formatter! 'fnlfmt "fnlfmt -" :modes 'fennel-mode)

(map! :map fennel-mode-map
      :nvi "M-d" #'lisp-eval-defun
      "M-w" #'fennel-eval-sexp
      "M-e" #'fennel-eval-list
      (:localleader "e r" #'lisp-eval-region
                    "e p" #'lisp-eval-paragraph
                    "e e" #'lisp-eval-last-sexp
                    "r" #'fennel-reload
                    "c" #'fennel-view-compilation)
      (:leader "o r" #'fennel-repl)
      )
