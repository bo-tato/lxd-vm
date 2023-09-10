;;; langs/elisp.el -*- lexical-binding: t; -*-

(defun my/forward-sexp ()
  (interactive)
  (when (evil-eolp)
    (forward-char))
  (call-interactively #'evil-cp-forward-sexp))

(defun eval-sexp-at-point ()
  "eval current sexp"
  (interactive)
  (save-excursion
    (unless (memq (char-after) '(?\s ?\)))
      (forward-sexp)
      (backward-char))
    (call-interactively #'eros-eval-last-sexp)))

(defun eval-list-at-point ()
  "eval within surrounding parens"
  (interactive)
  (save-excursion
    (evil-next-close-paren)
    (call-interactively #'eros-eval-last-sexp)))

(defun my/save-eval-result (result)
  "Save text into register e and return it"
  (set-register ?e (format "%s" result))
  result)
(advice-add 'eval-last-sexp :filter-return #'my/save-eval-result)
(advice-add 'eval-defun :filter-return #'my/save-eval-result)

(defun evil-cp-wrap-and-insert ()
  (interactive)
  (call-interactively #'evil-cp-wrap-next-round)
  (call-interactively #'evil-cp-insert-at-beginning-of-form))

(defun my/eval-list-as-setf (sexp-at-point eval-last-sexp)
  "Insert setf at start of form and eval it, takes functions
SEXP-AT-POINT and EVAL-LAST-SEXP for a given lisp dialect"
  (lambda ()
    (interactive)
    (save-excursion
      (unless (eq ?\( (char-after))
        (evil-previous-open-paren))
      (let ((sexp (funcall sexp-at-point)))
        (with-temp-buffer
          (insert (format "%S" sexp))
          (goto-char 2)
          (insert "setf ")
          (message (buffer-string))
          (goto-char (point-max))
          (call-interactively eval-last-sexp))))))

;; this needs lexical binding true to work
(defun eval-defun-at-point-and-mark (eval-defun mark &optional eval-at-mark)
  "`eval-defun' is a eval defun-at-point for your language
`mark' is the mark it will also eval
returns a lambda that eval defun at point,
then eval defun (or optionally another function) at mark"
  (lambda ()
    (interactive)
    (call-interactively eval-defun)
    (save-excursion
      (save-window-excursion
        (evil-goto-mark mark)
        (call-interactively (or eval-at-mark eval-defun))))))

(map! :map emacs-lisp-mode-map
      "M-w" #'eval-sexp-at-point
      "M-e" #'eval-list-at-point
      :nvi "M-d" #'eval-defun
      "M-c" #'emacs-lisp-native-compile-and-load
      "M-s" (my/eval-list-as-setf #'sexp-at-point #'eval-last-sexp)
      "M-q" (eval-defun-at-point-and-mark #'eros-eval-defun ?q)
      "M-Q" (eval-defun-at-point-and-mark #'eros-eval-defun ?Q))


;; use eros for stepper, from https://xenodium.com/inline-previous-result-and-why-you-should-edebug/

(defun adviced:edebug-compute-previous-result (_ &rest r)
  "Adviced `edebug-compute-previous-result'."
  (let ((previous-value (nth 0 r)))
    (if edebug-unwrap-results
        (setq previous-value
              (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
          (edebug-safe-prin1-to-string previous-value))))

(advice-add #'edebug-compute-previous-result
            :around
            #'adviced:edebug-compute-previous-result)

(require 'eros)

(defun adviced:edebug-previous-result (_ &rest r)
  "Adviced `edebug-previous-result'."
  (eros--make-result-overlay edebug-previous-result
    :where (point)
    :duration eros-eval-result-duration))

(advice-add #'edebug-previous-result
            :around
            #'adviced:edebug-previous-result)
