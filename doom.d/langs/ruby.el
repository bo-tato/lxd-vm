;;; langs/ruby.el -*- lexical-binding: t; -*-

(defun my/ruby-forward-sexp ()
  (interactive)
  (when (evil-eolp)
    (forward-char))
  (call-interactively #'ruby-forward-sexp))

(defun my/ruby-send-sexp ()
  (interactive)
  (-if-let ((start . end) (bounds-of-thing-at-point 'sexp))
      (ruby-send-region start end)
    (user-error "no sexp at point")))

(advice-add 'ruby-send-region :filter-return #'my/save-eval-result)

(defun my/ruby-repl ()
  "Start a plain non-project repl in current directory, with robe loaded."
  (interactive)
  (require 'robe)
  (prog1 (call-interactively #'inf-ruby)
    ;; (select-window (previous-window))
    (unless robe-running (robe-start))))

(map! :map ruby-mode-map
      :m "(" #'ruby-beginning-of-block
      :m ")" #'er/ruby-end-of-block
      :m "[ [" #'ruby-beginning-of-defun
      :m "] ]" #'end-of-defun
      :m "L" #'my/ruby-forward-sexp
      :m "H" #'ruby-backward-sexp
      "M-w" #'my/ruby-send-sexp
      "M-e" #'ruby-send-line
      "M-r" #'ruby-send-region
      "M-b" #'my/ruby-send-block
      "M-q" (eval-defun-at-point-and-mark #'ruby-send-definition ?q #'ruby-send-block)
      "M-Q" (eval-defun-at-point-and-mark #'ruby-send-definition ?Q #'ruby-send-block)
      ;; can't use :leader as that sets in global map
      :nvi "M-d" #'ruby-send-definition
      :localleader "l" #'ruby-load-current-file)

(setq inf-ruby-default-implementation "pry")

(after! robe
  (set-repl-handler! 'ruby-mode #'my/ruby-repl))

(defun ruby-init ()
  (interactive)
  (setq-local dash-docs-docsets '("Ruby"))
  ;; https://emacs.stackexchange.com/a/48877
  ;; could add minibuffer hook to set last buffer and check that it was ruby-mode
  ;; required for paredit-doublequote
  (require 'paredit)
  (map! :map vertico-map
        "K" (lambda ()
              (interactive)
              (let ((inhibit-message t))
                (robe-show-doc (robe-jump-prompt (vertico--candidate))))))
  (setq +lookup-documentation-functions '(robe-doc)
        +lookup-definition-functions '(robe-jump)))

(add-hook 'ruby-mode-hook 'ruby-init)

                                        ; disable format on save
(when (eq 'not (car +format-on-save-enabled-modes))
  (nconc +format-on-save-enabled-modes '(ruby-mode)))

                                        ; waiting on fix to be upstreamed
                                        ; https://github.com/nonsequitur/inf-ruby/issues/171
(defun my/ruby-send-block (&optional print)
  "Send the current block to the inferior Ruby process."
  (interactive "P")
  (-let (((start . end) (er/get-ruby-block)))
    (ruby-send-region start end))
  (ruby-print-result print))

(defun my/filter-ruby-repl (string)
  "filter out pry's prompts that end in '* ', so the ruby-send-* commands
don't show extra prompts"
  (if (and (string-match inf-ruby-prompt-pattern string)
           (s-suffix? "* " string))
      ""
    string))

;; this breaks displaying results
;; (defun my/add-ruby-comint-filter ()
;;   (add-hook 'comint-preoutput-filter-functions #'my/filter-ruby-repl))

;; (add-hook 'inf-ruby-mode-hook #'my/add-ruby-comint-filter)
