;;; common-lisp.el --- common lisp settings  -*- lexical-binding: t; -*-

;; fancy overlay display for sly
(add-hook 'lisp-mode-hook #'eros-mode)
(add-hook! 'lisp-mode-hook
  (setq-local dash-docs-docsets '("Common Lisp")))
(add-hook! 'sly-mode-hook
  (setq +lookup-definition-functions '(sly-edit-definition)
        +lookup-references-functions '(sly-edit-uses)))

;; fix for evil mode
(advice-add 'sly-sexp-at-point :around #'my/forward-char-unless-insert-mode)

(defun sly-eval-sexp-overlay ()
  (interactive)
  (let ((result (->> `(slynk:pprint-eval ,(sly-sexp-at-point))
                     sly-eval
                     s-trim
                     (s-replace "\n" ", "))))
    (set-register ?e result)
    (eros--make-result-overlay result
      :where (point)
      :duration eros-eval-result-duration)))

;; this and defun show overlay at start of buffer?
;; after modify to just take sexp and display overlay at point
;; will address this and list-as-setf lack of overlay
(defun sly-eval-list-at-point ()
  "eval within surrounding parens"
  (interactive)
  (save-excursion
    (unless (eq ?\) (char-after))
      (evil-next-close-paren))
    (call-interactively #'sly-eval-sexp-overlay)))

(defun sly-eval-defun-overlay ()
  (interactive)
  (save-excursion
    (forward-char)
    (evil-cp-beginning-of-defun)
    (call-interactively #'sly-eval-sexp-overlay)))

(defun my/sly-eval (command)
  (lambda ()
    (interactive)
    (sly-interactive-eval command)))

(defun my/lisp-irc ()
  (interactive)
  (switch-to-buffer "#common-lisp")
  (erase-buffer)
  (let ((proc (start-process "lisp-irc-logs" "#common-lisp"
                             "ciel" "~/research/irc_log_reader/irc-log-reader.lisp")))
    (visual-line-mode)
    ;; so we stay at top when it starts outputing
    (insert "#common-lisp irc logs:\n")
    (set-marker (process-mark proc) (point))
    (beginning-of-buffer)))

(map! :map sly-stickers--replay-mode-map
      "TAB" 'forward-button)

(map! :map sly-trace-dialog-mode-map
      :nv "t" 'sly-trace-dialog-fetch-traces
      :nv "r" 'sly-trace-dialog-fetch-status)

(map! :map sly-thread-control-mode-map
      :n "q" #'quit-window
      :n "r" #'sly-update-threads-buffer)

(after! sly
  (add-to-list 'sly-contribs 'sly-refactor)
  (setq sly-complete-symbol-function 'sly-flex-completions
        sly-lisp-implementations `((ciel-sbcl ("sbcl" "--core" ,(substitute-in-file-name "$HOME/lisp-cores/ciel-core")
                                               "--eval" "(in-package :ciel-user)"
                                               "--eval" "(named-readtables:in-readtable :syntax-sugar)"))
                                   ;; (ciel-git  ("/home/user/sbcl/install/bin/sbcl"))
                                   ;; (ccl       ("/home/user/ccl/lx86cl64"))
                                   )
        sly-default-lisp 'ciel-sbcl
        sly-threads-update-interval 2)

  ;; for source staying local, could use tramp
  ;; with tramp could just use sly-create-filename-translator
  ;;(setq sly-filename-translations
        ;;`(("^hostname$"
           ;;,(lambda (emacs-filename)
              ;;(string-replace "/user/research/" "/ubuntu/" emacs-filename))
           ;;,(lambda (lisp-filename)
              ;;(string-replace "/ubuntu/" "/user/research/" lisp-filename)))))
  (put '-> 'sly-common-lisp-indent-function 0)
  (put '->> 'sly-common-lisp-indent-function 0)
  (map! :map lisp-mode-map
      "M-w" 'sly-eval-sexp-overlay
      "M-e" 'sly-eval-list-at-point
      :nvi "M-d" 'sly-eval-defun-overlay
      "M-q" (eval-defun-at-point-and-mark #'sly-eval-defun ?q)
      "M-Q" (eval-defun-at-point-and-mark #'sly-eval-defun ?Q)
      "M-c" 'sly-compile-defun
      "M-C" 'sly-compile-file
      "M-s" (my/eval-list-as-setf #'sexp-at-point #'sly-eval-last-expression)
      :nv "] e" 'sly-next-note
      :nv "[ e" 'sly-previous-note
      :nv "] s" 'sly-stickers-next-sticker
      :nv "[ s" 'sly-stickers-prev-sticker
      (:localleader "v" 'sly-edit-value
       "i" 'sly-inspect
       "I" 'my/lisp-irc
       "Q" '+lisp/find-file-in-quicklisp
       "e d" 'sly-eval-defun
       "e f" nil
       "e p" 'sly-pprint-eval-last-expression
       (:prefix ("t" . "tracing")
        :desc "toggle print tracing" "p" 'sly-toggle-fancy-trace
        "d" 'sly-trace-dialog
        "t" 'sly-trace-dialog-toggle-trace
        "s" nil
        :desc "untrace all print tracing" "P" 'sly-untrace-all
        "u" 'sly-trace-dialog-untrace-all)
       "r l" 'sly-list-connections
       "r n" 'sly-next-connection
       "r p" 'sly-prev-connection
       :desc "Threads" "T" 'sly-list-threads
       "f" nil
       (:prefix ("f" . "reFactoring")
                :desc "unwind threading macro once" "u" 'sly-unwind
                :desc "fully unwind threading macro" "U" 'sly-unwind-all
                :desc "thread first" "f" 'sly-thread-first-all
                :desc "thread last" "l" 'sly-thread-last-all
                :desc "thread once" "t" 'sly-thread)
       ;; add back +lisp/test-system or some unit test shortcuts
       (:prefix ("p" . "package")
        :desc "goto defpackage" "d" (lambda ()
                                      (interactive)
                                      (sly-goto-package-source-definition
                                       (sly-current-package))
                                      (switch-to-buffer (current-buffer)))
        :desc "export symbol" "x" 'sly-export-symbol-at-point
        :desc "import symbol" "i" 'sly-import-symbol-at-point)
       (:prefix ("q" . "quick eval menu")
        :desc "Stop Server" "s" (my/sly-eval "(stop-server)")))))
