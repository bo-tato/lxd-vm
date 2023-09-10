;;; utils.el -*- lexical-binding: t; -*-

(defun my/scroll-to-top ()
  "scroll buffer to cursor at top"
  (recenter 0))

(defun my/do-other-frame (f)
  "runs f in a new frame"
  (lambda (&rest args)
    (other-frame-prefix)
    (message nil)
    (apply f args)))

(defun my/find-dir-in ()
  "Jump to DIR under DIR (recursive)."
  (interactive)
  (require 'consult)
  (let ((cmd (split-string-and-unquote
              (concat +vertico-consult-fd-args "--type d")
              " ")))
    (funcall eshell-z-change-dir-function
     (consult--read
      (split-string (cdr (apply #'doom-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;; for advising functions that are off by one character with evil
(defun my/forward-char-unless-insert-mode (orig-fun &rest args)
  "Call forward-char when we're not in insert mode."
  (let ((move (not (or (eq evil-state 'insert)
                       (memq (char-after) '(?\s ?\())))))
    (when move
      (forward-char))
    (prog1 (apply orig-fun args)
      (when move
        (backward-char)))))
