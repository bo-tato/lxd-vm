;;; utils.el -*- lexical-binding: t; -*-
;;;
;;; eshell and comint functions

;; from: https://www.emacswiki.org/emacs/comint-kill-output-to-kill-ring.el
(defun eshell-kill-output-to-kill-ring ()
  "Kills all output from last command and puts it in kill buffer
Does not delete the prompt."
  (interactive)
  (let ((replacement nil)
        (inhibit-read-only t))
    (save-excursion
      ;; Add the text to the kill ring.
      (copy-region-as-kill eshell-last-input-end eshell-last-output-start)
      (delete-region eshell-last-input-end eshell-last-output-start)
      (goto-char eshell-last-output-end)
      (setq replacement (concat "*** output flushed to kill ring ***\n"
                                (buffer-substring eshell-last-input-end (point))))
      (delete-region eshell-last-input-end (point)))
    ;; Output message and put back prompt
    (eshell-output-filter nil replacement)))

;; from: https://www.emacswiki.org/emacs/comint-kill-output-to-kill-ring.el
(defun comint-kill-output-to-kill-ring ()
  "Kills all output from last command and puts it in kill buffer
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        ;; Add the text to the kill ring.
        (copy-region-as-kill comint-last-input-end pmark)
        (delete-region comint-last-input-end pmark)
        (goto-char (process-mark proc))
        (setq replacement (concat "*** output flushed to kill ring ***\n"
                                  (buffer-substring pmark (point))))
        (delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun eshell-insert-last-command (&optional command-type)
  "Inserts last eshell command at current position surrounded by {} or () when
called with prefix argument."
  (interactive "P")
  ;; $ is needed as capf and company complete are buggy without it
  (if command-type
      (insert "$(" (eshell-previous-input-string 0) ")")
    (insert "${" (eshell-previous-input-string 0) "}")))

(defun eshell-kill-last-command ()
  "Copy last eshell command input to kill ring"
  (interactive)
  (kill-new (eshell-previous-input-string 0)))

(defun eshell-insert-history-command (&optional command-type)
  "Opens consult-history and inserts without replacing the current line"
  (interactive "P")
  (if command-type (insert "$(") (insert "${"))
  (consult-history eshell-history-ring 'eshell-history-index)
  (if command-type (insert ")") (insert "}")))

;; need backward-char to fix end of buffer error cause of evil
(defun evil-fixed-eshell-search-history ()
  (interactive)
  (backward-char)
  (call-interactively #'+eshell/search-history))

(defun eshell-other-frame ()
  "Open a `shell' in a new frame."
  (interactive)
  (let ((buf (call-interactively #'+eshell/here)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-frame buf)))

(defun repl-other-frame ()
  "Open a `repl' in a new frame."
  (interactive)
  (+eval-open-repl nil #'switch-to-buffer-other-frame))

(defun eshell/drop-until (regex lines)
  (--drop-while (not (string-match-p regex it)) lines))

(defun eshell/take-until (regex lines)
  (--take-while (not (string-match-p regex it)) lines))
