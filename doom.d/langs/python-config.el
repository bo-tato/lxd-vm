;;; langs/python.el -*- lexical-binding: t; -*-

;; python
;; python-shell-send-statement is buggy in emacs 28, fixed in 29
(defun my/python-shell-send-statement ()
  "Send the current line to the inferior python process for evaluation."
  (interactive)
  (save-excursion
    (let ((end (python-nav-end-of-statement))
          (beginning (python-nav-beginning-of-statement)))
      (python-shell-send-region beginning end))))

(defun my/python-shell-send-block ()
  "Send the current block to the inferior python process for evaluation."
  (interactive)
  (save-excursion
    (let ((beginning (python-nav-beginning-of-block))
          (end (python-nav-end-of-block)))
      (python-shell-send-region beginning end))))

(defun my/ipython-whos (process)
  (interactive (list (python-shell-get-process)))
  (comint-send-string process "%whos\n"))

(defun my/python-send-symbol-at-point ()
  (interactive)
  (python-shell-send-string (python-info-current-symbol)))

;; actually just need this once, not load on each file open?
(defun my/remote-lsp ()
  (interactive)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "jedi-language-server")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'jedi-remote))
  (lsp-mode)
  ;; (flycheck-select-checker 'lsp)
  )

(map! :map python-mode-map
      :nvi "M-d" #'python-shell-send-defun
      "M-w" #'my/python-send-symbol-at-point
      "M-e" #'my/python-shell-send-statement
      "M-r" #'+eval/buffer-or-region
      "M-b" #'my/python-shell-send-block
      "M-q" (eval-defun-at-point-and-mark #'python-shell-send-defun ?q #'my/python-shell-send-statement)
      "M-Q" (eval-defun-at-point-and-mark #'python-shell-send-defun ?Q #'my/python-shell-send-statement)
      :localleader "w" #'my/ipython-whos
      :desc "Start remote lsp" :localleader "l" #'my/remote-lsp
      :localleader "i i" #'importmagic-fix-imports
      :localleader "i m" #'importmagic-mode
      :localleader "i p" #'importmagic-fix-symbol-at-point
      :localleader "s f" #'python-shell-send-file
      :localleader "s s" #'python-shell-send-string)
;; (setq python-shell-interpreter "ipython3"
;;       python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
;;       dap-python-executable "python3")
;;(use-package lsp-jedi)
;;(setq lsp-disabled-clients '(pyright))
(after! dap-mode
  (setq dap-python-debugger 'debugpy))
(add-hook! 'python-mode-hook
           ;; #'importmagic-mode
           (setq flycheck-checker 'python-flake8)
           (map! :map evil-motion-state-map
                 "(" #'python-nav-backward-statement
                 ")" #'python-nav-forward-statement))
