;;; langs/crystal.el -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(crystal-mode . "crystal"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
                    :activation-fn (lsp-activate-on "crystal")
                    :priority '1
                    :server-id 'crystalline)))

(defun crystal-init ()
  (interactive)
                                        ; required for paredit-doublequote
  (require 'paredit)
  (map! :map vertico-map
        "K" (lambda ()
              (interactive)
              (let ((inhibit-message t))
                (display-buffer (lsp-doc-buffer (lsp-completion--get-documentation (vertico--candidate))))))))

(add-hook 'crystal-mode-hook 'crystal-init)
