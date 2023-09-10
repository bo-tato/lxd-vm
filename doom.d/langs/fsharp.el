;; fsharp
(map! :map fsharp-mode-map
      "M-e" #'fsharp-eval-phrase)
(add-hook! 'fsharp-mode-hook
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (setq format-all-formatters nil
        lsp-fsharp-enable-reference-code-lens nil
        lsp-lens-enable nil))
