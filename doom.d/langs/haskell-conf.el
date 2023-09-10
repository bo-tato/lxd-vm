;;; langs/haskell.el -*- lexical-binding: t; -*-

(map! :map haskell-mode-map
      "M-e" #'+eval/line-or-region
      :localleader "t" #'haskell-process-do-type
      :localleader "r" #'+haskell/open-repl)

(map! :map haskell-error-mode-map
      :nv "q" #'+popup/quit-window)

(after! haskell-mode
  (setq dash-docs-docsets '("Haskell")))
