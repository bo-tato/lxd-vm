;;; langs/clojure.el -*- lexical-binding: t; -*-

(defun cider-custom-setup ()
  ;; otherwise cider replaces with cider-jump-to-compilation-error
  ;; everytime we open repl
  (require 'cider)

  (map! :map evil-operator-state-local-map
        "a f" #'evil-a-paren
        "i f" #'evil-inner-paren
        :map evil-visual-state-local-map
        "a f" #'evil-a-paren
        "i f" #'evil-inner-paren
        :map cider--debug-mode-map
        ;; won't be needed after https://github.com/emacs-evil/evil-collection/pull/751
        :n "C" (lambda () (interactive) (cider-debug-mode-send-reply ":continue-all")))
  ;; doom added this but we want cider completion not lsp
  (remove-hook 'cider-mode-hook #'+clojure--cider-disable-completion)
  (setq-local dash-docs-docsets '("Clojure")
              next-error-function #'flycheck-next-error-function
              lsp-enable-completion-at-point nil
              lsp-eldoc-enable-hover nil
              lsp-ui-doc-enable nil
              lsp-lens-enable nil       ; remove the reference code inlay
              cider-eldoc-display-for-symbol-at-point t
              +lookup-documentation-functions '(cider-doc))
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "com.github.jpmonettas/flow-storm-dbg" "3.3.309")
  ;; (cider-add-to-alist 'cider-jack-in-dependencies
  ;;                     "org.clojars.abhinav/snitch" "0.0.12")
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "vvvvalvalval/scope-capture" "0.3.3")
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "vvvvalvalval/scope-capture-nrepl" "0.3.1")
  (add-to-list 'cider-jack-in-nrepl-middlewares "sc.nrepl.middleware/wrap-letsc"))

(add-hook 'cider-mode-hook #'cider-custom-setup)

(setq cider-show-error-buffer nil)
(advice-add 'evil-collection-cider-debug-stacktrace :after #'+cider-goto-error-buffer)

(defun scope-capture-in-ep (ep)
  "sets local variables to given Execution point (from sc.api/spy)
if none provided defaults to the most recent EP"
  (interactive "P")
  (if ep
      (cider-interactive-eval (format "(sc.nrepl.repl/in-ep %s)" ep))
    (cider-interactive-eval "(sc.nrepl.repl/in-ep (first (sc.api/last-ep-id)))")))

(defun +cider-goto-error-buffer ()
  (interactive)
  (pop-to-buffer cider-error-buffer))

(defun my/cider-eval-list-at-point ()
  "eval within the surrounding parens"
  (interactive)
  (save-excursion
    (unless (eq ?\) (char-after))
      (evil-next-close-paren))
    (call-interactively #'cider-eval-last-sexp)))

;; easier clojure eval and wrapping
(map! :map clojure-mode-map
      "M-w" #'cider-eval-last-sexp
      "M-e" #'my/cider-eval-list-at-point
      "M-q" (eval-defun-at-point-and-mark #'cider-eval-defun-at-point ?q)
      "M-Q" (eval-defun-at-point-and-mark #'cider-eval-defun-at-point ?Q)
      :nvi "M-d" #'cider-eval-defun-at-point
      :localleader "e n" #'cider-eval-ns-form
      :localleader "v" #'cider-toggle-trace-var
      :localleader "f" #'clojure-refactor-map
      :localleader "a" #'cljr-add-project-dependency
      :localleader "h A" #'cider-apropos-documentation
      (:desc "Start FlowStorm debugger"
       :localleader "d f" (lambda ()
                            (interactive)
                            (cider-interactive-eval
                             "(require '[flow-storm.api :as fs-api])
                              (fs-api/local-connect)")))
      (:desc "Stop FlowStorm debugger"
       :localleader "d F" (lambda ()
                            (interactive)
                            (cider-interactive-eval
                             "(require '[flow-storm.api :as fs-api])
                              (fs-api/stop)")))
      (:desc "load scope-capture/spy"
       :localleader "d s" (lambda ()
                            (interactive)
                            (cider-interactive-eval
                             "(require '[sc.api :refer [spy]] '[sc.nrepl.repl])")))
      (:desc "save local variables"
       ;; this is useful when stopped in cider debugger
       ;; to save local variables for playing with later
       :localleader "d y" (lambda ()
                            (interactive)
                            (cider-interactive-eval
                             "(spy)")))
      (:desc "sc.nrepl.repl/in-ep"
       :localleader "d i" #'scope-capture-in-ep)
      (:desc "exit scope capture execution point"
       :localleader "d o" (lambda ()
                            (interactive)
                            (cider-interactive-eval
                             "(sc.nrepl.repl/exit)")))
      :desc "show error buffer" :localleader "x" '+cider-goto-error-buffer
      ;; (:desc "Inline def with snitch"
      ;;  :localleader "d i" (lambda ()
      ;;                       (interactive)
      ;;                       (cider-interactive-eval
      ;;                        "(require '[snitch.core :refer [defn* defmethod* *fn *let]])")))
      :map cider-inspector-mode-map
      :n "d" #'cider-inspector-def-current-val)
