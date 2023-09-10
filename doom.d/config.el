;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; open which key menu faster
(setq which-key-idle-delay 0.3
      undo-tree-visualizer-timestamps t
      ;; browse-url-browser-function #'eww-browse-url
      ;; browse-url-secondary-browser-function #'browse-url-firefox
      eww-after-render-hook #'my/scroll-to-top
      evil-snipe-spillover-scope 'visible
      next-error-find-buffer-function #'next-error-buffer-unnavigated-current
      fancy-splash-image "/usr/share/emacs/29.1/etc/images/splash.png"
      eshell-list-files-after-cd t
      eshell-history-size 4096
      company-global-modes '(not org-mode eshell-mode)
      url-user-agent nil ; set back to 'default when need github requests to work, ie dash-docs
      org-id-link-to-org-use-id t
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
;;      link-hint-action-fallback-commands (list :copy (lambda ()
;;                                                       (call-interactively
;;                                                        #'evil-yank) t))
)

(setq-default auto-fill-function 'do-auto-fill)
;; (map! :map org-mode-map
      ;; :n "y" #'link-hint-copy-link-at-point)

(push "~/.config/doom/" load-path)
(load "eshell-comint-term.el")
(load "utils.el")
(set-eshell-alias! "u" "eshell-up $1")

(add-hook 'comint-mode-hook #'capf-autosuggest-mode)
(add-hook 'eshell-mode-hook #'capf-autosuggest-mode)
(map! :map global-map
      "C-<return>" 'next-line
      "M-z" 'eshell-z
      :map doom-leader-toggle-map
      "d" 'toggle-debug-on-error
      :map doom-leader-open-map
      :desc "ruby shell" "b" 'my/ruby-repl
      :map inf-ruby-mode-map
      "C-c C-o" 'comint-kill-output-to-kill-ring
      :map capf-autosuggest-active-mode-map
      "C-f" 'capf-autosuggest-end-of-line
      :map eshell-mode-map
      "C-c C-o" 'eshell-kill-output-to-kill-ring
      :nri "C-y" 'eshell-kill-last-command
      "M-e" 'eshell-insert-last-command
      "M-f" 'find-file
      "M-F" '+default/find-file-under-here
      "M-c" 'my/find-dir-in
      :nvi "M-d" 'dired-jump
      :ni "C-r" 'evil-fixed-eshell-search-history
      :nvi "C-s" '+default/search-buffer
      :map dired-mode-map
      "M-f" 'find-file
      "M-F" '+default/find-file-under-here
      "M-e" (lambda () (interactive) (eshell "new"))
      :map eshell-hist-mode-map
      "M-r" 'eshell-insert-history-command
      :map backtrace-mode-map
      "RET" 'backtrace-toggle-locals
      :map debugger-mode-map
      :n "RET" 'backtrace-toggle-locals
      :n "v" 'push-button
      :n "K" 'debug-help-follow)

(add-hook! '(emacs-lisp-mode-hook clojure-mode-hook lisp-mode-hook sly-mrepl-mode-hook)
           #'evil-cleverparens-mode)
;; (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)

(map! :map +popup-buffer-mode-map
      :nv "q" #'+popup/quit-window)

;; vertico shortcuts
(map! :map vertico-map
      "M-SPC" #'vertico-quick-exit)

(push "~/.config/doom/langs" load-path)
(load "elisp.el")
(load "common-lisp.el")
;; (load "fennel.el")
;; (load "haskell-conf.el")
(load "clojure.el")
(load "python-config.el")
(load "ruby.el")
;;(load "crystal.el")

;; remove conflicting bindings and add new
(setq evil-cleverparens-use-s-and-S nil)
(map! :map evil-cleverparens-mode-map
      :n  "M-w" nil
      :n  "M-d" nil
      :n  "M-s" nil
      :n  "M-c" nil
      :n  "M-C" nil
      :n  "M-q" nil
      :nv "["   nil
      :nv "]"   nil
      :nv "{"   nil
      :nv "}"   nil
      :nvo "L" 'my/forward-sexp
      "M-b"    #'evil-cp-wrap-and-insert
      "C-j"    #'sp-down-sexp)

;; embark default file open actions in a new frame
(after! embark
  (setf (alist-get 'consult-grep embark-default-action-overrides)
        (my/do-other-frame #'embark-consult-goto-grep))
  (setf (alist-get 'consult-location embark-default-action-overrides)
        (my/do-other-frame #'embark-consult-goto-location))
  (setf (alist-get '(file . consult-locate) embark-default-action-overrides nil nil #'equal)
        #'find-file-other-frame)
  (setf (alist-get '(file . consult-find) embark-default-action-overrides nil nil #'equal)
        #'find-file-other-frame)
  (setf (alist-get '(file . find-file) embark-default-action-overrides nil nil #'equal)
        #'find-file-other-frame))

(map!
 ;; open in other frame
 (:leader "F" #'ctl-x-5-prefix
          "F r" #'repl-other-frame)
 (:map global-map
       ;; fast avy find
       "M-f" #'evil-avy-goto-char-2
       "C-s" #'+default/search-buffer
       "M-\""   #'paredit-doublequote
       "M-S-<return>" #'eshell-other-frame)

 (:map doom-leader-search-map
  ;; copy rather than open links
  ;; "l" #'link-hint-copy-link
  "h" #'consult-history))

;; by default M-<mouse-1> creates a secondary selection
;; change it to add multiple cursos
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") #'evil-mc-toggle-cursor-on-click)

;; Some functionality uses this to identify you, e.g GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; for github
(setq auth-sources '("~/.authinfo"))
;;(setq forge-owned-accounts '(("bo-tato")))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/shared/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
