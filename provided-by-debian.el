;; Begin section of emacs packages available in debian
;; usually they begin with elpa-*

;; sudo apt install elpa-vterm
(use-package vterm
  :ensure nil
  :pin manual)

;; sudo apt install elpa-magit elpa-magit-section elpa-transient elpa-llama elpa-with-editor
(use-package magit
  :ensure nil
  :pin manual
  :bind
  ("<f9>" . magit-status))

(use-package magit-section
  :ensure nil
  :pin manual)

(use-package transient
  :ensure nil
  :pin manual)

(use-package llama
  :ensure nil
  :pin manual)

(use-package with-editor
  :ensure nil
  :pin manual)

;; sudo apt install elpa-format-all elpa-inheritenv elpa-language-id
(use-package format-all
  :ensure nil
  :pin manual
  :bind
  ("M-F" . format-all-buffer))

(use-package inheritenv
  :ensure nil
  :pin manual)

(use-package language-id
  :ensure nil
  :pin manual)

;; sudo apt install elpa-helpful elpa-dash elpa-f elpa-s elpa-elisp-refs
(use-package helpful
  :ensure nil
  :pin manual
  :bind
  (("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key)))

(use-package s
  :ensure nil
  :pin manual)

(use-package f
  :ensure nil
  :pin manual)

(use-package dash
  :ensure nil
  :pin manual)

(use-package elisp-refs
  :ensure nil
  :pin manual)

;; sudo apt install elpa-vertico
(use-package vertico
  :ensure nil
  :pin manual
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
          ("DEL" . vertico-directory-delete-char))
  :custom
  (vertico-count 10))

;; sudo apt install elpa-orderless
(use-package orderless
  :ensure nil
  :pin manual
  :config
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))))

;; sudo apt install elpa-marginalia
(use-package marginalia
  :ensure nil
  :pin manual
  :hook (after-init . marginalia-mode))

;; sudo apt install elpa-consult
(use-package consult
  :ensure nil
  :pin manual
  :bind
  (([remap switch-to-buffer] . consult-buffer)
    ([remap goto-line] . consult-goto-line)
    ([remap project-switch-to-buffer] . consult-project-buffer)
    ([remap yank-pop] . consult-yank-pop)
    ([remap bookmark-jump] . consult-bookmark)
    ("M-g o" . consult-outline)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s r" . consult-ripgrep)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ("M-s k" . consult-keep-lines)
    ("M-s u" . consult-focus-lines)
    ("M-s l" . consult-line)))

;; sudo apt install elpa-expand-region
(use-package expand-region
  :ensure nil
  :pin manual
  :bind
  ("M-@" . er/expand-region))

;; sudo apt install elpa-markdown-mode
(use-package markdown-mode
  :ensure nil
  :pin manual)

;; sudo apt install elpa-ledger
(use-package ledger-mode
  :ensure nil
  :pin manual)

;; sudo apt install elpa-org-roam elpa-emacsql elpa-emacsql-sqliteorg-roam-doc
(use-package org-roam
  :ensure nil
  :pin manual
  :custom
  (org-roam-completion-everywhere t)
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture))

;; Loads the section of the configuration dedicated to emacs packages
;; available in gnu/nongnu/melpa, comment the next line for disabling it.
(load-file (locate-user-emacs-file "provided-by-gnu-nongnu-melpa.el"))
