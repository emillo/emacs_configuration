;; Begin section of emacs packages available in debian
;; usually they begin with elpa-*

;; sudo apt install elpa-vterm
(use-package vterm
  :ensure nil)

;; sudo apt install elpa-magit
(use-package magit
  :ensure nil
  :bind
  ("<f9>" . magit-status))

;; sudo apt install elpa-helpful
(use-package helpful
  :ensure nil
  :bind
  (("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key)))

;; sudo apt install elpa-vertico
(use-package vertico
  :ensure nil
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
          ("DEL" . vertico-directory-delete-char))
  :custom
  (vertico-count 10))

;; sudo apt install elpa-orderless
(use-package orderless
  :ensure nil
  :config
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))))

;; sudo apt install elpa-marginalia
(use-package marginalia
  :ensure nil
  :hook (after-init . marginalia-mode))

;; sudo apt install elpa-consult
(use-package consult
  :ensure nil
  :bind
  (([remap switch-to-buffer] . consult-buffer)))

;; sudo apt install elpa-expand-region
(use-package expand-region
  :ensure nil
  :bind
  ("M-@" . er/expand-region))

;; sudo apt install elpa-markdown-mode
(use-package markdown-mode
  :ensure nil)

;; sudo apt install elpa-ledger
(use-package ledger-mode
  :ensure nil)

;; Loads the section of the configuration dedicated to emacs packages
;; available in gnu/nongnu/melpa, comment the next sexp for disabling it.

(load-file
  (concat
    (file-name-directory load-file-name)
    "provided-by-gnu-nongnu-melpa.el"))
