(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-headings
    '((1 semibold  variable-pitch 1.6)
       (2 regular 1.4)
       (3 regular 1.3)
       (agenda-date 1.4)
       (agenda-structure variable-pitch light 1.6)
       (t variable-pitch))))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package scratch
  :ensure t
  :bind (("C-c s" . scratch)))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  :hook (after-init . drag-stuff-global-mode))
