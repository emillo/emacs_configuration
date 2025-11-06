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
  ("C-c n c" . org-roam-capture)
  ("C-c n p" . my/org-roam-find-project))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (delete-dups
         (append org-agenda-files
                 (my/org-roam-list-notes-by-tag "Project")))))

;; Build the agenda list the first time for the session
;; (my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   nil
   :templates
   '(("p" "Project" plain "* Obiettivi\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun my/org-roam-files-with-tag (tag-name)
  (seq-uniq
   (mapcar #'car
           (org-roam-db-query
            `[:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote ,(concat "%\"" tag-name "\"%")))]))))

(defun my/org-roam-update-agenda ()
  (interactive)
  (setq org-agenda-files
        (delete-dups
         (append org-agenda-files
                 (my/org-roam-files-with-tag "Project")
                 (my/org-roam-files-with-tag "Agenda")))))

(defun my/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let () (counsel-rg "" org-roam-directory)))

;; sudo apt install elpa-corfu
(use-package corfu
  :ensure nil
  :pin manual
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq cofru-popupinfo-delay '(1.25 . 0.5)))

;; sudo apt install elpa-rainbow-delimiters
(use-package rainbow-delimiters
  :ensure nil
  :pin manual
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Loads the section of the configuration dedicated to emacs packages
;; available in gnu/nongnu/melpa, comment the next line for disabling it.
(load-file (locate-user-emacs-file "provided-by-gnu-nongnu-melpa.el"))
