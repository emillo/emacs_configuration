;; Emiliano Grilli's Emacs configuration

;; Vanilla emacs section (init.el):
;; it shoud work without requiring any external package
;; tested on emacs 30.1 on debian 13 trixie

;; Setting some variables
(setq
  Man-notify-method 'aggressive
  auto-revert-verbose t
  create-lockfiles nil
  desktop-save t
  help-window-select t
  inhibit-splash-screen t
  initial-scratch-message nil
  lisp-indent-offset 2
  make-backup-files nil
  ring-bell-function 'ignore
  shell-kill-buffer-on-exit t
  use-short-answers t
  visible-bell t)

(setq-default
  indent-tabs-mode nil
  tab-width 4)

(set-default indent-line-function 'insert-tab)

;; send customizations away from init.el (prot)
(setq custom-file (make-temp-file "emacs-custom-"))

;; Minor modes
(column-number-mode 1)
(delete-selection-mode 1)
(desktop-save-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(fido-vertical-mode -1)
(global-auto-revert-mode 1)
(global-completion-preview-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(scroll-bar-mode -1)
(size-indication-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(which-key-mode 1)

;; Electric pair also for backtick
(setq electric-pair-pairs
  (quote
    ((34 . 34)
      (8216 . 8217)
      (8220 . 8221)
      (96 . 96))))

;; Theme
(setq modus-themes-headings
  '((1 . (variable-pitch 1.8))
     (2 . (1.5))
     (agenda-date . (1.5))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.3))))
(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

(load-theme 'modus-vivendi-tinted)

;; Font
(set-frame-font "Hack-15")
(set-face-attribute 'default t :font "Hack-15")
(add-to-list 'default-frame-alist '(font . "Hack-15" ))

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format
  '(:eval (format "Emacs - %s  [ %s ]"
            (buffer-name)
            last-command))
  icon-title-format t)

;; Key bindings
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Save history
(use-package savehist
  :ensure nil
  :hook  (after-init . savehist-mode))

;; Org mode
(use-package org
  :ensure nil
  :defer t
  :config
  (setq
    org-agenda-include-diary t
    org-confirm-babel-evaluate nil
    org-ctrl-k-protect-subtree t
    org-cycle-separator-lines 0
    org-ellipsis " ⤵"
    org-hide-emphasis-markers t
    org-log-done 'time
    org-startup-indented t
    org-startup-folded 'showall))

;; Italian calendar names
(setq calendar-week-start-day 1
  calendar-day-name-array ["Domenica" "Lunedì" "Martedì" "Mercoledì"
                            "Giovedì" "Venerdì" "Sabato"]
  calendar-day-abbrev-array ["Dom" "Lun" "Mar" "Mer" "Gio" "Ven" "Sab"]
  calendar-day-header-array ["Do" "Lu" "Ma" "Me" "Gi" "Ve" "Sa"]
  calendar-month-name-array ["Gennaio" "Febbraio" "Marzo" "Aprile"
                              "Maggio" "Giugno" "Luglio" "Agosto"
                              "Settembre" "Ottobre" "Novembre"
                              "Dicembre"]
  calendar-month-abbrev-array ["Gen" "Feb" "Mar" "Apr" "Mag"
                                "Giu" "Lug" "Ago"
                                "Set" "Ott" "Nov" "Dic"]
  calendar-date-style 'european)

;; Italian Holidays
(setq holiday-general-holidays
  '((holiday-fixed 1 1 "Capodanno")
     (holiday-fixed 5 1 "Festa dei lavoratori")
     (holiday-fixed 4 25 "Festa della liberazione")
     (holiday-fixed 6 2 "Festa della repubblica")
     ))

(setq holiday-christian-holidays
  '((holiday-fixed 12 8 "Immacolata concezione")
     (holiday-fixed 12 25 "Natale")
     (holiday-fixed 12 26 "Santo Stefano")
     (holiday-fixed 1 6 "Epifania")
     (holiday-easter-etc -52 "Giovedì grasso")
     (holiday-easter-etc -47 "Martedì grasso")
     (holiday-easter-etc  -2 "Venerdì Santo")
     (holiday-easter-etc   0 "Pasqua")
     (holiday-easter-etc  +1 "Lunedì di Pasqua")
     (holiday-fixed 8 15 "Assunzione di Maria")
     (holiday-fixed 11 1 "Ognissanti")))

(setq calendar-holidays
  (append holiday-christian-holidays holiday-general-holidays))

;; Disable other holidays
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq oriental-holidays nil)
(setq general-holidays nil)

;; Other calendar/diary goodies
(setq cal-tex-diary t)
(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)

;; Custom toggles
(defun mil/toggle-line-numbers (args)
  "toggles display-line-numbers-mode"
  (interactive "P")
  (if (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(defun mil/toggle-whitespace (args)
  "toggles whitespace-mode"
  (interactive "P")
  (if (bound-and-true-p whitespace-mode)
    (whitespace-mode -1)
    (whitespace-mode 1)))
(defun mil/toggle-visual-line-mode (args)
  "toggles visual-line-mode"
  (interactive "P")
  (if(bound-and-true-p visual-line-mode)
    (visual-line-mode -1)
    (visual-line-mode 1)))

(global-set-key (kbd "<f12>") 'mil/toggle-line-numbers)
(global-set-key (kbd "S-<f12>") 'mil/toggle-whitespace)
(global-set-key (kbd "C-<f12>") 'mil/toggle-visual-line-mode)

;; Loads the section of the configuration dedicated to emacs packages
;; available in debian, comment the next sexp for disabling it.

(load-file
  (concat
    (file-name-directory load-file-name)
    "provided-by-debian.el"))
