;; Minimal config
(use-package emacs
  :init
  (column-number-mode 1)
  (delete-selection-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (fido-vertical-mode 1)
  (global-hl-line-mode +1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (size-indication-mode 1)
  (if
    (boundp 'which-key-mode)
    (which-key-mode 1)
    (message "which-key-mode not available, skipping")))

(setq 
  inhibit-splash-screen t
  initial-scratch-message nil
  lisp-indent-offset 2
  ring-bell-function 'ignore
  use-short-answers t
  visible-bell 'top-bottom
  visible-bell 1)

(setq-default
  indent-tabs-mode nil
  tab-width 4)

(set-default indent-line-function 'insert-tab)

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
     (holiday-fixed 5 1 "1 Maggio")
     (holiday-fixed 4 25 "Liberazione")
     (holiday-fixed 6 2 "Festa Repubblica")
     ))

(setq holiday-christian-holidays
  '((holiday-fixed 12 8 "Immacolata Concezione")
     (holiday-fixed 12 25 "Natale")
     (holiday-fixed 12 26 "Santo Stefano")
     (holiday-fixed 1 6 "Epifania")
     (holiday-easter-etc -52 "Giovedì grasso")
     (holiday-easter-etc -47 "Martedì grasso")
     (holiday-easter-etc  -2 "Venerdì Santo")
     (holiday-easter-etc   0 "Pasqua")
     (holiday-easter-etc  +1 "Lunedì Pasqua")
     (holiday-fixed 8 15 "Assunzione di Maria")
     (holiday-fixed 11 1 "Ognissanti")))

(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq oriental-holidays nil)
(setq general-holidays nil)
(setq calendar-holidays
  (append holiday-christian-holidays holiday-general-holidays))

;; Key bindings
(global-set-key (kbd "<f12>") 'mil/toggle-line-numbers)
(global-set-key (kbd "S-<f12>") 'mil/toggle-whitespace)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; Theme                                   
(setq modus-themes-headings
  '((1 . (variable-pitch 1.8))
     (2 . (1.5))
     (agenda-date . (1.5))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.3))))
(load-theme 'modus-vivendi-tinted)
