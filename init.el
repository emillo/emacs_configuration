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
  (which-key-mode 1))

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

(global-set-key (kbd "<f12>") 'mil/toggle-line-numbers)
(global-set-key (kbd "S-<f12>") 'mil/toggle-whitespace)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "M-o") 'other-window)

;; Theme                                   
(load-theme 'modus-vivendi-tinted)
