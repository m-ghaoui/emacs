;; -*- lexical-binding: t; -*-

;; Clock (Org mode)
(setq org-clock-clocktable-default-properties (quote (:narrow 70 :maxlevel 3 :scope file)))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-continuously t)
(setq org-clock-out-remove-zero-time-clocks nil)
(define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)
