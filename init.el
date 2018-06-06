;; -*- lexical-binding: t; -*-

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Basic settings
(setq inhibit-startup-screen t)
(savehist-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq make-backup-files nil) ; stop creating backup~ files
;; (setq auto-save-default nil) ;
;; (setq create-lockfiles nil) ; stop creating .#lock file links
(load-theme 'tango-dark t)

;; https://github.com/KallDrexx/emacs-zero-to-ide-journey/tree/main/03-Initial-Config
(setq gc-cons-threshold 10000000) ;; 10MB
(setq read-process-output-max (* 1024 1024 4))
(tool-bar-mode -1)

;; https://github.com/KallDrexx/emacs-zero-to-ide-journey/blob/main/04-Emacs-Variables/Readme.md
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Clock (Org mode)
(setq org-clock-clocktable-default-properties (quote (:narrow 70 :maxlevel 3 :scope file)))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-continuously t)
(setq org-clock-out-remove-zero-time-clocks nil)
(define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)
