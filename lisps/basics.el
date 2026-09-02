;; -*- lexical-binding: t; -*-

;; Basic settings
(setq inhibit-startup-screen t)
;; (savehist-mode 1)
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
