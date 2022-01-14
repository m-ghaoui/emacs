(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Clock (Org mode)

;; Put this in ~\AppData\Roaming\.doom.d\config.el and run 'doom sync'
(setq org-clock-clocktable-default-properties (quote (:narrow 70 :maxlevel 2 :scope file)))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-continuously t)
(setq org-clock-out-remove-zero-time-clocks nil)

;; Basic settings

(savehist-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ;
(setq create-lockfiles nil) ; stop creating .#lock file links

(load-theme 'tango-dark t)
