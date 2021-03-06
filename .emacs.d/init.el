(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (js2-mode typescript-mode yasnippet csharp-mode helm-lsp company-lsp lsp-ui lsp-mode smooth-scrolling color-theme-sanityinc-tomorrow solarized-theme company projectile helm web-mode php-mode flycheck editorconfig better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; better-defaults
(require 'better-defaults)

;; editorconfig
(editorconfig-mode 1)

;; web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; flycheck
(global-flycheck-mode)

(if (string-equal system-type "windows-nt")
  (setq flycheck-php-executable "c:/php-7.3.9-Win32-VC15-x64/php.exe"))

;; helm
(require 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; Markdown mode

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; js2 mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; lsp-mode
(require 'lsp-mode)
(setq lsp-prefer-flymake nil)

; (add-hook 'csharp-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
; (add-hook 'csharp-mode-hook 'flycheck-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)

;; Clock (Org mode)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-continuously t)

;; theme
;; (load-theme 'solarized-dark t)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme bright)

;; Menu bar
(menu-bar-mode 1)

;; Smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Basic settings

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil) ; stop creating .#lock file links

;; Maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))
