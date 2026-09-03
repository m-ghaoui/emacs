;; -*- lexical-binding: t; -*-

(keymap-global-set '"C-c C-v" 'eval-buffer)

;; Eglot
;; Run eglot-upgrade-eglot to get the latest

;; Python
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; Don't bother with dotnet: https://lists.gnu.org/archive/html/bug-gnu-emacs/2026-07/msg00783.html

;; Completion
;; (global-completion-preview-mode 1)

(use-package company
    :ensure t
    :config
    (global-company-mode 1))

(use-package magit
    :ensure t)

(use-package rust-mode
    :ensure t)