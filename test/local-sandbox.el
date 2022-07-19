;; -*- lexical-binding: t; -*-

;; FIXME: Merely loading a file should not cause such drastic changes.

(setq package-user-dir
      (concat
       default-directory
       "elpa-sandbox/"
       (int-to-string emacs-major-version)
       "."
       (int-to-string emacs-minor-version)
       ))


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))

;; switch this off or Emacs-25 will fail to get to gnu
(setq package-check-signature nil)
(package-initialize)
(package-refresh-contents)

(package-install 'm-buffer)
(package-install 'load-relative)

(load-file "assess-discover.el")

