;;; init-const.el --- Define constants -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

(defconst sys/linuxp (eq system-type 'gnu/linux)
  "Running on GNU/Linux.")

(defconst sys/macp (eq system-type 'darwin)
  "Running on Mac system.")

(defconst sys/cygwinp (eq system-type 'cygwin)
  "Running on Cygwin system.")

(defconst sys/winp (eq system-type 'windows-nt)
  "Running on Windows system.")

(defconst sys/mac-x-p (and (display-graphic-p) sys/macp)
  "Running under X on Mac system.")

(defconst sys/linux-x-p (and (display-graphic-p) sys/linuxp)
  "Running under X on GNU/Linux system.")

(defconst sys/rootp (string-equal "root" (getenv "USER"))
  "Root user.")

(defconst emacs/>=28p (>= emacs-major-version 28)
  "Emacs version is 28 or above.")

(defconst emacs/>=27p (>= emacs-major-version 27)
  "Emacs version is 27 or above.")

(defconst emacs/>=26p (>= emacs-major-version 26)
  "Emacs version is 26 or above.")

(defconst emacs/>=25p (>= emacs-major-version 25)
  "Emacs version is 25 or above.")

(provide 'init-const)

;;; init-const.el ends here
