;;; init-ido.el --- Ido setup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Ido configuration.
;;

;;; Code:

(if (fboundp 'fido-mode)
    (progn
      (fido-mode +1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode +1)))
  (progn
    (ido-mode +1)
    (ido-everywhere +1)))

(provide 'init-ido)

;;; init-ido.el ends here
