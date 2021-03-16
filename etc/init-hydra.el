;;; init-hydra.el --- personal hydra -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Hydra configuration.
;;
;; A short Intro: https://oremacs.com/download/london.pdf
;;
;; color could be: red, blue, amaranth, pink, teal
;;
;; It aggregates `:exit` and `:foreign-keys` key in the following way:
;;
;;     | color    | toggle                     |
;;     |----------+----------------------------|
;;     | red      |                            |
;;     | blue     | :exit t                    |
;;     | amaranth | :foreign-keys warn         |
;;     | teal     | :foreign-keys warn :exit t |
;;     | pink     | :foreign-keys run          |
;;

;;; Code:

(require 'hydra)

;; ---------------------------------------------------------
;; gnus
;; ---------------------------------------------------------

;; gnus-summary-mode
(with-eval-after-load 'gnus-sum
  (defhydra my/hydra-gnus-summary (:color blue)
    "
[_F_] Forward (C-c C-f)             [_s_] Show thread
[_e_] Resend (S D e)                [_h_] Hide thread
[_r_] Reply                         [_n_] Refresh (/ N)
[_R_] Reply with original           [_!_] Mail -> disk
[_w_] Reply all (S w)               [_d_] Disk -> mail
[_W_] Reply all with original (S W) [_c_] Read all
[_#_] Mark                          [_A_] Show Raw article
"
    ("s" gnus-summary-show-thread)
    ("h" gnus-summary-hide-thread)
    ("n" gnus-summary-insert-new-articles)
    ("F" gnus-summary-mail-forward)
    ("!" gnus-summary-tick-article-forward)
    ("d" gnus-summary-put-mark-as-read-next)
    ("c" gnus-summary-catchup-and-exit)
    ("e" gnus-summary-resend-message-edit)
    ("R" gnus-summary-reply-with-original)
    ("r" gnus-summary-reply)
    ("W" gnus-summary-wide-reply-with-original)
    ("w" gnus-summary-wide-reply)
    ("#" gnus-topic-mark-topic)
    ("A" gnus-summary-show-raw-article)
    ("q" nil))
  ;; y is not used by default
  (define-key gnus-summary-mode-map "y" #'my/hydra-gnus-summary/body))

;; gnus-article-mode
(with-eval-after-load 'gnus-art
  (defhydra my/hydra-gnus-article (:color blue)
    "
[_F_] Forward
[_r_] Reply           [_R_] Reply with original
[_w_] Reply all (S w) [_W_] Reply all with original (S W)
"
    ("F" gnus-summary-mail-forward)
    ("r" gnus-article-reply)
    ("R" gnus-article-reply-with-original)
    ("w" gnus-article-wide-reply)
    ("W" gnus-article-wide-reply-with-original)
    ("q" nil))
  ;; y is not used by default
  (define-key gnus-article-mode-map "y" #'my/hydra-gnus-article/body))

;; ---------------------------------------------------------
;; dired
;; ---------------------------------------------------------

(with-eval-after-load 'dired
  (defun my/convert-video-format ()
    "Convert file to target format using ffmpeg."
    (interactive)
    (progn
      (mapc (lambda (file)
              (let* ((fmt (read-string "Input target format: "))
                     (limit (read-string "Do you want to set video rates? (y to confirm, other to skip): "))
                     rates
                     cmd)
                (cond
                  ((string= limit "y")
                    (setq rates (read-string "Input frame rates you want (default value: \"8\") or press [ENTER] to skip: " nil nil "8"))
                    (setq cmd (format "ffmpeg -i \"%s\" -r \"%s\" \"%s\""
                                      file
                                      rates
                                      (format "%s.%s" (file-name-base file) fmt))))
                  (t
                    (setq cmd (format "ffmpeg -i \"%s\" \"%s\""
                                      file
                                      (format "%s.%s" (file-name-base file) fmt)))))
                (async-shell-command cmd)))
        (dired-get-marked-files))
      (dired-unmark-all-marks)))

  (defun my/extract-mp3-from-video ()
    "Extract mp3 from current video file using ffmpeg."
    (interactive)
    (let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
           (params (split-string (string-trim (read-string "Input start-second [total seconds] (e.g, \"6 10\" or \"05:30 5\") or just press enter: "))
                     "[ \t\n\r+]"))
           (start (car params))
           (total (if (eq (length params) 1) "5" (nth 1 params)))
           cmd)
      (cond
        ((string= start "")
          ;; extract audio to MP3 with sample rate 44.1Khz (CD quality), stereo, and 2 channels
          (setq cmd (format "ffmpeg -i \"%s\" -vn -ar 44100 -ac 2 -b:a 192k -f mp3 \"%s\""
                            video-file
                            (concat (file-name-base video-file) ".mp3"))))
        (t
          (setq cmd (format "ffmpeg -i \"%s\" -vn -ss %s -t %s \"%s\""
                            video-file
                            start
                            total
                            (format "%s-%s-%s.mp3" (file-name-base video-file) start total)))))
      (async-shell-command cmd)))

  (defun my/extract-mp4-from-video ()
    "Extract mp4 from current video file using ffmpeg."
    (interactive)
    (let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
           (params (split-string (string-trim (read-string "Input start-second [duration seconds] (e.g, \"6 10\" or \"05:30 5\") or just press enter: "))
                     "[ \t\n\r+]"))
           (start (car params))
           (total (if (eq (length params) 1) "5" (nth 1 params)))
           cmd)
      (cond
        ((string= start "")
          (setq cmd (format "ffmpeg -i \"%s\" -codec copy -avoid_negative_ts 1 \"%s\""
                            video-file
                            (concat (file-name-base video-file) ".mp4"))))
        (t
          (setq cmd (format "ffmpeg -ss %s -t %s -accurate_seek -i \"%s\" -codec copy -avoid_negative_ts 1 \"%s\""
                            start
                            total
                            video-file
                            (format "%s-%s-%s.mp4" (file-name-base video-file) start total)))))
      (async-shell-command cmd)))

  (defun my/subtract-mp4-from-video ()
    "Subtract mp4 from current video file using ffmpeg."
    (interactive)
    (let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
           (params (split-string (string-trim (read-string "Please input start-time and stop-time (e.g, \"6 10\" or \"05:30 07:10\") or just press enter: "))
                     "[ \t\n\r+]"))
           (start (car params))
           (total (if (eq (length params) 1) "5" (nth 1 params)))
           cmd)
      (cond
        ((string= start "")
          (setq cmd (format "ffmpeg -i \"%s\" -codec copy -avoid_negative_ts 1 \"%s\""
                            video-file
                            (concat (file-name-base video-file) ".mp4"))))
        (t
          (setq cmd (format "ffmpeg -ss %s -to %s -accurate_seek -i \"%s\" -codec copy -avoid_negative_ts 1 \"%s\""
                            start
                            total
                            video-file
                            (format "%s-[%s-%s].mp4" (file-name-base video-file) start total)))))
      (async-shell-command cmd)))

  (defhydra my/hydra-dired (:color blue :hint nil)
    "
  ^convert^            ^misc^
--------------------------------------------
 [_vs_] Video => MP3  [_td_] toggle-details
 [_vv_] Video => MP4  [_ff_] find-file
 [_vc_] Video Cut
 [_vf_] Video Format
---------------------------------------------
 [_q_]  quit
"
    ("vs" my/extract-mp3-from-video)
    ("vv" my/extract-mp4-from-video)
    ("vc" my/subtract-mp4-from-video)
    ("vf" my/convert-video-format)
    ("ff" (lambda (regexp)
            (interactive "sMatching regexp: ")
            (find-lisp-find-dired default-directory regexp)))
    ("td" dired-hide-details-mode)
    ("q" nil))
  ;; y is not used by default
  (define-key dired-mode-map "y" #'my/hydra-dired/body))

;; ---------------------------------------------------------
;; Toggle minor mode
;; ---------------------------------------------------------
;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

(defhydra my/hydra-toggle (:color amaranth :hint nil)
  "
------------------------------------------------------------
[_a_] abbrev-mode           : %`abbrev-mode
[_d_] debug-on-error        : %`debug-on-error
[_f_] fill-column-indicator : %(bound-and-true-p display-fill-column-indicator-mode)
[_g_] glasses-mode          : %(bound-and-true-p glasses-mode)
[_h_] hl-line-mode          : %(bound-and-true-p hl-line-mode)
[_i_] indent-tabs-mode      : %`indent-tabs-mode
[_l_] line-numbers          : %(if (bound-and-true-p display-line-numbers) display-line-numbers
                                 (bound-and-true-p linum-mode))
[_o_] outline-minor         : %(bound-and-true-p outline-minor-mode)
[_p_] prettier symbols      : %`prettify-symbols-mode
[_r_] rainbow-mode          : %(bound-and-true-p rainbow-mode)
[_s_] sp-strict-mode        : %(bound-and-true-p smartparens-strict-mode)
[_t_] truncate-lines        : %`truncate-lines
[_v_] view-mode             : %`view-mode
[_V_] visual-line           : %`visual-line-mode
[_w_] whitespace-mode       : %`whitespace-mode
[_y_] yasnippet-mode        : %(bound-and-true-p yas-minor-mode)
------------------------------------------------------------
"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" display-fill-column-indicator-mode)
  ("g" glasses-mode)
  ("h" hl-line-mode)
  ("i" (lambda ()
         (interactive)
         (setq indent-tabs-mode (not indent-tabs-mode))))
  ("l" my/toggle-line-number)
  ("o" outline-minor-mode)
  ("p" prettify-symbols-mode)
  ("r" rainbow-mode)
  ("s" smartparens-strict-mode)
  ("t" toggle-truncate-lines)
  ("v" view-mode)
  ("V" visual-line-mode)
  ("w" whitespace-mode)
  ("y" yas-minor-mode)
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c h t") #'my/hydra-toggle/body)

;; ---------------------------------------------------------
;; window
;; ---------------------------------------------------------
;; https://github.com/abo-abo/hydra/wiki/Window-Management
;; helpers from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

(defun my/hydra-move-splitter-left (arg)
  "Move ARG window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
    (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun my/hydra-move-splitter-right (arg)
  "Move window ARG splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
    (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun my/hydra-move-splitter-up (arg)
  "Move window ARG splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
    (enlarge-window arg)
    (shrink-window arg)))

(defun my/hydra-move-splitter-down (arg)
  "Move window ARG splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
    (shrink-window arg)
    (enlarge-window arg)))

(defhydra my/hydra-window (:hint nil)
  "
+-^^-----------+-^^-------------+-^^-----------+-^^----------+-^^---------------+
| ^Movement^   | ^Split^        | ^Switch^     | ^Resize^    | ^scroll^         |
+-^^-----------+-^^-------------+-^^-----------+-^^----------+-^^---------------+
| _h_ Left     | _v_ertical     | _b_uffer     | _q_ X left  | _;_ X up         |
| _j_ Down     | _x_ horizontal | _f_ind files | _w_ X Down  | _'_ X down       |
| _k_ Top      | _z_ undo       | _a_ce        | _e_ X Top   | _,_ other X up   |
| _l_ Right    | _Z_ reset      | _s_wap       | _r_ X Right | _._ other X down |
| _F_ollow     | _D_elete Other | _S_ave       | max_i_mize  | ^^               |
| _SPC_ cancel | _o_nly this    | _d_elete     | _B_alance   | ^^               |
|-^^-----------+-^^-------------+-^^-----------+-^^----------+-^^---------------+
"
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
           #'my/hydra-window/body)))
  ("b" ivy-switch-buffer)
  ("B" balance-windows)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
           #'my/hydra-window/body)))
  ("e" my/hydra-move-splitter-up)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("h" windmove-left)
  ("i" ace-delete-other-windows)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("o" delete-other-windows)
  ("q" my/hydra-move-splitter-left)
  ("r" my/hydra-move-splitter-right)
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
           #'my/hydra-window/body)))
  ("S" save-buffer)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("w" my/hydra-move-splitter-down)
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  (";" scroll-down-command)
  ("'" scroll-up-command)
  ("," my/scroll-other-window-up)
  ("." scroll-other-window)
  ("SPC" nil))

(global-set-key (kbd "C-c h w") #'my/hydra-window/body)

;; ---------------------------------------------------------
;; parenthesis
;; ---------------------------------------------------------
;; https://ebzzry.io/en/emacs-pairs/
;; https://emacs-china.org/t/paredit-smartparens/6727/27

(defhydra my/hydra-paredit (:hint nil :color amaranth)
  "
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
| ^MOVEMENT^      | ^forward^    | ^backward^    | ^up^        | ^down^        | ^EDIT^   | ^slurp^   | ^barf^    | ^unwrap^        | ^kill^           |
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
| ^All units are^ | _f_: forward | _b_: backward | _u_: b-w-up | _d_: down     | _,_ undo | _[_: <--( | _{_: (--> | _w_: unwrap     | _k_: kill        |
| ^S-expressions^ | _n_: next    | _p_: prev     | _U_: up     | _D_: b-w-down | _._ redo | _]_: )--> | _}_: <--) | _W_: b-w-unwrap | _K_: kill-hybrid |
| _SPC_ cancel    | _e_: end     | _a_: begin    | ^^          | ^^            | _t_ tran | ^^        | ^^        | _R_: raise      | ^^               |
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
 _i_: enter evil-insert-state | _S_: sp-strict-mode
"
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("d" sp-down-sexp)
  ("D" sp-backward-down-sexp)
  ("u" sp-backward-up-sexp)
  ("U" sp-up-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("[" sp-backward-slurp-sexp)
  ("]" sp-forward-slurp-sexp)
  ("{" sp-backward-barf-sexp)
  ("}" sp-forward-barf-sexp)
  ("w" sp-unwrap-sexp)
  ("W" sp-backward-unwrap-sexp)
  ("k" sp-kill-sexp)
  ("K" sp-kill-hybrid-sexp)
  ("R" sp-raise-sexp)
  ("t" sp-transpose-sexp)
  ("," undo)
  ("." repeat)
  ("i" evil-insert :color blue)
  ("S" smartparens-strict-mode)
  ("SPC" nil :color blue))

(global-set-key (kbd "C-c h p") #'my/hydra-paredit/body)

(defhydra my/hydra-paredit-move (:hint nil :color amaranth)
  "
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+
| ^MOVEMENT^      | ^forward^    | ^backward^    | ^up^        | ^down^        |
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+
| ^All units are^ | _f_: forward | _b_: backward | _u_: b-w-up | _d_: down     |
| ^S-expressions^ | _n_: next    | _p_: prev     | _U_: up     | _D_: b-w-down |
| _SPC_ cancel    | _e_: end     | _a_: begin    | ^^          | ^^            |
+-^^--------------+-^^-----------+-^^------------+-^^----------+-^^------------+
 _i_: enter evil-insert-state | _S_: sp-strict-mode
"
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("d" sp-down-sexp)
  ("D" sp-backward-down-sexp)
  ("u" sp-backward-up-sexp)
  ("U" sp-up-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("i" evil-insert :color blue)
  ("S" smartparens-strict-mode)
  ("SPC" nil :color blue))

(global-set-key (kbd "C-c h M") #'my/hydra-paredit-move/body)

(defhydra my/hydra-paredit-edit (:hint nil :color amaranth)
  "
+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
| ^EDIT^   | ^slurp^   | ^barf^    | ^unwrap^        | ^kill^           |
+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
| _,_ undo | _[_: <--( | _{_: (--> | _w_: unwrap     | _k_: kill        |
| _._ redo | _]_: )--> | _}_: <--) | _W_: b-w-unwrap | _K_: kill-hybrid |
| _t_ tran | ^^        | ^^        | _R_: raise      | ^^               |
+-^^-------+-^^--------+-^^--------+-^^--------------+-^^---------------+
 _SPC_ cancel | _i_: enter evil-insert-state | _S_: sp-strict-mode
"
  ("[" sp-backward-slurp-sexp)
  ("]" sp-forward-slurp-sexp)
  ("{" sp-backward-barf-sexp)
  ("}" sp-forward-barf-sexp)
  ("w" sp-unwrap-sexp)
  ("W" sp-backward-unwrap-sexp)
  ("R" sp-raise-sexp)
  ("k" sp-kill-sexp)
  ("K" sp-kill-hybrid-sexp)
  ("t" sp-transpose-sexp)
  ("," undo)
  ("." repeat)
  ("i" evil-insert :color blue)
  ("S" smartparens-strict-mode)
  ("SPC" nil :color blue))

(global-set-key (kbd "C-c h E") #'my/hydra-paredit-edit/body)

;; ---------------------------------------------------------
;; file
;; ---------------------------------------------------------

(defhydra my/hydra-file (:hint nil)
  "
+-^^^-----------------+-^^^---------------+
| ^useful^^           | ^misc^^           |
+-^^^-----------------+-^^^---------------+
| ^_y_ank file name   | ^_s_udo this file |
| ^_c_opy this file   | ^_S_udo find file |
| ^_d_elete this file | ^_b_rowser file   |
| ^_R_evert this file | ^_o_pen file      |
| ^_r_ename this file | ^DOS _e_ol remove |
| ^_m_ove this file   | ^DOS _E_ol hidden |
| ^save as _u_tf8     | ^_f_ormat buffer  |
+-^^^-----------------+-^^^---------------+
  ^_q_uit
"
  ("b" my/browse-this-file)
  ("c" my/copy-this-file :color blue)
  ("d" my/delete-this-file :color blue)
  ("e" my/remove-dos-eol)
  ("E" my/hide-dos-eol)
  ("f" my/format-region-or-buffer)
  ("o" my/open-this-file-externally :color blue)
  ("R" my/revert-this-buffer :color blue)
  ("r" my/rename-this-file :color blue)
  ("s" my/sudo-edit-file)
  ("S" my/sudo-find-file)
  ("m" my/move-this-file)
  ("u" my/save-file-as-utf8)
  ("y" my/copy-file-name)
  ("q" nil))

(global-set-key (kbd "C-c h f") #'my/hydra-file/body)

;; ---------------------------------------------------------
;; theme
;; ---------------------------------------------------------

(defhydra my/hydra-theme (:hint nil)
  "
+-^^^-------------+-^^^-----------+
| ^^^load         | ^^^cycle      |
+-^^^-------------+-^^^-----------+
| ^_r_andom theme | ^_n_ext theme |
| ^_d_ark theme   | ^_p_rev theme |
| ^_b_lack theme  | ^^^           |
| ^_l_ight theme  | ^^^           |
| ^_L_oad theme   | ^^^           |
+-^^^-------------+-^^^-----------+
 _q_uit
"
  ("n" (my//cycle-theme (1+ my-current-theme-index)))
  ("p" (my//cycle-theme (1- my-current-theme-index)))
  ("r" (my//random-theme my-theme-alist))
  ("d" (my//random-theme my-dark-theme-alist))
  ("l" (my//random-theme my-light-theme-alist))
  ("b" (my//random-theme my-black-theme-alist))
  ("L" my/load-theme)
  ("q" nil))

(global-set-key (kbd "C-c h T") #'my/hydra-theme/body)

(provide 'init-hydra)

;;; init-hydra.el ends here
