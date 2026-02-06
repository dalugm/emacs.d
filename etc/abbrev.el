;; -*- coding: utf-8-unix; lexical-binding: t; -*-

(define-abbrev-table 'global-abbrev-table
  '(("realnum" "\\([A-Za-z0-9]+\\)")
    ("reisodate" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
    ("redigit" "\\([0-9]+\\)")
    ("restr" "\\([^\"]+?\\)")
    ("shebang" "#!/usr/bin/env "))
  "Global abbrevs.")
