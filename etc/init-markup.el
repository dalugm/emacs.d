;;; init-markup.el --- markup languages configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Markup language configuration.
;;

;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-command-map
              ("'" . markdown-edit-code-block)
              ("f" . markdown-footnote-goto-text)
              ("r" . markdown-footnote-return))
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)

  ;; This is set to `nil' by default, which causes a wrong-type-arg error
  ;; when you use `markdown-open'. These are more sensible defaults.
  (markdown-open-command (cond
                          (my-mac-p "open")
                          (my-linux-p "xdg-open")))

  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                        "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
  body {
    box-sizing: border-box;
    max-width: 740px;
    width: 100%;
    margin: 40px auto;
    padding: 0 10px;
  }
</style>

<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
  document.addEventListener('DOMContentLoaded', () => {
    document.body.classList.add('markdown-body');
    document.querySelectorAll('pre code').forEach((code) => {
      if (code.className !== 'mermaid') {
        hljs.highlightBlock(code);
      }
    });
  });
</script>

<script src='https://unpkg.com/mermaid/dist/mermaid.min.js'></script>
<script>
  mermaid.initialize({
    theme: 'default',  // default, forest, dark, neutral
    startOnLoad: true
  });
</script>
")
  (markdown-gfm-additional-languages "Mermaid")
  :hook (markdown-mode . (lambda ()
                           "Don't wrap lines because there are tables in `markdown-mode'"
                           (setq-local truncate-lines t)))
  :config
  (defun my-markdown-demote-or-promote (&optional is-promote)
    "Demote or promote current markdown tree according to IS-PROMOTE."
    (interactive "P")
    (unless (region-active-p)
      (markdown-mark-subtree))
    (if is-promote (markdown-promote) (markdown-demote)))

  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown")))

(use-package yaml-mode
  :mode "\\.\\(yml\\|yaml\\)\\'")

(use-package ox-hugo
  :after ox)

(use-package toc-org
  :hook ((org-mode markdown-mode) . toc-org-mode)
  :config
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map
                (kbd "C-c C-o")
                #'toc-org-markdown-follow-thing-at-point)))

;; Pixel-perfect visual alignment for Org and Markdown tables.
(use-package valign
  :when (display-graphic-p)
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (defun my-valign-fancy-bar ()
    "Toggle valign fancy bar."
    (interactive)
    (setq valign-fancy-bar
          (not valign-fancy-bar)))

  ;; compatible with `outline-mode'
  (define-advice outline-show-entry (:override nil)
    "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
    (interactive)
    (save-excursion
      (outline-back-to-heading t)
      (outline-flag-region (max (point-min) (1- (point)))
                           (progn
                             (outline-next-preface)
                             (if (= 1 (- (point-max) (point)))
                                 (point-max)
                               (point)))
                           nil))))

(provide 'init-markup)

;;; init-markup.el ends here
