;;; grain-mode.el --- Major mode for Grain language

;; Copyright (C) 2017 Philip Blair, all rights reserved.
;; Licensed under GNU General Public License

;; Author: Philip Blair <philip@pblair.org>
;; Created: 17 Apr 2017
;; Keywords: languages

;;; Commentary:
;; This mode is heavily inspired by the Pyret major mode.
;; There is currently no support for indentation.

;;; Code:

(defvar grain-keywords '("let" "rec" "import" "match" "data"))

(defvar grain-keywords-regexp (regexp-opt grain-keywords))

(defvar grain-punctuation-regexp
  (regexp-opt '("::" ">" "<" ">=" "<=" "==" "!=" ";" "(" ")" "+" "-" "*" "{" "}" "=>")))

(defvar grain-font-lock-keywords
  `((,(concat "\\_<" grain-keywords-regexp "\\_>") . font-lock-keyword-face)
    (,(concat "\\_<" (regexp-opt '("true" "false") t) "\\_>") . font-lock-constant-face)
    (,grain-punctuation-regexp . font-lock-builtin-face)))

(defvar grain-paragraph-starters
  '("let" "rec" "begin" "lambda"))

(defconst grain-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "< 14b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?' "\"" st)
    st)
  "Syntax table for grain-mode")

(defun grain-mode ()
  "Major mode for editing Grain files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table grain-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(grain-font-lock-keywords))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'paragraph-start)
       (concat "\\|[ \t]*" (regexp-opt grain-paragraph-starters)))
  (setq major-mode 'grain-mode)
  (setq mode-name "Grain")
  (run-hooks 'grain-mode-hook))

(provide 'grain-mode)

;;; grain-mode.el ends here
