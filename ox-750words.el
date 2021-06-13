;;; ox-750words.el --- Org mode exporter for 750words.com -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Diego Zamboni
;;
;; Author: Diego Zamboni <https://github.com/zzamboni>
;; Maintainer: Diego Zamboni <diego@zzamboni.org>
;; Created: June 10, 2021
;; Modified: June 10, 2021
;; Version: 0.0.1
;; Keywords: files, org, writing
;; Homepage: https://github.com/zzamboni/750words-client
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An Org exporter which converts Org to Markdown and posts it to 750words.com.
;;
;; See https://github.com/zzamboni/750words-client for full usage instructions.
;;
;;; Code:

(require '750words)
(require 'ox-md)

(org-export-define-derived-backend '750words 'md
  :menu-entry
  '(?m 1
       ((?7 "Post to 750words.com"
            (lambda (_a s v _b) (750words-export-to-750words s v))))))

(defun 750words-export-to-750words (subtreep visible-only)
  "Post Org text to 750words.com.

The Org buffer is first converted to Markdown using ox-md, and
the result posted to 750words.com.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements."
  (let* ((outfile (make-temp-file "ox-750words"))
         (org-export-with-smart-quotes nil))
    (org-export-to-file 'md outfile nil subtreep visible-only)
    (750words-file outfile)))

(provide 'ox-750words)
;;; ox-750words.el ends here
