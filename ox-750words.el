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
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
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
            (lambda (_a s v _b) (org-750words-export-to-750words s v))))))

(defun org-750words-export-to-750words (subtreep visible-only)
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
