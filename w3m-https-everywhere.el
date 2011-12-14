;;; w3m-https-everywhere.el --- encrypting your communications with a number of major websites

;; Copyright (C) 2011 Hironori OKAMOTO

;; Author: Hironori OKAMOTO <k.ten87@gmail.com>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'regexp-opt)
(require 'xml)
(require 'w3m)

(defgroup w3m-https-everywhere nil
  ""
  :group 'w3m
  :prefix "w3m-https-everywhere-")

(defcustom w3m-https-everywhere-system-directory
  "/usr/share/xul-ext/https-everywhere/chrome/content/rules"
  ""
  :type 'directory
  :group 'w3m-https-everywhere)

(defcustom w3m-https-everywhere-user-directory
  (expand-file-name "https-everywhere"
		    w3m-profile-directory)
  ""
  :type 'directory
  :group 'w3m-https-everywhere)

(defun w3m-https-everywhere-regexp-js-to-elisp (string)
  (replace-regexp-in-string "[(){|}]"
			    "\\\\\\&"
			    string))

(defun w3m-https-everywhere-newtext-js-to-elisp (string)
  (replace-regexp-in-string "\\$\\([[:digit:]]\\)"
			    "\\\\\\1"
			    string))

(defun w3m-https-everywhere-pattern-uri-replace (uri alist)
  (let ((to (cdr (assoc-if '(lambda (from)
			      (string-match from uri))
			   alist))))
    (if to
	(replace-match to nil nil uri)
      uri)))

(defun w3m-https-everywhere-parse-target (node)
  (xml-get-attribute node 'host))

(defun w3m-https-everywhere-parse-exclusion (node)
  (cons (w3m-https-everywhere-regexp-js-to-elisp
	 (xml-get-attribute node 'pattern))
	"\\&"))

(defun w3m-https-everywhere-parse-rule (node)
  (cons (w3m-https-everywhere-regexp-js-to-elisp (xml-get-attribute node 'from))
	(w3m-https-everywhere-newtext-js-to-elisp (xml-get-attribute node
								     'to))))

(defun w3m-https-everywhere-parse-ruleset (node)
  (list (format "^https?://%s/"
		(replace-regexp-in-string
		 "\\*"
		 "[^./]+"
		 (regexp-opt (mapcar 'w3m-https-everywhere-parse-target
				     (xml-get-children node 'target))
			     'paren)))
	'w3m-https-everywhere-pattern-uri-replace
	(loop for child in (xml-node-children node)
	      for function = (and (listp child)
				  (case (xml-node-name child)
				    (exclusion
				     'w3m-https-everywhere-parse-exclusion)
				    (rule
				     'w3m-https-everywhere-parse-rule)))
	      when function
	      collect (funcall function child))))

(defun w3m-https-everywhere-parse-rulesetlibrary (node)
  (mapcar 'w3m-https-everywhere-parse-ruleset
	  (xml-get-children node 'ruleset)))

(defun w3m-https-everywhere-parse-file (file)
  (let ((node (car (xml-parse-file file))))
    (case (xml-node-name node)
      (rulesetlibrary (w3m-https-everywhere-parse-rulesetlibrary node))
      (ruleset (list (w3m-https-everywhere-parse-ruleset node))))))

;;;###autoload
(defun w3m-https-everywhere ()
  (mapcan 'w3m-https-everywhere-parse-file
	  (mapcan (lambda (directory)
		    (when (file-directory-p directory)
		      (directory-files directory 'full "^[^.]")))
		  (list w3m-https-everywhere-user-directory
			w3m-https-everywhere-system-directory))))

(provide 'w3m-https-everywhere)

;;; w3m-https-everywhere.el ends here
