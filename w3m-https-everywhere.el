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

(eval-when-compile
  (require 'cl))

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

(defun w3m-https-everywhere-parse-rule (node)
  (list (replace-regexp-in-string "[()|]"
				  "\\\\\\&"
				  (xml-get-attribute node 'from))
	'w3m-pattern-uri-replace
	(replace-regexp-in-string "\\$\\([[:digit:]]\\)"
				  "\\\\\\1"
				  (xml-get-attribute node 'to))))

(defun w3m-https-everywhere-parse-ruleset (node)
  (mapcar 'w3m-https-everywhere-parse-rule
	  (xml-get-children node 'rule)))

(defun w3m-https-everywhere-parse-rulesetlibrary (node)
  (mapcan 'w3m-https-everywhere-parse-ruleset
	  (xml-get-children node 'ruleset)))

(defun w3m-https-everywhere-parse-file (file)
  (let ((node (xml-node-name (xml-parse-file file))))
    (case (car node)
      (rulesetlibrary (w3m-https-everywhere-parse-rulesetlibrary node))
      (ruleset (w3m-https-everywhere-parse-ruleset node)))))

;;;###autoloads
(defun w3m-https-everywhere ()
  (mapcan 'w3m-https-everywhere-parse-file
	  (mapcan (lambda (directory)
		    (when (file-directory-p directory)
		      (directory-files directory t "^[^.]")))
		  (list w3m-https-everywhere-user-directory
			w3m-https-everywhere-system-directory))))

(provide 'w3m-https-everywhere)

;;; w3m-https-everywhere.el ends here
