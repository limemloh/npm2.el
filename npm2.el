;;; npm2.el --- Useful tools for npm (Node package manager)
;;; Version: 0.0.1

;; Copyright (C) 2016 Emil Gjørup
;; Author: Emil Gjørup <limemloh@gmail.com>
;; Created: 21 Mar 2016
;; Keywords: npm
;; Package-Requires: ((f "0.18.1"))

;;; Commentary:
;;; code:


(require 'json)
(require 'f)

(defun npm2--find-project-root-directory-name ()
  "Find the project root directory."
  (file-name-nondirectory
   (directory-file-name default-directory)))


(defun npm2-init-package (name version description)
  "Initialise project and create a package.json.
NAME is a the project name.
VERSION is the project version.
DESCRIPTION is the project description."
  (interactive
   (if (and
        (f-exists? "package.json")
        (not (y-or-n-p "package.json already exist. Overwrite?")))
       (list nil nil nil)
     (list
      (let ((project-root-directory-name (npm2--find-project-root-directory-name)))
        (read-string (concat "Enter project name (default " project-root-directory-name "): ") nil nil project-root-directory-name))
      (read-string "Enter project version: " "0.0.1")
      (read-string "Enter project description: "))))
  (if (and name version description)
      (let ((json-encoding-pretty-print t))
        (with-temp-file "package.json"
          (insert (json-encode-list
                   (list (cons "name" name)
                         (cons "version" version)
                         (cons "description" description)
                         '(main . "index.js")
                         '(scripts . ((test . "echo \"Error: no test specified\" && exit 1")))
                         (cons "author" (format "%s <%s>" user-full-name user-mail-address))
                         '(license . "ISC"))))))))



(provide 'npm2)
;;; npm2.el ends here
