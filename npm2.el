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

(defun npm2-init-package (name version author description main-file license)
  "Creates a package.json.
NAME is the project name.
VERSION is the project version.
AUTHOR the name of the author.
DESCRIPTION is the project description.
MAIN-FILE the project mainfile.
LICENSE the project license"
  (interactive
   (if (and
        (f-exists? "package.json")
        (not (y-or-n-p "package.json already exist. Overwrite?")))
       (list nil nil nil nil nil nil)
     (let ((project-root-directory-name (file-name-nondirectory (directory-file-name default-directory)))
           (author (format "%s <%s>" user-full-name user-mail-address)))
       (list (read-string (concat "Enter project name (default " project-root-directory-name "): ") nil nil project-root-directory-name)
             (read-string "Enter project version: " "0.0.1" nil "0.0.1")
             (read-string (concat "Enter name of the author (default " author "): ") nil nil author t)
             (read-string "Enter project description: ")
             (read-string "Enter project main file (default index.js): " nil nil "index.js")
             (read-string "Enter project license (default ISC): " nil nil "ISC")))))
  (if (and name version description author description)
      (let ((json-encoding-pretty-print t))
        (with-temp-file "package.json"
          (insert (json-encode-list
                   (list (cons "name" name)
                         (cons "version" version)
                         (cons "author" author)
                         (cons "description" description)
                         (cons "main" main-file)
                         '(scripts . ((test . "echo \"Error: no test specified\" && exit 1")))
                         (cons "license" license)))
                  )))))



(provide 'npm2)
;;; npm2.el ends here
