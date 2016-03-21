;;; npm2.el --- NPM Node Package Manager
;;
;; Copyright (C) 2016 Emil Gjørup
;;
;; Author: Emil Gjørup <limemloh@gmail.com>
;; Keywords: npm
;;
;;; Version: 0.0.1
;;
;;; Commentary:
;;
;;
;;
;;; code:

(require 'json)


(defun npm2--find-project-root-directory ()
  "Find the project root directory."
  (file-name-nondirectory
   (directory-file-name default-directory)))


(defun npm2-init-package (name version description)
  "Initialise project and create a package.json.
NAME is a the project name.
VERSION is the project version.
DESCRIPTION is the project description."
  (interactive
   (let ((project-root-directory (npm2--find-project-root-directory)))
     (list (read-string (concat "Enter project name (default " project-root-directory "): ") nil nil project-root-directory)
           (read-string "Enter project version: " "0.0.1")
           (read-string "Enter project description: "))))
  (let ((json-encoding-pretty-print t))
    (with-temp-file "package.json"
      (insert (json-encode-list
               (list (cons "name" name)
                     (cons "version" version)
                     (cons "description" description)
                     '(main . "index.js")
                     '(scripts . ((test . "echo \"Error: no test specified\" && exit 1")))
                     (cons "author" (format "%s <%s>" user-full-name user-mail-address))
                     '(license . "ISC")))))))



(provide 'npm2)
;;; npm2.el ends here
