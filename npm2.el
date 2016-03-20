;;; npm2.el --- NPM Node Package Manager

;; Copyright (C) 2016 Emil Gjørup

;; Author: Emil Gjørup <limemloh@gmail.com>
;; Keywords: npm

;;; Version: 0.0.1

;;; code:

(require 'json)


;; Helpers


(defun npm2-create-package-json-string (name version description)
  "Creates the package.json string"
  (json-encode-list
   (list (cons "name" name)
         (cons "version" version)
         (cons "description" description)
         '(main . "index.js")
         '(scripts . ((test . "echo \"Error: no test specified\" && exit 1")))
         (cons "author" (format "%s <%s>" user-full-name user-mail-address))
         '(license . "ISC"))))



(defun npm2-init-package (name version description)
  "Creates the package.json"
  (interactive
   (let ((inputstring (lambda (text default) (read-string (concat text " (default " default "): ") nil nil default)))
         ())
     (list (funcall inputstring "Enter project name" "NPM")
           (funcall inputstring "Enter project version" "0.0.1")
           (funcall inputstring "Enter project description" ""))))
  (let ((json-encoding-pretty-print t))
    (with-temp-file "package.json"
      (insert (create-package-json-string name version description)))))


(provide 'npm2)
;;; npm2.el ends here
