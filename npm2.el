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
(require 'compile)

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
       (make-list 6 nil)
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
                         (cons "license" license))))))))

(defun npm2-run-script (script-name)
  "A wrapper for npm run script."
  (interactive
   (let* ((package-file (json-read-file "package.json"))
          (scripts (if package-file
                       (mapcar (lambda (key) (car  key)) (cdr (assoc 'scripts package-file))))))
     (list (completing-read "npm run " scripts))))
  (compile (concat "npm run " script-name) t))

(defun npm2-install-package (package-names)
  "Wrapper for npm install."
  (interactive "snpm install ")
  (compile (concat "npm install " package-names) t ))

(defvar npm2-mode-map (make-sparse-keymap)
  "npm2-mode keymap")

(defun npm2-mode--compilation-buffer-name (&rest ignore) "*npm2*")

(defun npm2-mode--clean-up-ansi-mess (&rest ignore)
  (with-current-buffer (npm2-mode--compilation-buffer-name)
    (save-excursion
      (goto-char (point-min))
      (save-excursion
        (while (search-forward "[1A" nil t)
          (delete-char -5)
          (delete-char (- (current-column)))))
      (save-excursion
        (while (search-forward "[?25h" nil t)
          (delete-char -6)))
      (save-excursion
        (while (search-forward "[?25l" nil t)
          (delete-char -6)))
      (save-excursion
        (while (search-forward "[0G" nil t)
          (delete-char -4)))
      (save-excursion
        (while (search-forward "[0" nil t)
          (delete-char -3)))

      )))

(define-minor-mode npm2-mode
  "npm2-minor-mode --- Bringing npm inside Emacs." nil " npm" npm2-mode-map
  (if npm2-mode
      (progn
        (set (make-local-variable 'compilation-buffer-name-function) 'npm2-mode--compilation-buffer-name)
        (add-hook 'comint-output-filter-functions 'npm2-mode--clean-up-ansi-mess t)
        )
    (remove-hook 'comint-output-filter-functions 'npm2-mode--clean-up-ansi-mess)
    ))



(provide 'npm2)
;;; npm2.el ends here
