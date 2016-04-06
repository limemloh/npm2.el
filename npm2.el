;;; npm2.el --- Useful tools for npm (Node package manager)
;;; Version: 0.2.3

;; Copyright (C) 2016 Emil Gjørup
;; Author: Emil Gjørup <limemloh@gmail.com>
;; Created: 21 Mar 2016
;; Keywords: npm
;; Package-Requires: ((f "0.18.1"))

;;; Commentary:
;;; code:


(require 'json)
(require 'f)
(require 'comint)


(defvar npm2-executable-path "npm"
  "Path to the program `npm'. Assumes that it is part of your global path.")

(defvar npm2-cli-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; Keybindings here
    map)
  "map for npm2-mode all with the prefix C-c C-n")

(defun npm2--run-npm-cmd (&rest cmd-list)
  "Run a npm command"
  (let ((buffer (comint-check-proc "npm")))
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'npm-cli-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*npm*"))
       (current-buffer)))
    (apply 'make-comint-in-buffer "npm" buffer npm2-executable-path nil cmd-list)
    (npm2-cli-mode)))

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
  "A wrapper for npm run script.
The function tries to parse the package.json file to find script names."
  (interactive
   (let* ((package-location (locate-dominating-file buffer-file-name "package.json"))
          (package-file (if package-location (json-read-file (f-join package-location "package.json"))))
          (scripts (if package-file
                       (mapcar (lambda (key) (car  key)) (cdr (assoc 'scripts package-file))))))
     (list (completing-read "npm run " scripts))))
  (npm2--run-npm-cmd "run" script-name))

(defun npm2-install-package (package-name)
  "Install a npm package.
This is just a wrapper around the command 'npm install'."
  (interactive
   (save-excursion
     (let ((empty-list ()))
       (when (derived-mode-p 'js-mode)
         (goto-char (point-min))
         (while (search-forward-regexp "require *( *\"" nil t)
           (let ((item (sexp-at-point)))
             (if item
                 (push item empty-list)))))
       (list (completing-read "npm install " empty-list)))))
  (npm2--run-npm-cmd "install" package-name))

(define-derived-mode npm2-cli-mode comint-mode "NPM"
  "Major-mode for npm2-cli"
  (add-hook 'comint-output-filter-functions 'npm2-mode--clean-up-ansi-mess t t))

(defun npm2-mode--clean-up-ansi-mess (&rest ignore)
  (with-current-buffer "*npm*"
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
          (delete-char -3))))))

(provide 'npm2)
;;; npm2.el ends here
