(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq root-path project-directory))

(add-to-list 'load-path root-path)

(require 'npm2)
(require 'espuds)


(Before
 (switch-to-buffer (get-buffer-create "*npm2*"))
 (erase-buffer))
