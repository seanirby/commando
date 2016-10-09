(defvar commando-file-name ".commando")

(defun command-plist-take-pair (plist)
  (let ((k (car plist))
        (v (cadr plist)))
    (list k v)))

(defun commando-plist-keys (plist)
  (cl-labels ((inner (plist)
                     (let* ((pair (command-plist-take-pair plist))
                            (first (car pair)))
                       (if (keywordp first)
                           (cons first (inner (cdr (cdr plist))))))))
    (if (evenp (length plist))
        (inner plist)
      (user-error "plist must be even"))))

(defun commando-plist-values (plist)
  (let ((plist-length (length plist)))
    (cl-labels ((inner (plist count)
                       (let* ((pair (command-plist-take-pair plist))
                              (value (cadr pair)))
                         (if (equal count plist-length)
                             nil
                           (cons value (inner (cdr (cdr plist)) (+ count 2)))))))
      (if (evenp (length plist))
          (inner plist 0)
        (user-error "plist must be even")))))

(defun commando-key-to-string (key)
  (substring (symbol-name key) 1))

(defun commando-run-task ()
  "Prompts the user to select a task to run from the current projects commando directory."
  (interactive)
  (let ((commando-file (commando-project-p)))
    (if commando-file
        (let* ((tasks (commando-build-tasks commando-file))
               (task (completing-read "Select a task: " (mapcar 'commando-key-to-string (commando-plist-keys tasks))))
               (commando/project-root (directory-file-name commando-file))
               (commando/current-file (buffer-file-name))
               (commando/current-directory (directory-file-name commando/current-file)))
          (eval (plist-get tasks (intern (concat ":" task)))))
      (user-error "You are not in a commando project, no .commando file was found"))))


(defun commando-find-root (starting-directory)
  "Searches up from STARTING-DIRECTORY to find a commando file.  Returns that path if found, otherwise returns nil."
  (let ((dir (directory-file-name starting-directory)))
    (if (equal dir "/")
        nil
      (let ((commando-file (format "%s%s" starting-directory commando-file-name)))
        (if (file-exists-p commando-file)
            commando-file
          (commando-find-root (file-name-directory dir)))))))

(defun commando-project-p ()
  "Returns path to current project's commando-file.  Otherwise returns nil"
  (let ((filename (buffer-file-name)))
    (if filename
        (commando-find-root (file-name-directory filename)))))

(defmacro commando-add-task (name code)
  `(list ,name (quote ,code)))

(defun commando--build-tasks (forms)
  "Recurse through FORMS to build tasks plist."
  (if (null forms)
      nil
    (let ((result (eval (car forms))))
      (cons (car result) (cons (cadr result) (commando--build-tasks (cdr forms)))))))

(defun commando-build-tasks (commando-file)
  "Return a list of tasks defined in COMMANDO-FILE."
  (let* ((forms (commando-read-commando-file commando-file)))
    (commando--build-tasks forms)))

(defun commando-read-commando-file (commando-file)
  "Return the list of forms defined in COMMANDO-FILE."
  (let ((contents (commando-file-contents commando-file)))
    (car (read-from-string (format "(%s)" contents)))))

(defun commando-file-contents (commando-file)
  "Returs the contents of COMMANDO-FILE as a string."
  (with-temp-buffer
    (insert-file-contents commando-file)
    (buffer-string)))

(provide 'commando)
