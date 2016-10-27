(defvar commando-file-name ".commando")

(add-to-list 'auto-mode-alist '("\\.commando\\'" . emacs-lisp-mode))

(defun command-plist-take-pair (plist)
  "Return the first key value pair from PLIST."
  (let ((k (car plist))
        (v (cadr plist)))
    (list k v)))

(defun commando-plist-keys (plist)
  "Return the keys of PLIST."
  (cl-labels ((inner (plist)
                     (let* ((pair (command-plist-take-pair plist))
                            (first (car pair)))
                       (if (keywordp first)
                           (cons first (inner (cdr (cdr plist))))))))
    (if (evenp (length plist))
        (inner plist)
      (user-error "PLIST must be even"))))

(defun commando-plist-values (plist)
  "Return the values of PLIST."
  (let ((plist-length (length plist)))
    (cl-labels ((inner (plist count)
                       (let* ((pair (command-plist-take-pair plist))
                              (value (cadr pair)))
                         (if (equal count plist-length)
                             nil
                           (cons value (inner (cdr (cdr plist)) (+ count 2)))))))
      (if (evenp (length plist))
          (inner plist 0)
        (user-error "PLIST must be even")))))

(defun commando-key-to-string (key)
  "Convert KEY to a display string."
  (substring (symbol-name key) 1))

(defun commando-find-root (starting-directory)
  "Search up from STARTING-DIRECTORY to find a commando file.
Return that path if found, otherwise return nil."
  (let ((dir (directory-file-name starting-directory)))
    (if (equal dir "/")
        nil
      (let ((commando-file (format "%s%s" starting-directory commando-file-name)))
        (if (file-exists-p commando-file)
            commando-file
          (commando-find-root (file-name-directory dir)))))))

(defun commando-project-p ()
  "Return path to current project's commando-file.  Otherwise return nil."
  (let ((filename (buffer-file-name)))
    (if filename
        (commando-find-root (file-name-directory filename)))))

(defmacro commando-add-script (name code)
  "Use in a commando file to define a script.

NAME is a keyword identifier for the script name.

CODE is the elisp form which should be executed when the script is
run."
  `(list ,name (quote ,code)))

(defun commando-build-scripts (commando-file)
  "Return a list of scripts defined in COMMANDO-FILE."
  (let* ((forms (commando-read-commando-file commando-file)))
    (cl-labels ((inner (forms)
                       (if (null forms)
                           nil
                         (let ((result (eval (car forms))))
                           (cons (car result) (cons (cadr result) (inner (cdr forms))))))))
      (inner forms))))

(defun commando-read-commando-file (commando-file)
  "Return the list of forms defined in COMMANDO-FILE."
  (let ((contents (commando-file-contents commando-file)))
    (car (read-from-string (format "(%s)" contents)))))

(defun commando-file-contents (commando-file)
  "Return the contents of COMMANDO-FILE."
  (with-temp-buffer
    (insert-file-contents commando-file)
    (buffer-string)))

;;;###autoload
(defun commando-run-script (&optional script)
  "Run a script defined in your commando file.

SCRIPT is an optional argument.  If it's defined then commando will
run that script.  Otherwise, the user will be prompted to choose a
script."
  (interactive)
  (let ((commando-file (commando-project-p)))
    (if commando-file
        (let* ((scripts (commando-build-scripts commando-file))
               (script (if (not (eq script nil))
                           script
                         (completing-read "Select a script: " (mapcar 'commando-key-to-string (commando-plist-keys scripts)))))
               (commando/project-root (file-name-directory commando-file))
               (commando/current-file (buffer-file-name))
               (commando/current-directory (file-name-directory commando/current-file))
               ;; change the default directory temporarily so shell commands start from project root
               (default-directory commando/project-root)) 
          (eval (plist-get scripts (intern (concat ":" script)))))
      (user-error "You are not in a commando project, no .commando file was found"))))

(provide 'commando)
