;;;; A small tool for generating concept-exercise scaffolding
(defpackage scaffolder
  (:use :cl)
  (:export :make-scaffold))
(in-package :scaffolder)

;;; Load required libraries
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :alexandria) ; Loading files to a string
  (ql:quickload :cl-ppcre)) ; Regex for search-and-replace

;;; Declare *SLUG* as a dynamic variable
(defvar *slug*)

;;; Helper functions
(defun fill-template (template)
  "Searches for placeholders within a template string and replaces them with the appropriate value"
  (let ((mapping `(("SLUG" . ,*slug*)))) ; More could be added
    (reduce (lambda (str map) (cl-ppcre:regex-replace-all (car map) str (cdr map)))
            mapping :initial-value template)))

(defun list-file-tree (directory)
  "Lists all files in a directory and its subdirectories (recursively)"
  (let ((files nil))
    (uiop:collect-sub*directories directory t t
      (lambda (subdir) (push (uiop:directory-files subdir) files)))
    (apply #'append files)))

(defun load-template-files (pathnames)
  "Loads a list of template pathnames, returning an alist of template-filled pathnames and files"
  (mapcar (lambda (path) (cons (fill-template (namestring path))
                               (fill-template (alexandria:read-file-into-string path))))
          pathnames))

;;; Scaffold generator for REPL use
(defun make-scaffold (slug &optional (destination "../../exercises/concept/") (template-path "template/"))
  "Creates a new concept exercise with the given slug in the specified directory"
  (let* ((*slug* slug)
         (file-list (load-template-files (list-file-tree template-path))))
    (loop :for f :in file-list :do
      (let* ((relative-path (enough-namestring (car f) (truename template-path)))
             (output-directory (format nil "~a/~a/" (truename destination) slug))
             (filename (merge-pathnames relative-path output-directory)))
        (format t "Creating ~a...~%" (enough-namestring filename (truename "../../")))
        (ensure-directories-exist filename)
        (with-open-file (fs filename :direction :output :if-does-not-exist :create)
          (write-string (cdr f) fs))))))

;;; Main entrypoint
(defun main ()
  (apply #'make-scaffold (uiop:command-line-arguments))
  (uiop:quit))
