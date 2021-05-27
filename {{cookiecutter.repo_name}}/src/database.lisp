(in-package :{{ cookiecutter.project_name}}/models)
;;;
;;; DB connection, migrations.
;;;

(defparameter *tables* '(product)
  "List of the DB tables that need to be checked for migrations.")

(defun connect (&optional (db-name *db-name*))
  "Connect to the DB."
  ;; *db* could be mito:*connection*
  (log:debug "connecting to ~a~&" *db-name*)
  (setf *db* (mito:connect-toplevel :sqlite3 :database-name db-name)))

(defun ensure-tables-exist ()
  "Run SQL to create the missing tables."
  (unless mito::*connection*
    (connect))
  (mapcar #'mito:ensure-table-exists *tables*))

(defun migrate-all ()
  "Migrate the tables after we changed the class definition."
  (mapcar #'mito:migrate-table *tables*))

;;
;; Entry points
;;
(defun init-db ()
  "Connect to the DB, run the required migrations and define a couple base user roles."
  (ensure-tables-exist))
