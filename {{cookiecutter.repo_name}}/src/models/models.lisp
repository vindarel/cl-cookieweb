
(in-package :{{ cookiecutter.project_name}}/models)

(defparameter *db-name* (asdf:system-relative-pathname :{{ cookiecutter.project_name}} "db.db"))

(defparameter *db* nil
  "DB connection object, returned by (connect).")

;; After modification, run (migrate-all)
;;
;; - to create a date: (local-time:now)
;; "
(defclass product ()
  ((title
    :accessor title
    :initarg :title
    :initform nil
    :type string
    :col-type (:varchar 128))

   (reference
    :accessor reference
    :initarg :reference
    :initform nil
    :type (or string null)
    :col-type (or (:varchar 128) :null))

   (price
    :accessor price
    :initarg :price
    ;; we don't the price to 0 (nil denotes a missing field)
    :initform nil
    :type (or integer null)
    :col-type (or :float :null)
    :documentation "Store prices as integers. $9.80 => 980")

   (quantity
    :accessor quantity
    :initform 1
    :type (or integer null)
    :col-type (or (:integer) :null)
    :documentation "Quantity in stock."))

  (:metaclass mito:dao-table-class)
  (:documentation "A product."))

(defun make-product (&key title reference price)
  "Create a product instance.
  It is not saved in the DB yet."
  (make-instance 'product
                 :title title
                 :reference reference
                 :price price))

(defun select-products (&key (order :asc))
  (mito:select-dao 'product
    (sxql:order-by `(,order :created-at))))

(defun find-by (key val)
  "Find a product by slot. Example: (find-by :id xxx). Return only the first matching result."
  (when val
    (mito:find-dao 'product key val)))
