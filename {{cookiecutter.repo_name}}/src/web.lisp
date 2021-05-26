(in-package :{{ cookiecutter.project_name}}/web)

(defvar *server* nil
  "Current instance of easy-acceptor.")

(defparameter *port* 4242)

;;;
;;; Djula filters.
;;;

(djula:def-filter :price (val)
  (format nil "~,2F" val))

;;;
;;; Load templates.
;;;
(djula:add-template-directory
 (asdf:system-relative-pathname "{{ cookiecutter.project_name }}" "src/templates/"))

(defparameter +base.html+ (djula:compile-template* "base.html"))

; (defparameter +404.html+ (djula:compile-template* "404.html"))

;;;
;;; Serve static assets
;;;
(defparameter *default-static-directory* "src/static/"
  "The directory where to serve static assets from (STRING). If it starts with a slash, it is an absolute directory. Otherwise, it will be a subdirectory of where the system :abstock is installed.
  Static assets are reachable under the /static/ prefix.")

(defun serve-static-assets ()
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *default-static-directory*
                                     (asdf:system-source-directory :{{ cookiecutter.project_name }})))
        hunchentoot:*dispatch-table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Access roles.
;; (models:define-role-access home-route :view :visitor)
;; (models:define-role-access add-or-create-route :view :visitor)

;; Root route.
(defroute home-route ("/") ()
  (render-template* +base.html+ nil
                    :route "/"
                    :current-user (current-user)
                    :data (list :products (models:select-products))))

(defroute card-page ("/product/:slug")
    (&get raw)
  "Show a product.

  Dev helper: if the URL parameter RAW is \"t\" (the string), then display the card object literally (with describe)."
  ;; The product URL is of the form: /xyz-product-title where xyz is its pk.
  (let* ((product-id (ignore-errors
                       (parse-integer (first (str:split "-" slug)))))
         (product (when product-id
                 (models:find-by :id product-id))))
    (cond
      ((null product-id)
       (render-template* +404.html+ nil))
      (product
       (render-template* +product-stock.html+ nil
                         :messages nil
                         :route "/product"
                         :product product
                         :raw raw))
      (t
       (render-template* +404.html+ nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start-up functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-app (&key (port *port*))
  (models:connect)

  ;; fix a puri bug. puri:parse-uri "/login?referer-route=/stock?q=lisp" fails,
  ;; it doesn't like the last ?. See https://gitlab.common-lisp.net/clpm/puri/-/issues/2
  (setf puri::*strict-illegal-query-characters*
        (remove #\? puri::*strict-illegal-query-characters*))

  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (serve-static-assets)
  (uiop:format! t "~&Application started on port ~a.~&" port))

(defun stop-app ()
  ;; disconnect db ?
  (hunchentoot:stop *server*))
