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
(defparameter +dashboard.html+ (djula:compile-template* "dashboard.html"))

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

;; Root route.
(defroute home-route ("/") ()
  (djula:render-template* +dashboard.html+ nil
                          :route "/"))

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

(defun find-config ()
  (cond
    ((uiop:file-exists-p "config.lisp")
     "config.lisp")
    (t
     nil)))

(defun load-config ()
  "Load `config.lisp', situated at the project's root."
  (let ((file (find-config)))
    (if file
        ;; One case of failure: a symbolic link exists, but
        ;; the target file doesn't.
        (progn
          (uiop:format! t "Loading config file ~a…~&" file)
          (load (uiop:native-namestring file)))
        (format t "... no config file found.~&"))))

(defun start-app (&key (port *port*) (load-config-p nil))
  "Start the Hunchentoot web server on port PORT (defaults to `*PORT*'), serve static assets.

  If LOAD-CONFIG-P is non nil, load the config file (this is normally done in the main function of run.lisp before)."
  ;; You can use the find-port library to find an available port.

  ;; Load the config.lisp init file.
  (if load-config-p
      (load-config)
      (uiop:format! t "Skipping config file."))

  ;; Set up the DB.
  (models:connect)

  ;; Start the server.
  (uiop:format! t "Starting Hunchentoot on port ~a…~&" port)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (serve-static-assets)
  (uiop:format! t "~&Application started on port ~a.~&" port))

(defun stop-app ()
  ;; disconnect db ?
  (hunchentoot:stop *server*))
