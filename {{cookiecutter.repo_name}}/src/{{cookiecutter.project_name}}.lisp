(in-package :{{ cookiecutter.project_name }})

;; Define your project functionality here...

(defparameter +version+ "0.0.1") ;; xxx: read from .asd

(defun print-system-info (&optional (stream t))
  ;; see also https://github.com/40ants/cl-info
  (format stream "~&OS: ~a ~a~&" (software-type) (software-version))
  (format stream "~&Lisp: ~a ~a~&" (lisp-implementation-type) (lisp-implementation-version))
  #+asdf
  (format stream "~&ASDF: ~a~&" (asdf:asdf-version))
  #-asdf
  (format stream "NO ASDF!")
  #+quicklisp
  (format stream "~&Quicklisp: ~a~&" (ql-dist:all-dists))
  #-quicklisp
  (format stream "!! Quicklisp is not installed !!"))

(defun handle-parser-error (c)
  "unix-opts error handler."
  (format t "Argument error: ~a~&" (opts:option c)))

(defun main ()

  (unless (uiop:file-exists-p models::*db-name*)
    (uiop:format! t "Creating the database into ~a...~&" models::*db-name*)
    (models::init-db))

  (opts:define-opts
    (:name :help
           :description "print this help and exit."
           :short #\h
           :long "help")

    (:name :version
           :description "print the version number and exit."
           :short #\v
           :long "version")

    (:name :verbose
           :description "print debug info."
           :short #\V
           :long "verbose")

    (:name :port
           :arg-parser #'parse-integer
           :description "set the port for the web server. You can also use the XYZ_PORT environment variable."
           :short #\p
           :long "port"))

  (multiple-value-bind (options free-args)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (format t "{{ cookiecutter.project_name }} version ~a~&" +version+)

    (when (getf options :version)
      (print-system-info)
      (uiop:quit))

    (when (getf options :help)
      (opts:describe)
      (uiop:quit))

    (when (getf options :verbose)
      (print-system-info))

    ;; Run the web server and catch errors.
    (handler-case
        (progn
          ;; Load the init config file:
          (web::load-config)

          (web:start-app :port (or (getf options :port)
                                   (ignore-errors (parse-integer (uiop:getenv "XYZ_PORT")))
                                   web::*port*))

          ;; XXX: that's only needed for the binary, not from sources.
          ;; Put the server thread on the foreground.
          ;; Without this, the binary exits immediately after having
          ;; run the web server in its thread.
          (bt:join-thread
           (find-if (lambda (th)
                      (search "hunchentoot" (bt:thread-name th)))
                    (bt:all-threads))))

      ;; Catch some errors.
      (usocket:address-in-use-error ()
        (format *error-output* "This port is already taken.~&"))
      #+sbcl
      (sb-sys:interactive-interrupt ()
        (format *error-output* "~&Bye!~&")
        (uiop:quit))
      (error (c)
        (format *error-output* "~&An error occured: ~a~&" c)
        (uiop:quit 1)))))
