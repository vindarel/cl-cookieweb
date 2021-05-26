;;;
;;; define helper packages,
;;; the models,
;;; the web,
;;; and the base package that relies on all of them.
;;;

(defpackage {{ cookiecutter.project_name }}/utils
  (:use :cl
        :log4cl)
  (:export #:format-date
           #:i18n-load
           #:_
           #:parse-iso-date)
  (:documentation "Utilities that do not depend on models."))

(defpackage {{ cookiecutter.project_name }}/models
  (:use :cl)
  (:export :connect
           :make-product
           :select-products
           :find-by))

(defpackage {{ cookiecutter.project_name }}/web
  (:use :cl)
  (:import-from :easy-routes
                :defroute)
  (:export :start-app
           :stop-app)
  (:local-nicknames (#:a #:alexandria)
                    (#:models #:{{ cookiecutter.project_name }}/models)
                    (#:utils #:{{ cookiecutter.project_name}}/utils)))

(defpackage {{ cookiecutter.project_name }}
  (:use :cl
        :log4cl)
  (:export :main)
  (:local-nicknames (#:a #:alexandria)
                    (#:models #:{{ cookiecutter.project_name }}/models)
                    (#:web #:{{ cookiecutter.project_name }}/web)
                    (#:utils #:{{ cookiecutter.project_name}}/utils)))
