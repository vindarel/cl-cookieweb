(in-package :asdf-user)

(defsystem "{{ cookiecutter.project_name }}"
  :description "{{ cookiecutter.description }}"
  :author "{{ cookiecutter.author }} <{{ cookiecutter.email }}>"
  :version "{{ cookiecutter.version }}"
  :license "BSD"
  :serial t

  ;; dependencies
  :depends-on (
               ;; HTTP client
               :dexador

               ;; templates
               :djula

               ;; server, routing
               :hunchentoot
               :easy-routes

               ;; JSON
               :cl-json

               ;; DB
               :mito
               :mito-auth

               ;; utilities
               :access
               :cl-ppcre
               :cl-slug
               :local-time
               :local-time-duration
               :log4cl
               :str

               ;; scripting
               :unix-opts

               ;; development utilities
               )

  ;; build a binary:
  :build-operation "program-op"
  :entry-point "{{ cookiecutter.project_name}}:main"

  :components ((:module "src"
                        :components
                        ;; stand-alone packages.
                        ((:file "packages")
                         (:file "utils")
                         ;; they depend on the above.
                         ;; (:file "authentication")
                         (:file "web")
                         (:file "{{ cookiecutter.project_name }}")
                         (:file "database")))

               (:module "src/models"
                        :components
                        ((:file "models")))))
