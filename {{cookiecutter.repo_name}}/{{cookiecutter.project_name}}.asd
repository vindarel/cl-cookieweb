(in-package :asdf-user)

(defsystem "{{ cookiecutter.project_name }}"
  :author "{{ cookiecutter.author }} <{{ cookiecutter.email }}>"
  :version "{{ cookiecutter.version }}"
  :license "{{ cookiecutter.licence }}"
  :description "{{ cookiecutter.description }}"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
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

               ;; deployment
               :deploy

               ;; development utilities
               )

  ;; Build a binary.
  ;; :build-operation "program-op"  ;; usual op to build a binary.
  ;; Deploy:
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "{{ cookiecutter.project_name }}"
  :entry-point "{{ cookiecutter.project_name}}:run"

  ;; Project stucture.
  :serial t
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
                        ((:file "models")))

               (:static-file "README.md")))

;; Deploy may not find libcrypto on your system.
;; But anyways, we won't ship it to rely instead
;; on its presence on the target OS.
(require :cl+ssl)  ; sometimes necessary.
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)

;; ASDF wants to update itself and fails.
;; Yeah, it does that even when running the binary on my VPS O_o
;; Please, don't.
(deploy:define-hook (:deploy asdf) (directory)
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () NIL))
