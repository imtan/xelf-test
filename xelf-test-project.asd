;;;; xelf-test-project.asd

(asdf:defsystem #:xelf-test-project
  :description "Describe xelf-test-project here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:xelf)
  :serial t
  :components ((:file "package")
               (:file "xelf-test-project")))

