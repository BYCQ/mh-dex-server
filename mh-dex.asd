(in-package :cl-user)
(defpackage mh-dex-asd
  (:use :cl :asdf))
(in-package :mh-dex-asd)

(defsystem mh-dex
    :version "0.0.5"
    :author "BreakDS <breakds@gmail.com>, Cassandra Qi <cassandraqs@gmail.com>"
    :license "MIT"
    :depends-on (:realispic
                 :sqlite
                 :jonathan)
    :components ((:module "src" 
                          :components
                          ((:file "common")
                           (:file "weapon" :depends-on ("common"))
                           (:file "server" :depends-on ("weapon")))))
    :description "Ping's Monster Hunter Dex server.")
