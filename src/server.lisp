(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.server
    (:import-from :realispic
                  :def-app
                  :def-service)
    (:export :dex)
    (:use :cl)))
(in-package :mh-dex.server)


;; -------------------- Weapon --------------------
(def-service get-weapon (type base-id level)
  (list :type type
        :base base-id
        :level level))

(def-app dex ()
  :title "Ping's Dex"
  :port 5120
  :system :mh-dex
  :widget (:div ()))
  





