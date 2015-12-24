(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.server
    (:import-from :realispic
                  :def-app
                  :def-service)
    (:import-from :mh-dex.weapon
                  :reload-weapons
                  :get-weapon-list
                  :get-weapon-type-list)
    (:export :dex
             :init-server)
    (:use :cl)))
(in-package :mh-dex.server)

;; -------------------- Operations --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *initialized* nil
    "Inidicating that the server is fully initialized.")

  (defun init-server ()
    "Load all the data from the Dex databse to initialzie the server."
    (unless *initialized*
      (reload-weapons)
      (setf *initialized* t))))

;; -------------------- RPCs --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-dex-service (name args &body body)
    `(def-service ,name ,args
       (init-server)
       ,@body)))

(def-dex-service environment ()
  (list :weapontypes (get-weapon-type-list)))

(def-dex-service weapon-list (type)
  (get-weapon-list (parse-integer type)))



(def-app dex ()
  :title "Ping's Dex"
  :port 5120
  :system :mh-dex
  :widget (:div ()))
  





