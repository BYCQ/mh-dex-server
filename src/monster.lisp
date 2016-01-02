(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.monster
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-monster-key
                  :lang-text)
    (:export :*monsters*
             :reload-monsters
             :get-monster-list)))
(in-package :mh-dex.monster)

(defparameter *monsters* nil)

(defun reload-monsters ()
  "Reload the variable *monsters* which contains all the monster
   information."

  (setf *monsters* nil)

  (with-dex-queries ((monsters (:select "ID_Mon_Name.Mon_ID"
                                        ;; names: 0 = en, 1 = zh, 3 = jp
                                        "Mon_Name_0" "Mon_Name_1" "Mon_Name_3")
                               (:from "ID_Mon_Name")
                               (:where "Mon_ID > -1")
                               (:order-by "Mon_ID")))
    (loop
       for id from 0
       for (dex-id en zh jp) in monsters
       do (let ((key (make-monster-key id)))
            (assert (= dex-id id))
            (push (list :id id
                        :key key
                        :name (lang-text :en en :zh zh :jp jp))
                  *monsters*))))
  
  (setf *monsters* (nreverse *monsters*))

  (format t "[ ok ] Skills loaded, total: ~a~%" (length *monsters*)))
  
(defun get-monster-list () *monsters*)
    



