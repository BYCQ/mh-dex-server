(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.jewel
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-jewel-key
                  :lang-text)
    (:export :*jewels*
             :reload-jewels
             :get-jewel-list)))
(in-package :mh-dex.jewel)

(defparameter *jewels* nil)

(defun make-skill-list (skill-a points-a skill-b points-b)
  "Make a list whose elements are (:skill x :points x)."
  (let ((result nil))
    (unless (= skill-a -1)
      (push (list :skill skill-a :points points-a) result))
    (unless (= skill-b -1)
      (push (list :skill skill-b :points points-b) result))
    result))

(defun reload-jewels ()
  "Reload the variable *jewels* which contains all the jewel
   information."

  (setf *jewels* nil)

  (with-dex-queries ((jewels (:select "Jew_ID"
                                      ;; names: 0 = en, 1 = zh, 3 = jp
                                      "Itm_Name_0" "Itm_Name_1" "Itm_Name_3"
                                      "Slot" "Price"
                                      "SklTree1_ID" "SklTree1_Pt"
                                      "SklTree2_ID" "SklTree2_Pt")
                             (:from "DB_Jew")
                             (:inner-join "ID_Itm_Name" "DB_Jew.Itm_ID = ID_Itm_Name.Itm_ID")
                             (:order-by "Jew_ID")))
    (let ((jewel-skill-table (make-hash-table)))
      (loop
         for id from 0
         for (dex-id en zh jp slots price skill-a points-a skill-b points-b)
         in jewels
         do (let ((key (make-jewel-key id)))
              (push (list :id id
                          :key key
                          :name (lang-text :en en :zh zh :jp jp)
                          :slots slots
                          :price price
                          :skills (make-skill-list skill-a points-a
                                                   skill-b points-b))
                    *jewels*)))))
  
  (setf *jewels* (nreverse *jewels*))

  (format t "[ ok ] Jewels loaded, total: ~a~%" (length *jewels*)))

(defun get-jewel-list () *jewels*)
  
