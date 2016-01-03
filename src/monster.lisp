(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.monster
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-monster-key
                  :make-item-key
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
                               (:order-by "Mon_ID"))
                     (items (:select "Mon_ID" "Itm_ID" "Qty"
                                     "Pct" "Rank_ID"
                                     ;; names: 0 = en, 1 = zh, 3 = jp
                                     "Mon_GetItmType_0"
                                     "Mon_GetItmType_1"
                                     "Mon_GetItmType_3")
                            (:from "DB_ItmtoMon")
                            (:inner-join "ID_Mon_GetItmType"
                                         "DB_ItmtoMon.Mon_GetItmType_ID = Id_Mon_GetItmType.Mon_GetItmType_ID")))
    (let ((items-table (make-hash-table)))
      (loop for (dex-id dex-item-id quantity probability rank en zh jp)
         in items
         do (push (list :key (make-item-key (1- dex-item-id))
                        :probability (round (* probability 100))
                        :rank rank
                        :approach (lang-text :en en :zh zh :jp jp))
                  (gethash dex-id items-table nil)))
      
      (loop
         for id from 0
         for (dex-id en zh jp) in monsters
         do (let ((key (make-monster-key id)))
              (assert (= dex-id id))
              (push (list :id id
                          :key key
                          :items (gethash dex-id items-table)
                          :name (lang-text :en en :zh zh :jp jp))
                    *monsters*)))))
  
  (setf *monsters* (nreverse *monsters*))

  (format t "[ ok ] Skills loaded, total: ~a~%" (length *monsters*)))
  
(defun get-monster-list () *monsters*)
    



