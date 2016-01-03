(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.armor
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-armor-key
                  :make-skill-key
                  :make-used-item-map
                  :lang-text)
    (:export :*armors*
             :reload-armors
             :get-armor-list)))
(in-package :mh-dex.armor)

(defparameter *armors* nil)

(defun reload-armors ()
  "Reload the variable *armors* which contains all the armor
   information."

  (setf *armors* nil)

  (with-dex-queries ((armors (:select "DB_Amr.Amr_ID"
                                      ;; names: 0 = en, 1 = zh, 3 = jp
                                      "Amr_Name_0" "Amr_Name_1" "Amr_Name_3"
                                      "BorG" ;; range/melee
                                      "MorF" ;; gender
                                      "Rare" "Part" "Slot"
                                      "Def" "MaxDef" "Price"
                                      "Res_Fire" "Res_Water" "Res_Thunder" "Res_Ice" "Res_Dragon")
                             (:from "DB_Amr")
                             (:inner-join "ID_Amr_Name" "DB_Amr.Amr_ID = ID_Amr_Name.Amr_ID")
                             (:order-by "DB_Amr.Amr_ID"))
                     (items (:select "Amr_ID" "Itm_ID" "Qty")
                            (:from "DB_ItmtoAmr"))
                     (skills (:select "Amr_ID" "SklTree_ID" "Pt")
                             (:from "DB_SklTreetoAmr")))
    (let ((armor-skill-table (make-hash-table))
          (used-item-map (make-used-item-map items)))

      ;; Prepare the hash table that maps armor (dex) ID to its
      ;; corresponding skill sets.
      (loop for (dex-id skill-system-id points) in skills
         do (push (list :key (make-skill-key skill-system-id)
                        :points points)
                  (gethash dex-id armor-skill-table nil)))

      (loop
         for id from 0
         for (dex-id en zh jp type gender rare part slot defense max-defense
                     price fire water thunder ice dragon) in armors
         do (let ((key (make-armor-key id)))
              (assert (= (1- dex-id) id))
              (push (list :id id
                          :key key
                          :name (lang-text :en en :zh zh :jp jp)
                          :type type ;; 0 = both, 1 = melee, 2 = range
                          :gender gender ;; 0 = both, 1 = male, 2 = female
                          :rare rare
                          :part part
                          :slots slot
                          :defense (list :min defense
                                         :max max-defense)
                          :price price
                          :resist (list :fire fire :water water :thunder thunder
                                        :ice ice :dragon dragon)
                          :material (gethash dex-id used-item-map nil)
                          :skillset (gethash dex-id armor-skill-table nil))
                    *armors*)))))

  (setf *armors* (nreverse *armors*))

  (format t "[ ok ] Armors loaded, total: ~a~%" (length *armors*)))

(defun get-armor-list () *armors*)
  
