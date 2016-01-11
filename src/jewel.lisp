(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.jewel
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :def-db-interface
                  :make-jewel-key
                  :make-skill-key
                  :make-used-item-map
                  :lang-text)
    (:import-from :mh-dex.item
                  :*item-icon-path*)
    (:import-from :mh-dex.skill
                  :ensure-skills-loaded
                  :*skills*)
    (:export :*jewels*
             :reload-jewels
             :get-jewel-list
             :fetch-jewel-db)))
(in-package :mh-dex.jewel)

(defparameter *jewels* nil)

(defun make-skill-list (skill-a points-a skill-b points-b)
  "Make a list whose elements are (:skill x :points x)."
  (let ((result nil))
    (unless (= skill-a -1)
      (push (list :skill (make-skill-key (1- skill-a))
                  :name (getf (nth (1- skill-a ) *skills*) :name)
                  :points points-a) result))
    (unless (= skill-b -1)
      (push (list :skill (make-skill-key (1- skill-b))
                  :name (getf (nth (1- skill-b ) *skills*) :name)
                  :points points-b) result))
    result))

(defun reload-jewels ()
  "Reload the variable *jewels* which contains all the jewel
   information."

  (setf *jewels* nil)

  (ensure-skills-loaded)

  (with-dex-queries ((jewels (:select "Jew_ID" "DB_Jew.Itm_ID"
                                      ;; names: 0 = en, 1 = zh, 3 = jp
                                      "Itm_Name_0" "Itm_Name_1" "Itm_Name_3"
                                      "Slot" "Price"
                                      "SklTree1_ID" "SklTree1_Pt"
                                      "SklTree2_ID" "SklTree2_Pt")
                             (:from "DB_Jew")
                             (:inner-join "ID_Itm_Name" "DB_Jew.Itm_ID = ID_Itm_Name.Itm_ID")
                             (:order-by "Jew_ID"))
                     (items (:select "Jew_ID" "Itm_ID" "Qty")
                            (:from "DB_ItmtoJew")))
    (let ((item-icon-map (make-hash-table))
          (used-item-map (make-used-item-map items)))
      ;; Load the item id to icon-id map.
      (with-open-file (input *item-icon-path*
                             :direction :input
                             :if-does-not-exist :error)
        (loop for entry = (read input nil nil)
           while entry
           do (setf (gethash (car entry) item-icon-map)
                    (cadr entry))))

      (loop
         for id from 0
         for (dex-id dex-item-id en zh jp slots price skill-a points-a skill-b points-b)
         in jewels
         do (let ((key (make-jewel-key id)))
              (assert (= (1- dex-id) id))
              (push (list :id id
                          :key key
                          :name (lang-text :en en :zh zh :jp jp)
                          :slots slots
                          :price price
                          :iconid (gethash (1- dex-item-id)  item-icon-map)
                          :material (gethash dex-id used-item-map nil)
                          :skills (make-skill-list skill-a points-a
                                                   skill-b points-b))
                    *jewels*)))))
  
  (setf *jewels* (nreverse *jewels*))

  (format t "[ ok ] Jewels loaded, total: ~a~%" (length *jewels*)))

(defun get-jewel-list () *jewels*)
  
(def-db-interface jewel *jewels*)
