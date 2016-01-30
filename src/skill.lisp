(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.skill
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :def-db-interface
                  :make-skill-key
                  :make-jewel-key
                  :make-armor-key
                  :lang-text)
    (:export :*skills*
             :reload-skills
             :get-skill-list
             :fetch-skill-db
             :ensure-skills-loaded)))
(in-package :mh-dex.skill)

(defparameter *skills* nil)

(defun reload-skills ()
  "Reload the variable *skills* which contains all the skill
   information."

  (setf *skills* nil)


  (with-dex-queries ((skill-systems (:select "SklTree_ID"
                                             ;; names: 0 = en, 1 = zh, 3 = jp
                                             "SklTree_Name_0" "SklTree_Name_1" "SklTree_Name_3")
                                    (:from "ID_SklTree_Name")
                                    (:where "SklTree_ID > 0")
                                    (:order-by "SklTree_ID"))
                     (real-skills (:select "DB_Skl.Skl_ID"
                                           ;; names: 0 = en, 1 = zh, 3 = jp
                                           "Skl_Name_0" "Skl_Name_1" "Skl_Name_3"
                                           "SklTree_ID" "Pt" "CompSkl_ID")
                                  (:from "DB_Skl")
                                  (:inner-join "ID_Skl_Name" "DB_Skl.Skl_ID = ID_Skl_Name.Skl_ID")
                                  (:outer-join "DB_CompSkl" "DB_Skl.Skl_ID = DB_CompSkl.Skl_ID")
                                  (:where "SklTree_ID > 0")
                                  (:order-by "SklTree_ID" "Pt"))
                     (armors (:select "Amr_ID" "SklTree_ID" "Pt")
                             (:from "DB_SklTreetoAmr"))
                     (jewels (:select "Jew_ID" "SklTree_ID" "Pt")
                             (:from "DB_SklTreetoJew")))
    (let ((system-skill-table (make-hash-table))
          (skill-to-system-map (make-hash-table))
          (armors-table (make-hash-table))
          (jewels-table (make-hash-table)))

      (loop for (armor-dex-id dex-id points) in armors
         when (> points 0)
         do (push (list :key (make-armor-key (1- armor-dex-id))
                        :points points)
                  (gethash dex-id armors-table nil)))
      
      (loop for (jewel-dex-id dex-id points) in jewels
         when (> points 0)
         do (push (list :key (make-jewel-key (1- jewel-dex-id))
                        :points points)
                  (gethash dex-id jewels-table nil)))

      (loop
         for (skill-dex-id en zh jp dex-id points composite-parent) in real-skills
         do (setf (gethash skill-dex-id skill-to-system-map) dex-id))
      
      (loop
         for (unused en zh jp dex-id points composite-parent) in real-skills
         do (push (list :name (lang-text :en en :zh zh :jp jp)
                        :points points
                        :composite (if composite-parent
                                       (1- (gethash composite-parent skill-to-system-map))
                                       :null))
                  (gethash dex-id system-skill-table nil)))
                                           
      (loop
         for id from 0
         for (dex-id en zh jp) in skill-systems
         do (let ((key (make-skill-key id)))
              (assert (= (1- dex-id) id))
              (push (list :id id
                          :key key
                          :name (lang-text :en en :zh zh :jp jp)
                          :jewels (sort (gethash dex-id jewels-table)
                                        (lambda (x y) (> (getf x :points) (getf y :points))))
                          :armors (sort (gethash dex-id armors-table)
                                        (lambda (x y) (> (getf x :points) (getf y :points))))
                          :list (gethash dex-id system-skill-table nil))
                    *skills*)))))

  (setf *skills* (nreverse *skills*))

  (format t "[ ok ] Skills loaded, total: ~a~%" (length *skills*)))
  
(declaim (inline ensure-skills-loaded))
(defun ensure-skills-loaded ()
  "Make sure that reload-skills is already excuted and *skills* has
   valid data. Call reload-skills if not."
  (unless *skills* (reload-skills)))

(defun get-skill-list () *skills*)

(def-db-interface skill *skills*)
