(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.skill
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-skill-key
                  :lang-text)
    (:export :*skills*
             :reload-skills
             :get-skill-list
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
                                           "SklTree_ID" "Pt")
                                  (:from "DB_Skl")
                                  (:inner-join "ID_Skl_Name" "DB_Skl.Skl_ID = ID_Skl_Name.Skl_ID")
                                  (:where "SklTree_ID > 0")
                                  (:order-by "SklTree_ID" "Pt")))

    (let ((system-skill-table (make-hash-table)))
      (loop
         for (unused en zh jp dex-id points) in real-skills
         do (push (list :name (lang-text :en en :zh zh :jp jp)
                        :points points)
                  (gethash dex-id system-skill-table nil)))
                                           
      (loop
         for id from 0
         for (dex-id en zh jp) in skill-systems
         do (let ((key (make-skill-key id)))
              (assert (= (1- dex-id) id))
              (push (list :id id
                          :key key
                          :name (lang-text :en en :zh zh :jp jp)
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



