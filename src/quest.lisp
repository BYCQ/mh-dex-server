(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.quest
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :make-quest-key
                  :lang-text)
    (:export :*quests*
             :reload-quests
             :get-quest-list)))
(in-package :mh-dex.quest)

(defun quest-level (level-id)
  "Mapping the Dex level-id to its correpsonding (village?, star)
   pair."
  (ecase level-id
    (10 (list t ;; village
              1 ));; star
    (11 (list t 2))
    (12 (list t 3))
    (13 (list t 4))
    (14 (list t 5))
    (15 (list t 6))
    (16 (list :false 1))
    (17 (list :false 2))
    (18 (list :false 3))
    (19 (list :false 4))
    (20 (list :false 5))
    (21 (list :false 6))
    (22 (list :false 7))
    (23 (list :false 8))))

(defparameter *quests* nil)

(defun reload-quests ()
  "Reload the variable *quests* which contains all the quest
   information."

  (setf *quests* nil)

  (with-dex-queries ((quests (:select "DB_Qst.Qst_ID" "Qst_Lv_ID" "Qst_Key_ID"
                                      ;; names: 0 = en, 1 = zh, 3 = jp
                                      "Qst_Name_0" "Qst_Name_1" "Qst_Name_3")
                             (:from "DB_Qst")
                             (:inner-join "ID_Qst_Name" "DB_Qst.Qst_ID = ID_Qst_Name.Qst_ID")
                             (:order-by "DB_Qst.Qst_ID")))
    (loop
       for id from 0
       for (dex-id level-id significance en zh jp) in quests
       do (let ((level (quest-level level-id)))
            (assert (= (1- dex-id) id))
            (push (list :id id
                        :key (make-quest-key id)
                        :name (lang-text :en en :zh zh :jp jp)
                        :significance significance
                        :village (car level)
                        :star (cadr level))
                  *quests*))))
  (setf *quests* (nreverse *quests*))
  (format t "[ ok ] Quests loaded, total: ~a~%" (length *quests*)))

(defun get-quest-list () *quests*)

                        
                      
    
                                      

              
