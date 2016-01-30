(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.monster
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :def-db-interface
                  :make-monster-key
                  :make-item-key
                  :make-quest-key
                  :lang-text)
    (:export :*monsters*
             :reload-monsters
             :get-monster-list
             :get-ailment-types
             :fetch-monster-db)))
(in-package :mh-dex.monster)

(defparameter *monsters* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +ailment-types+ (list (list :name (lang-text :en "Poison" :zh "毒" :jp "毒"))
                                      (list :name (lang-text :en "Paralyze" :zh "麻痹" :jp "麻痺"))
                                      (list :name (lang-text :en "Sleep" :zh "睡眠" :jp "睡眠"))
                                      (list :name (lang-text :en "Blast" :zh "爆破" :jp "爆破"))
                                      (list :name (lang-text :en "Stun" :zh "气绝" :jp "気絶"))
                                      (list :name (lang-text :en "Fatigue" :zh "灭气" :jp "減気"))
                                      (list :name (lang-text :en "Mount" :zh "骑乘" :jp "乗り")))))

(defun combine-weakness-label (en zh jp condition-en condition-zh condition-jp)
  (labels ((make-label (name condition)
             (cond (condition (format nil "~a (~a)" name condition))
                   (condition-jp (format nil "~a (~a)" name condition-jp))
                   (t name))))
    (if condition-jp
        (lang-text :en (make-label en condition-en)
                   :zh (make-label zh condition-zh)
                   :jp (make-label jp condition-jp))
        (lang-text :en en :zh zh :jp jp))))

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
                     (quests (:select "Qst_ID"
                                      "Mon1_ID" "Mon2_ID" "Mon3_ID" "Mon4_ID"
                                      "Mon5_ID" "Mon6_ID" "Mon7_ID" "Mon8_ID")
                             (:from "DB_Qst")
                             (:order-by "-Qst_Lv_ID"))
                     (items (:select "Mon_ID" "Itm_ID" "Qty"
                                     "Pct" "Rank_ID"
                                     ;; names: 0 = en, 1 = zh, 3 = jp
                                     "Mon_GetItmType_0"
                                     "Mon_GetItmType_1"
                                     "Mon_GetItmType_3")
                            (:from "DB_ItmtoMon")
                            (:inner-join "ID_Mon_GetItmType"
                                         "DB_ItmtoMon.Mon_GetItmType_ID = Id_Mon_GetItmType.Mon_GetItmType_ID"))
                     (ailments (:select "Mon_ID" "Stat_ID" "TolInitial" "TolIncrease" "TolMax"
                                        "Duration" "Damage")
                               (:from "DB_Mon_Ail")
                               (:order-by "Mon_ID" "-Stat_ID"))
                     (weakness (:select "DB_Mon_Weak.MonPart_ID" "Mon_ID"
                                        ;; part names: 0 = en, 1 = zh, 3 = jp
                                        "Mon_Part_0" "Mon_Part_1" "Mon_Part_3"
                                        "Mon_PartCond_0" "Mon_PartCond_1" "Mon_PartCond_3"
                                        "Cut" "Impact" "Shot"
                                        "Fir" "Wtr" "Ice" "Thd" "Drg" "Diz")
                               (:from "DB_Mon_Weak")
                               (:inner-join "ID_Mon_Part" "DB_Mon_Weak.MonPart_ID = ID_Mon_Part.Mon_Part_ID")
                               (:inner-join "ID_Mon_PartCond" "DB_Mon_Weak.MonPart_ID = ID_Mon_PartCond.Mon_Part_ID")
                               (:order-by "-DB_Mon_Weak.MonPart_ID")))

    (let ((items-table (make-hash-table))
          (quests-table (make-hash-table))
          (ailments-table (make-hash-table))
          (weakness-table (make-hash-table)))

      (loop for (dex-id dex-item-id quantity probability rank en zh jp)
         in items
         do (push (list :key (make-item-key (1- dex-item-id))
                        :probability (round (* probability 100))
                        :rank rank
                        :quantity quantity
                        :approach (lang-text :en en :zh zh :jp jp))
                  (gethash dex-id items-table nil)))

      (loop
         for ids in quests
         for dex-quest-id = (car ids)
         do (loop for dex-id in (rest ids)
               do (push (list :key (make-quest-key (1- dex-quest-id)))
                        (gethash dex-id quests-table nil))))

      (loop for (dex-id dex-stat-id initial increase max duration damage) in ailments
         ;; There is a stat id = 13 in dex database, but not sure what it is.
         when (< dex-stat-id 13)
         do (push (list :id (- dex-stat-id 6)
                        :stats (list :initial initial
                                     :increase increase
                                     :max max
                                     :damage damage)
                        :duration duration)
                  (gethash dex-id ailments-table nil)))

      (loop for (seq dex-id
                     en zh jp
                     condition-en condition-zh condition-jp
                     cut impact shot fire water ice thunder dragon dizzle)
         in weakness
         do (push (list :label (combine-weakness-label en zh jp condition-en condition-zh condition-jp)
                        :hit (list :cut cut :impact impact :shot shot)
                        :elemental (list :fire fire
                                         :water water
                                         :ice ice
                                         :thunder thunder
                                         :dragon dragon
                                         :dizzle dizzle))
                  (gethash dex-id weakness-table)))
      
      (loop
         for id from 0
         for (dex-id en zh jp) in monsters
         do (let ((key (make-monster-key id)))
              (assert (= dex-id id))
              (push (list :id id
                          :type (cond ((= id 0) 0) ;; 0: N/A
                                      ((< id 35) 1) ;; 1: small monsters
                                      ((< id 106) 2) ;; 2: big monsters
                                      (t 3)) ;; 3: N/A
                          :key key
                          :items (gethash dex-id items-table)
                          :quests (gethash dex-id quests-table)
                          :ailments (gethash dex-id ailments-table)
                          :weakness (gethash dex-id weakness-table nil)
                          :name (lang-text :en en :zh zh :jp jp))
                    *monsters*)))))
  
  (setf *monsters* (nreverse *monsters*))

  (format t "[ ok ] Skills loaded, total: ~a~%" (length *monsters*)))

(defun get-monster-list () *monsters*)

(defun get-ailment-types () +ailment-types+)

(def-db-interface monster *monsters*)
