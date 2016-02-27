(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.quest
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :def-db-interface
                  :make-quest-key
                  :make-quest-key-from-dex
                  :make-item-key
                  :make-monster-key
                  :lang-text)
    (:export :*quests*
             :reload-quests
             :get-quest-list
             :fetch-quest-db)))
(in-package :mh-dex.quest)

(defun quest-level (level-id)
  "Mapping the Dex level-id to its correpsonding (category, star)
   pair, where category can be :v(lliage), :g(uild) or :d(lc)"
  (ecase level-id
    (10 (list :v 1))
    (11 (list :v 2))
    (12 (list :v 3))
    (13 (list :v 4))
    (14 (list :v 5))
    (15 (list :v 6))
    (16 (list :g 1))
    (17 (list :g 2))
    (18 (list :g 3))
    (19 (list :g 4))
    (20 (list :g 5))
    (21 (list :g 6))
    (22 (list :g 7))
    (23 (list :g 8))
    (24 (list :d 1))
    (25 (list :d 2))
    (26 (list :d 3))
    (27 (list :d 4))
    (28 (list :d 5))
    (29 (list :d 6))
    (30 (list :d 7))))

(defparameter *location-names*
  (list (lang-text :en nil :zh "古代林" :jp "古代林")
        (lang-text :en "Forest and Hills" :zh "森丘" :jp "森丘")
        (lang-text :en "Snowy Mountains" :zh "雪山" :jp "雪山")
        (lang-text :en "Misty Peaks" :zh "溪流" :jp "渓流")
        (lang-text :en "Dunes" :zh "旧沙漠" :jp "旧砂漠")
        (lang-text :en "Deserted Island" :zh "孤岛" :jp "孤島")
        (lang-text :en nil :zh "沼地" :jp "沼地")
        (lang-text :en "Volcano" :zh "火山" :jp "火山")
        (lang-text :en "A. Steppe" :zh "遗迹平原" :jp "遺跡平原")
        (lang-text :en "V. Hollow" :zh "地底火山" :jp "地底火山")
        (lang-text :en "P. Forest" :zh "原生林" :jp "原生林")
        (lang-text :en "F. Seaway" :zh "冰海" :jp "氷海")
        (lang-text :en "Arena" :zh "斗技场" :jp "闘技場")
        (lang-text :en "Slayground" :zh "立体斗技场" :jp "立体闘技場")
        (lang-text :en nil :zh "水上斗技场" :jp "水上闘技場")
        (lang-text :en "Sanctuary" :zh "禁足地" :jp "禁足地")
        (lang-text :en nil :zh "塔之秘境" :jp "塔の秘境")
        (lang-text :en nil :zh "灵峰" :jp "霊峰")
        (lang-text :en "Ingle Isle" :zh "溶岩岛" :jp "溶岩島")
        (lang-text :en "Polar Field" :zh "极圈" :jp "極圏")
        (lang-text :en nil :zh "龙之墓场" :jp "竜ノ墓場")
        (lang-text :en nil :zh "古代林＜昼＞" :jp "古代林＜昼＞")
        (lang-text :en nil :zh "古代林＜夜＞" :jp "古代林＜夜＞")
        (lang-text :en nil :zh "森丘＜昼＞" :jp "森丘＜昼＞")
        (lang-text :en nil :zh "森丘＜夜＞" :jp "森丘＜夜＞")
        (lang-text :en "Snowy Mt. (D)" :zh "雪山＜昼＞" :jp "雪山＜昼＞")
        (lang-text :en "Snowy Mt. (N)" :zh "雪山＜夜＞" :jp "雪山＜夜＞")
        (lang-text :en "M.Peaks (D)" :zh "溪流＜昼＞" :jp "渓流＜昼＞")
        (lang-text :en "M.Peaks (N)" :zh "溪流＜夜＞" :jp "渓流＜夜＞")))

(defparameter *quests* nil)

(defun reload-quests ()
  "Reload the variable *quests* which contains all the quest
   information."

  (setf *quests* nil)

  (with-dex-queries ((quests (:select "DB_Qst.Qst_ID" "Qst_Lv_ID" "Qst_Key_ID"
                                      ;; names: 0 = en, 1 = zh, 3 = jp
                                      "Qst_Name_0" "Qst_Name_1" "Qst_Name_3"
                                      "Qst_Goal_0" "Qst_Goal_1" "Qst_Goal_3"
                                      "Qst_Goal2_0" "Qst_Goal2_1" "Qst_Goal2_3"
                                      "QstIcon" "Qst_Type_ID" "Loc_ID" "Fee"
                                      "Time" "Reward" "HRP" "Reward2" "HRP2"
                                      "Mon1_ID" "Mon2_ID" "Mon3_Id" "Mon4_ID"
                                      "Mon5_ID" "Mon6_ID" "Mon7_Id" "Mon8_ID")
                             (:from "DB_Qst")
                             (:inner-join "ID_Qst_Name" "DB_Qst.Qst_ID = ID_Qst_Name.Qst_ID")
                             (:inner-join "ID_Qst_Goal" "DB_Qst.Qst_ID = ID_Qst_Goal.Qst_ID")
                             (:inner-join "ID_Qst_Goal2" "DB_Qst.Qst_ID = ID_Qst_Goal2.Qst_ID")
                             (:order-by "DB_Qst.Qst_ID"))
                     (items (:select "Qst_ID" "Itm_ID" "Qty" "Pct" "Type")
                            (:from "DB_ItmtoQst")))
    (let ((items-table (make-hash-table)))

      (loop for (dex-id dex-item-id quantity probability type)
         in items
         do (push (list :key (make-item-key (1- dex-item-id))
                        :quantity quantity
                        :probability (round (* probability 100)))
                  (getf (gethash dex-id items-table nil)
                        (cond ((string= type "A") :a)
                              ((string= type "B") :b)
                              ((string= type "C") :c)
                              ((string= type "D") :d)
                              (t (error "Unknown type!"))))))

      (loop
         for id from 0
         for (dex-id level-id significance en zh jp
                     main-goal-en main-goal-zh main-goal-jp
                     sub-goal-en sub-goal-zh sub-goal-jp
                     icon-id type location fee time
                     main-reward main-hrp sub-reward sub-hrp
                     monster-1 monster-2 monster-3 monster-4
                     monster-5 monster-6 monster-7 monster-8) in quests
         do (let ((level (quest-level level-id)))
              (assert (equal (make-quest-key id)
                             (make-quest-key-from-dex dex-id)))
              (push (list :id id
                          :key (make-quest-key id)
                          :name (lang-text :en en :zh zh :jp jp)
                          :significance significance
                          :category (car level)
                          :star (cadr level)
                          :iconid (1- icon-id)
                          :type (1- type)
                          :location (nth (1- location) *location-names*)
                          :fee fee
                          :time time
                          :items (list :a (getf (gethash dex-id items-table) :a)
                                       :b (getf (gethash dex-id items-table) :b)
                                       :c (getf (gethash dex-id items-table) :c)
                                       :d (getf (gethash dex-id items-table) :d))
                          :monsters (mapcar #'make-monster-key
                                           (remove-if (lambda (x) (= x -1))
                                                      (list monster-1 monster-2 monster-3 monster-4
                                                            monster-5 monster-6 monster-7 monster-8)))
                          :target (list :main (list :goal (lang-text :en main-goal-en
                                                                     :zh main-goal-zh
                                                                     :jp main-goal-jp)
                                                    :reward main-reward
                                                    :hrp main-hrp)
                                        :sub (list :goal (lang-text :en sub-goal-en
                                                                    :zh sub-goal-zh
                                                                    :jp sub-goal-jp)
                                                   :reward sub-reward
                                                   :hrp sub-hrp)))
                    *quests*)))))
  (setf *quests* (nreverse *quests*))
  (format t "[ ok ] Quests loaded, total: ~a~%" (length *quests*)))
  
(defun get-quest-list () *quests*)

(def-db-interface quest *quests*)
