(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.item
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :def-db-interface
                  :make-item-key
                  :make-armor-key
                  :make-quest-key-from-dex
                  :make-monster-key
                  :make-jewel-key
                  :lang-text)
    (:import-from :mh-dex.weapon
                  :ensure-weapons-loaded
                  :*weapons*)
    (:export :*items*
             :*item-icon-path*
             :reload-items
             :get-item-type-list
             :get-locations
             :get-approaches
             :get-item-list
             :ensure-items-loaded
             :fetch-item-db)))
(in-package :mh-dex.item)

(defparameter +item-types+
  (list (list :name (lang-text :en "Common"
                               :zh "生存"
                               :jp "生存")
              :range '((0 90) (168 219) (1015 1045)))
        (list :name (lang-text :en "Ammo"
                               :zh "弹药"
                               :jp "弾薬")
              :range '((91 140) (1852 1892)))
        (list :name (lang-text :en "Bait"
                               :zh "虫饵"
                               :jp "虫餌")
              :range '((141 167)))
        (list :name (lang-text :en "Ore"
                               :zh "矿石"
                               :jp "鉱石")
              :range '((220 252)))
        (list :name (lang-text :en "Fish"
                               :zh "鱼"
                               :jp "魚")
              :range '((253 275)))
        (list :name (lang-text :en "Insect"
                               :zh "虫"
                               :jp "虫")
              :range '((276 295)))
        (list :name (lang-text :en "Monster"
                               :zh "怪物"
                               :jp "モンスター")
              :range '((296 1014)))
        (list :name (lang-text :en "Ticket"
                               :zh "票券"
                               :jp "チケット")
              :range '((1046 1210) (1806 1851)))
        (list :name (lang-text :en "Account"
                               :zh "精算"
                               :jp "精算")
              :range '((1211 1298)))
        (list :name (lang-text :en "Scrap"
                               :zh "端材"
                               :jp "端材")
              :range '((1299 1478)))))

(defparameter +locations+
  (list (list :name (lang-text :en "A. Forest" :zh "古代林" :jp "古代林"))
        (list :name (lang-text :en "Forest and Hills" :zh "森丘" :jp "森丘"))
        (list :name (lang-text :en "Snowy Mountains" :zh "雪山" :jp "雪山"))
        (list :name (lang-text :en "Misty Peaks" :zh "溪流" :jp "渓流"))
        (list :name (lang-text :en "Dunes" :zh "旧沙漠" :jp "旧砂漠"))
        (list :name (lang-text :en "Deserted Island" :zh "孤岛" :jp "孤島"))
        (list :name (lang-text :en "Swamp" :zh "沼地" :jp "沼地"))
        (list :name (lang-text :en "Volcano" :zh "火山" :jp "火山"))
        (list :name (lang-text :en "A. Steppe" :zh "遗迹平原" :jp "遺跡平原"))
        (list :name (lang-text :en "V. Hollow" :zh "地底火山" :jp "地底火山"))
        (list :name (lang-text :en "P. Forest" :zh "原生林" :jp "原生林"))
        (list :name (lang-text :en "F. Seaway" :zh "冰海" :jp "氷海"))
        (list :name (lang-text :en "Arena" :zh "斗技场" :jp "闘技場"))
        (list :name (lang-text :en "Slayground" :zh "立体斗技场" :jp "立体闘技場"))
        (list :name (lang-text :en "Water Arena" :zh "水上斗技场" :jp "水上闘技場"))
        (list :name (lang-text :en "Sanctuary" :zh "禁足地" :jp "禁足地"))
        (list :name (lang-text :en "Tower" :zh "塔之秘境" :jp "塔の秘境"))
        (list :name (lang-text :en "Scrd Mountain" :zh "灵峰" :jp "霊峰"))
        (list :name (lang-text :en "Ingle Isle" :zh "溶岩岛" :jp "溶岩島"))
        (list :name (lang-text :en "Polar Field" :zh "极圈" :jp "極圏"))
        (list :name (lang-text :en "Drg Graveyard" :zh "龙之墓场" :jp "竜ノ墓場"))
        (list :name (lang-text :en "A. Forest <D>" :zh "古代林＜昼＞" :jp "古代林＜昼＞"))
        (list :name (lang-text :en  "A. Forest <N>" :zh "古代林＜夜＞" :jp "古代林＜夜＞"))
        (list :name (lang-text :en "F. and Hills <D>" :zh "森丘＜昼＞" :jp "森丘＜昼＞"))
        (list :name (lang-text :en  "F. and Hills <N>" :zh "森丘＜夜＞" :jp "森丘＜夜＞"))
        (list :name (lang-text :en "Snowy Mt. (D)" :zh "雪山＜昼＞" :jp "雪山＜昼＞"))
        (list :name (lang-text :en  "Snowy Mt. (N)" :zh "雪山＜夜＞" :jp "雪山＜夜＞"))
        (list :name (lang-text :en "M.Peaks (D)" :zh "溪流＜昼＞" :jp "渓流＜昼＞"))
        (list :name (lang-text :en  "M.Peaks (N)" :zh "溪流＜夜＞" :jp "渓流＜夜＞")))
  "The list of maps.")

(defparameter +approaches+
  (list (list :name (lang-text :en "Gathering" :zh "采集" :jp "採取"))
        (list :name (lang-text :en "Fishing" :zh "钓鱼" :jp "釣り"))
        (list :name (lang-text :en "Mining" :zh "采矿" :jp "採掘"))
        (list :name (lang-text :en "Bug-Catching" :zh "捕虫" :jp "虫捕り")))
  "The approaches to get items from a location.")

(defun identify-item-type (id)
  (loop
     for type in +item-types+
     for type-id from 0
     when (some (lambda (range)(<= (car range) id (cadr range)))
                (getf type :range))
     return type-id
     finally (return -1)))

(defparameter *item-icon-path* (merge-pathnames "data/items/item-icon.lisp"
                                                (asdf:system-source-directory :mh-dex))
  "The path to the item icon file that maps item to its corresponding
  icon-id.")

(defparameter *items* nil)

(defun find-combo-list-to-produce (dex-id combos)
  "Returns a list of possible combos that produce the specified item."
  (loop for (item1 item2 production success-rate quantity)
     in combos
     when (= dex-id production)
     collect (list :a (make-item-key (1- item1))
                   :b (make-item-key (1- item2))
                   :rate success-rate
                   :quantity quantity)))

(defun find-item-list-can-be-produced-from (dex-id combos)
  "Returns a list of items that can be produced via combo using the
   specified item."
  (loop for (item1 item2 production success-rate quantity)
     in combos
     when (or (= item1 dex-id) (= item2 dex-id))
     collect (list :b (if (= item1 dex-id)
                          (make-item-key (1- item2))
                          (make-item-key (1- item1)))
                   :c (make-item-key (1- production))
                   :rate success-rate
                   :quantity quantity)))

(defun reload-items ()
  "Reload the variable *items* which contains all the item
   information."

  ;; Clear the *items* list.
  (setf *items* nil)

  (ensure-weapons-loaded)

  (let ((item-icon-map (make-hash-table))
        (item-weapon-map (make-hash-table :test #'equal)))
    ;; Load the item id to icon-id map.
    (with-open-file (input *item-icon-path*
                           :direction :input
                           :if-does-not-exist :error)
      (loop for entry = (read input nil nil)
         while entry
         do (setf (gethash (car entry) item-icon-map)
                  (cadr entry))))

    (loop for weapon-type below (length *weapons*)
       do (loop for weapon in (aref *weapons* weapon-type)
             do
               (loop for entry in (append (getf (getf weapon :material) :produce)
                                          (getf (getf weapon :material) :upgrade))
                  do (push (getf weapon :key)
                           (gethash (getf entry :itemkey)
                                    item-weapon-map 
                                    nil)))))

    ;; Load the data from the Dex database.
    (with-dex-queries ((items (:select "DB_Itm.Itm_ID" "Rare" "Sell" "Buy" "Max"
                                       ;; names: 0 = en, 1 = zh, 3 = jp
                                       "Itm_Name_0" "Itm_Name_1" "Itm_Name_3")
                              (:from "DB_Itm")
                              (:inner-join "ID_Itm_Name" "DB_Itm.Itm_ID = ID_Itm_Name.Itm_ID")
                              (:order-by "DB_Itm.Itm_ID"))
                       (combos (:select "ItmA_ID" "ItmB_ID" "ItmC_ID" "Succ" "Qty")
                               (:from "DB_ItmtoCbo"))
                       (armors (:select "Amr_ID" "Itm_ID")
                               (:from "DB_ItmtoAmr"))
                       (jewels (:select "Jew_ID" "Itm_ID")
                               (:from "DB_ItmtoJew"))
                       (monsters (:select "Mon_ID" "Itm_ID")
                                 (:from "DB_ItmtoMon"))
                       (quests (:select "Qst_ID" "Itm_ID")
                               (:from "DB_ItmtoQst"))
                       (locations (:select "Itm_ID" "Loc_ID" "Loc_Ara_ID"
                                           "Loc_GetType_ID" "Rank_ID")
                                  (:from "DB_ItmtoLoc")
                                  (:order-by "-Rank_ID" "Loc_GetType_ID")))

      (let ((equal-key (lambda (x y) (equal (getf x :key) (getf y :key))))
            (armors-table (make-hash-table))
            (jewels-table (make-hash-table))
            (monsters-table (make-hash-table))
            (quests-table (make-hash-table))
            (locations-table (make-hash-table)))

        (loop for (dex-armor-id dex-id) in armors
           do (push (list :key (make-armor-key (1- dex-armor-id)))
                    (gethash dex-id armors-table)))

        (loop for (dex-jewel-id dex-id) in jewels
           do (push (list :key (make-jewel-key (1- dex-jewel-id)))
                    (gethash dex-id jewels-table)))

        (loop for (dex-monster-id dex-id) in monsters
           do (push (list :key (make-monster-key dex-monster-id))
                    (gethash dex-id monsters-table)))
        
        (loop for (dex-quest-id dex-id) in quests
           do (push (list :key (make-quest-key-from-dex dex-quest-id))
                    (gethash dex-id quests-table)))

        (loop for (dex-id dex-location-id area approach rank)
           in locations
           do (push (list :location (1- dex-location-id)
                          :area area
                          :approach (1- approach)
                          ;; 1 = low, 2 = high
                          :rank rank)
                    (gethash dex-id locations-table nil)))
        
        (loop
           for id from 0
           for (dex-id
                rare
                sell-price buy-price
                carry ;; maximum carry quantity
                en zh jp) in items
           do (let ((type (identify-item-type id))
                    (key (make-item-key id)))
                ;; Make sure that there is no missing item ids in Dex.
                (assert (= (1+ id) dex-id))
                (push (list :id id
                            :key (make-item-key id)
                            :type type
                            :rare rare
                            :name (lang-text :en (or en jp)
                                             :zh (or zh jp)
                                             :jp jp)
                            :price (list :sell sell-price :buy buy-price)
                            :carry carry
                            :acquire (list :combo (find-combo-list-to-produce dex-id combos))
                            :usage (list :combo (find-item-list-can-be-produced-from dex-id combos)
                                         :weapon (gethash (make-item-key id)
                                                          item-weapon-map
                                                          nil))
                            :iconid (gethash id item-icon-map)
                            :locations (gethash dex-id locations-table)
                            :armors (remove-duplicates (gethash dex-id armors-table)
                                                       :test equal-key)
                            :jewels (remove-duplicates (gethash dex-id jewels-table)
                                                       :test equal-key)
                            :monsters (remove-duplicates (gethash dex-id monsters-table)
                                                         :test equal-key)
                            :quests (remove-duplicates (gethash dex-id quests-table)
                                                       :test equal-key))
                      *items*))))))

  (setf *items* (nreverse *items*))
  (format t "[ ok ] Items loaded, total: ~a~%"
          (length *items*)))

(declaim (inline ensure-items-loaded))
(defun ensure-items-loaded ()
  "Make sure that reload-items is already excuted and *items* has
   valid data. Call reload-items if not."
  (unless *items* (reload-items)))

(defun get-item-type-list () +item-types+)

(defun get-locations () +locations+)

(defun get-approaches () +approaches+)

(defun get-item-list () *items*)

(def-db-interface item *items*)
