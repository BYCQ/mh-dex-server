(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.weapon
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :lang-text
                  :make-weapon-key
                  :make-item-key)
    (:export :*weapons*
             :get-weapon-list
             :get-weapon-type-list
             :get-sharpness-color-list
             :get-special-type-list
             :get-gunlance-shot-types
             :reload-weapons
             :ensure-weapons-loaded
             :fetch-weapon-db)))
(in-package :mh-dex.weapon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro create-columns (&body body)
    "BODY is a list of keywords representing the possible columns."
    `(list ,@(mapcar (lambda (column)
                       (ecase column
                         (:name '(list :name "name" :zh "武器名" :jp "武器名" :en "Name"))
                         (:slots '(list :name "slots" :zh "孔" :jp "スロット" :en "Slots"))
                         (:affinity '(list :name "affinity" :zh "会心" :jp "会心" :en "Affinity"))
                         (:special '(list :name "special" :zh "特殊" :jp "特別" :en "Special"))
                         (:sharpness '(list :name "sharpness" :zh "斩味" :jp "斬れ味":en "Sharpness"))
                         (:gunlance-shot-type '(list :name "gunlanceType" :zh "炮击" :jp "砲撃" :en "Gun"))
                         (:attack '(list :name "attack" :zh "攻击力" :jp "攻撃" :en "Attack" :numeric t))))
                     body)))

  (defparameter +weapon-types+ (list (list :name (lang-text :en "Great Sword" 
                                                            :zh "大剑"
                                                            :jp "大剣")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Long Sword"
                                                            :zh "太刀"
                                                            :jp "太刀")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Sowrd/Shield"
                                                            :zh "片手剑"
                                                            :jp "片手剣")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Dual Blades"
                                                            :zh "双剑"
                                                            :jp "双剣")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Hammer"
                                                            :zh "大锤"
                                                            :jp "ハンマー")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Hunting Horn"
                                                            :zh "狩猎笛"
                                                            :jp "狩猟笛")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Lance"
                                                            :zh "长枪"
                                                            :jp "ランス")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Gunlance"
                                                            :zh "铳枪"
                                                            :jp "ガンランス")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :gunlance-shot-type
                                                                    :sharpness))
                                     (list :name (lang-text :en "Switch Axe"
                                                            :zh "斩击斧"
                                                            :jp "スラッシュアックス")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Charge Blade"
                                                            :zh "盾斧"
                                                            :jp "チャージアックス")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Insect Glaive"
                                                            :zh "操虫棍"
                                                            :jp "操虫棍")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Light Bowgun"
                                                            :zh "轻弩"
                                                            :jp "ライトボウガン")
                                           :columns (create-columns :name :slots :attack))
                                     (list :name (lang-text :en "Heavy Bowgun"
                                                            :zh "重弩"
                                                            :jp "ヘビィボウガン")
                                           :columns (create-columns :name :slots :attack))
                                     (list :name (lang-text :en "Bow"
                                                            :zh "弓"
                                                            :jp "弓")
                                           :columns (create-columns :name :slots :attack)))
    "The names of the weapon-types.")

  (defparameter +sharpness-colors+ (list "#D52B00" "#FF732C" "#FFFF66"
                                         "#66FF33" "#3399FF" "#FCFCFC" "#9900FF")
    "The color of the sharpness blocks.")

  (defparameter +special-types+ (list (list :name (lang-text :en "Fire" :zh "火" :jp "火")
                                            :color "#000000")
                                      (list :name (lang-text :en "Wat" :zh "水" :jp "水")
                                            :color "#000000")
                                      (list :name (lang-text :en "Thun" :zh "雷" :jp "雷")
                                            :color "#000000")
                                      (list :name (lang-text :en "Ice" :zh "冰" :jp "氷")
                                            :color "#000000")
                                      (list :name (lang-text :en "Dra" :zh "龙" :jp "龍")
                                            :color "#000000")
                                      (list :name (lang-text :en "Pois" :zh "毒" :jp "毒")
                                            :color "#000000")
                                      (list :name (lang-text :en "Para" :zh "麻" :jp "麻")
                                            :color "#000000")
                                      (list :name (lang-text :en "Sleep" :zh "眠" :jp "眠")
                                            :color "#000000")
                                      (list :name (lang-text :en "Blast" :zh "爆破" :jp "爆破")
                                            :color "#000000"))
    "The information of the special attacks among weapons.")

  (defparameter +gunlance-shot-types+ (list (list :name (lang-text :en "Normal" :zh "通常" :jp "通常"))
                                            (list :name (lang-text :en "Wide" :zh "扩散" :jp "拡散"))
                                            (list :name (lang-text :en "Long" :zh "放射" :jp "放射")))
    "The shot types of gunlance.")

  (defparameter +bow-shot-types+ (list (list :name (lang-text :en "Pierce" :zh "" :jp ""))
                                       (list :name (lang-text :en "Rapid" :zh "" :jp ""))
                                       (list :name (lang-text :en "Spread" :zh "" :jp ""))
                                       (list :name (lang-text :en "Heavy" :zh "" :jp "")))
    "The shot types of bow."))


(defparameter *weapons* (make-array (length +weapon-types+)
                                    :initial-element nil)
  "An array of all dex weapons.")

(defun parse-sharpness (sharpness-string)
  "Parse the sharpness string, which is a 16-digit string grouped in 2
   digits. The first 2 digits represent to total sharpness value, and
   the rest represent the per-block sharpness in big edian."
  (let ((numbers (loop for start from 2 to 14 by 2
                    collect (handler-case
                                (parse-integer (subseq sharpness-string
                                                       start (+ start 2)))
                              (t () 0)))))
    (nreverse
     (loop 
        with enable-collect = nil
        for x in (rest numbers) 
        do (when (not (zerop x)) (setf enable-collect t))
        when enable-collect 
        collect x))))

(defun translate-gunlance-shot-type (id)
  "Translate the gunlance shot type ID from dex database into actual
  type ID and level."
  (cond ((<= id 5) (list :id 0 :level id))
        ((<= id 10) (list :id 1 :level (- id 5)))
        (t (list :id 2 :level (- id 10)))))

(defun reload-weapons ()
  "Reload the variable *weapons* which contains all the weapon
   information."
  ;; First, clear the *weapons* array.
  (loop for i below (length *weapons*)
     do (setf (aref *weapons* i) nil))
  
  ;; Load the data from the Dex database.
  (with-dex-queries ((weapons (:select "DB_Wpn.Wpn_ID" "Wpn_Type_ID"
                                       ;; names: 0 = en, 1 = zh, 3 = jp
                                       "Wpn_Name_0" "Wpn_Name_1" "Wpn_Name_3"
                                       "Rare" "Child"
                                       "Atk" "Affinity"
                                       "SpAtk1_ID" "SpAtk1_Pt" "SpAtk2_ID"" SpAtk2_Pt"
                                       "Def"
                                       "Slot"
                                       "Sharp" "SharpP1"
                                       "ProPx" "LvUpPx"
                                       ;; Gunlance
                                       "GLShotType_ID")
                              (:from "DB_Wpn")
                              (:inner-join "ID_Wpn_Name" "DB_Wpn.Wpn_ID = ID_Wpn_Name.Wpn_ID")
                              (:order-by "Wpn_Type_ID" "DB_Wpn.Wpn_ID"))
                     (materials (:select "Wpn_ID" "Itm_ID" "Qty" "Type")
                                (:from "DB_ItmtoWpn")))
    (let ((material-table (make-hash-table)))
      (loop for (dex-id item-id quantity type) in materials
         do (push (list :itemkey (make-item-key (1- item-id))
                        :quantity quantity)
                  ;; Use Dex ID here as it is what will be used to
                  ;; fetch the data below.
                  (getf (gethash dex-id material-table nil)
                        (if (equal type "P") :produce :upgrade))))

      (loop
         ;; Stack element is (count weapon), where count means how many
         ;; remaining children there are for this weapon.
         with stack = nil
         with id = 0
         for previous-dex-type-id = -1 then dex-type-id
         for (dex-id
              dex-type-id
              en zh jp 
              rare child
              attack affinity
              special-1 special-1-points special-2 special-2-points
              defense
              slot
              sharpness sharpness-plus
              produce-price upgrade-price
              gunlance-shot-type) in weapons
         do (let ((type-id (1- dex-type-id)))
              ;; Reset the id when starting a new type.
              (if (= previous-dex-type-id dex-type-id)
                  (incf id)
                  (setf id 0))
              (push (list :type type-id
                          :id id
                          :key (make-weapon-key type-id id)
                          :rare rare
                          :name (lang-text :en en :zh zh :jp jp)
                          :attack attack
                          :affinity (round (* 100 affinity))
                          :special (mapcan (lambda (special points)
                                             (when (plusp special)
                                               (list (list :special (1- special)
                                                           :points points))))
                                           (list special-1 special-2)
                                           (list special-1-points special-2-points))
                          :depth (length stack)
                          :defense defense
                          :slots slot
                          :children nil
                          :sharpness (list :blocks (parse-sharpness sharpness)
                                           :plus (if (= 1 dex-type-id) t :false))
                          :from (if stack
                                    (getf (cadar stack) :key)
                                    :null)
                          :material (gethash dex-id material-table nil)
                          :price (list :produce produce-price :upgrade upgrade-price))
                    (aref *weapons* type-id))

              ;; Type-specific Information
              ;;
              ;; type-id = 7, Gunlance
              (when (= type-id 7)
                (setf (getf (car (aref *weapons* type-id)) :shottype)
                      (translate-gunlance-shot-type gunlance-shot-type)))
              
              ;; Make the child-parent connection. Decrease the children
              ;; count for the parent (i.e. stack top).
              (when stack 
                (push id (getf (cadar stack) :children))
                (decf (caar stack)))
              (if (> child 0) 
                  (push (list child (car (aref *weapons* type-id)))
                        stack)
                  (loop while (and stack (= (caar stack) 0))
                     do (let ((children (getf (cadar stack) :children)))
                          (setf (getf (cadar stack) :children)
                                (nreverse children))
                          (pop stack)))))))
    
    ;; Reverse the lists of each weapon type.
    (loop for type-id below (length *weapons*)
       do (setf (aref *weapons* type-id)
                (nreverse (aref *weapons* type-id)))))
  (format t "[ ok ] Weapons loaded, total: ~a~%"
          (loop for weapons across *weapons*
             sum (length weapons))))

(declaim (inline ensure-weapons-loaded))
(defun ensure-weapons-loaded ()
  "Make sure that reload-weapon is already excuted and *weapons* has
   valid data. Call reload-weapons if not."
  (unless (loop for weapons across *weapons* always weapons)
    (reload-weapons)))

(defun get-weapon-list (type)
  "Return the list of the weapons of the specified type."
  (aref *weapons* type))

(defun get-weapon-type-list ()
  "Return the list of all weapon types, which is a list of their names
   in different languages."
  +weapon-types+)

(defun get-sharpness-color-list ()
  "Returns the list of sharpness colors."
  +sharpness-colors+)

(defun get-special-type-list ()
  "Returns the list of special attacks among weapons."
  +special-types+)

(defun get-gunlance-shot-types ()
  +gunlance-shot-types+)

(let ((*index* (make-array (length +weapon-types+) :initial-element nil)))
  (defun fetch-weapon-db (type &optional (ids nil))
    (cond ((null ids) (aref *weapons* type))
          (t (when (null (aref *index* type))
               (setf (aref *index* type) (make-hash-table))
               (loop for weapon in (aref *weapons* type)
                  do (setf (gethash (getf weapon :id)
                                    (aref *index* type))
                           weapon)))
             (loop for id in ids
                collect (gethash id (aref *index* type)))))))
