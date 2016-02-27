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
             :get-charge-blade-phials
             :get-switch-axe-phials
             :get-bullet-types
             :get-inner-bullet-types
             :get-bow-shot-types
             :get-bow-arc-types
             :get-coating-types
             :get-reload-types
             :get-recoil-types
             :get-steadiness-types
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
                         (:charge-blade-phial '(list :name "chargeBladePhial" :zh "瓶" :jp "ビン" :en "Phial"))
                         (:switch-axe-phial '(list :name "switchAxePhial" :zh "瓶" :jp "ビン" :en "Phial"))
                         (:bow-arc '(list :name "bowArc" :zh "曲射" :jp "曲射" :en "Arc"))
                         (:bow-shot '(list :name "bowShot" :zh "蓄力" :jp "溜め" :en "Charge"))
                         (:notes '(list :name "notes" :zh "音符" :jp "音色" :en "Notes"))
                         (:reload '(list :name "reload" :zh "装弹" :jp "リロード" :en "Reload"))
                         (:steadiness '(list :name "steadiness" :zh "摇晃" :jp "ブレ" :en "Shake"))
                         (:recoil '(list :name "recoil" :zh "后坐力" :jp "反動" :en "Recoil"))
                         (:rapid '(list :name "rapid" :zh "速射" :jp "速射" :en "Rapid"))
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
                                                                    :affinity :special :notes :sharpness))
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
                                                                    :affinity :special :switch-axe-phial
                                                                    :sharpness))
                                     (list :name (lang-text :en "Charge Blade"
                                                            :zh "盾斧"
                                                            :jp "チャージアックス")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special
                                                                    :charge-blade-phial
                                                                    :sharpness))
                                     (list :name (lang-text :en "Insect Glaive"
                                                            :zh "操虫棍"
                                                            :jp "操虫棍")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :sharpness))
                                     (list :name (lang-text :en "Light Bowgun"
                                                            :zh "轻弩"
                                                            :jp "ライトボウガン")
                                           :columns (create-columns :name :slots :attack :rapid
                                                                    :affinity :reload :steadiness :recoil))
                                     (list :name (lang-text :en "Heavy Bowgun"
                                                            :zh "重弩"
                                                            :jp "ヘビィボウガン")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :reload :steadiness :recoil))
                                     (list :name (lang-text :en "Bow"
                                                            :zh "弓"
                                                            :jp "弓")
                                           :columns (create-columns :name :slots :attack
                                                                    :affinity :special :bow-arc :bow-shot)))
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

  (defparameter +charge-blade-phials+ (list (list :name (lang-text :en "Element" :zh "强属性瓶" :jp "強属性ビン"))
                                            (list :name (lang-text :en "Impact" :zh "榴弹瓶" :jp "榴弾ビン")))
    "The phial types of charge blade.")

  (defparameter +switch-axe-phials+ (list (list :name (lang-text :en "Power" :zh "强击瓶" :jp "強撃ビン"))
                                          (list :name (lang-text :en "Para" :zh "麻痹瓶" :jp "麻痺ビン"))
                                          (list :name (lang-text :en "Poison" :zh "毒瓶" :jp "毒ビン"))
                                          (list :name (lang-text :en "Dragon" :zh "灭龙瓶" :jp "滅龍ビン"))
                                          (list :name (lang-text :en "Element" :zh "强属性瓶" :jp "強属性ビン"))
                                          (list :name (lang-text :en "Exhaust" :zh "减气瓶" :jp "減気ビン")))
    "The phial types of switch axe.")

  (defparameter +gunlance-shot-types+ (list (list :name (lang-text :en "Normal" :zh "通常" :jp "通常"))
                                            (list :name (lang-text :en "Wide" :zh "扩散" :jp "拡散"))
                                            (list :name (lang-text :en "Long" :zh "放射" :jp "放射")))
    "The shot types of gunlance.")

  (defparameter +bow-shot-types+ (list (list :name (lang-text :en "Pierce" :zh "贯通" :jp "貫通"))
                                       (list :name (lang-text :en "Rapid" :zh "连射" :jp "連射"))
                                       (list :name (lang-text :en "Spread" :zh "扩散" :jp "拡散"))
                                       (list :name (lang-text :en "Heavy" :zh "重射" :jp "重射")))
    "The shot types of bow.")

  (defparameter +bow-arc-types+ (list (list :name (lang-text :en "Wide" :zh "放射型" :jp "放散型"))
                                      (list :name (lang-text :en "Focus" :zh "集中型" :jp "集中型"))
                                      (list :name (lang-text :en "Blast" :zh "爆裂型" :jp "爆裂型"))
                                      (list :name (lang-text :en "Power" :zh "刚射型" :jp "剛射型")))
    "The arc shot types of bow.")

  (defparameter +coating-types+ (list (list :name (lang-text :EN "Power Lv1" :ZH "LV1 强击瓶" :JP "LV1 強撃ビン"))
                                      (list :name (lang-text :EN "Power Lv2" :ZH "LV2 强击瓶" :JP "LV2 強撃ビン"))
                                      (list :name (lang-text :EN "Element Lv1" :ZH "LV1 属性强化瓶" :JP "LV1 属性強化ビン"))
                                      (list :name (lang-text :EN "Element Lv2" :ZH "LV2 属性强化瓶" :JP "LV2 属性強化ビン"))
                                      (list :name (lang-text :EN "C.Range" :ZH "接击瓶" :JP "接撃ビン"))
                                      (list :name (lang-text :EN "Poison" :ZH "毒瓶" :JP "毒ビン"))
                                      (list :name (lang-text :EN "Para" :ZH "麻痹瓶" :JP "麻痺ビン"))
                                      (list :name (lang-text :EN "Sleep" :ZH "睡眠瓶" :JP "睡眠ビン"))
                                      (list :name (lang-text :EN "Exhaust" :ZH "减气瓶" :JP "減気ビン"))
                                      (list :name (lang-text :EN "Blast" :ZH "爆破瓶" :JP "爆破ビン"))
                                      (list :name (lang-text :EN "Paint" :ZH "染色瓶" :JP "ペイントビン")))
    "The types of bow coatings.")

  (defparameter +reload-types+ (list (list :name (lang-text :en "Fast" :zh "快" :jp "速い"))
                                     (list :name (lang-text :en "Abv.Avg" :zh "稍快" :jp "やや速い"))
                                     (list :name (lang-text :en "Average" :zh "普通" :jp "普通"))
                                     (list :name (lang-text :en "Bel.Avg" :zh "稍慢" :jp "やや遅い"))
                                     (list :name (lang-text :en "Slow" :zh "慢" :jp "遅い")))
    "The types of bowgun reload.")

  (defparameter +recoil-types+ (list (list :name (lang-text :en "Low" :zh "小" :jp "小"))
                                     (list :name (lang-text :en "Some" :zh "偏小" :jp "やや小"))
                                     (list :name (lang-text :en "Average" :zh "中" :jp "中"))
                                     (list :name (lang-text :en "High" :zh "大" :jp "大")))
    "The types of bowgun recoil.")

  (defparameter +steadiness-types+ (list (list :name (lang-text :en "(None)" :zh "无" :jp "なし"))
                                         (list :name (lang-text :en "LR Mild" :zh "左右/小" :jp "左右/小"))
                                         (list :name (lang-text :en "LR Severe" :zh "左右/大" :jp "左右/大"))
                                         (list :name (lang-text :en "L Mild" :zh "左侧/小" :jp "左側/小"))
                                         (list :name (lang-text :en "L Severe" :zh "左侧/大" :jp "左側/大"))
                                         (list :name (lang-text :en "R Mild" :zh "右侧/小" :jp "右側/小"))
                                         (list :name (lang-text :en "R Severe" :zh "右侧/大" :jp "右側/大")))
    "The types of bowgun steadiness.")

  (defparameter +bullet-types+ (list (list :name (lang-text :EN "Normal S" :ZH "通常弹" :JP "通常弾") :level 3)
                                     (list :name (lang-text :EN "Pierce S" :ZH "贯通弹" :JP "貫通弾") :level 3)
                                     (list :name (lang-text :EN "Pellet S" :ZH "散弹" :JP "散弾") :level 3)
                                     (list :name (lang-text :EN "Crag S" :ZH "彻甲榴弹" :JP "徹甲榴弾") :level 3)
                                     (list :name (lang-text :EN "Clust S" :ZH "扩散弹" :JP "拡散弾") :level 3)
                                     (list :name (lang-text :EN "Flaming S1" :ZH "火炎弹" :JP "火炎弾") :level 1)
                                     (list :name (lang-text :EN "Water S" :ZH "水冷弹" :JP "水冷弾") :level 1)
                                     (list :name (lang-text :EN "Thunder S" :ZH "电击弹" :JP "電撃弾") :level 1)
                                     (list :name (lang-text :EN "Freeze S" :ZH "冰结弹" :JP "氷結弾") :level 1)
                                     (list :name (lang-text :EN "Dragon S" :ZH "灭龙弹" :JP "滅龍弾") :level 1)
                                     (list :name (lang-text :EN "Poison S" :ZH "毒弹" :JP "毒弾") :level 2)
                                     (list :name (lang-text :EN "Para S" :ZH "麻痹弹" :JP "麻痺弾") :level 2)
                                     (list :name (lang-text :EN "Sleep S" :ZH "睡眠弹" :JP "睡眠弾") :level 2)
                                     (list :name (lang-text :EN "Exhaust S" :ZH "减气弹" :JP "減気弾") :level 2)
                                     (list :name (lang-text :EN "Recover S" :ZH "回复弹" :JP "回復弾") :level 2)
                                     (list :name (lang-text :EN "Paint S" :ZH "染色弹" :JP "ペイント弾") :level 1)
                                     (list :name (lang-text :EN "Tranq S" :ZH "捕获用麻醉弹" :JP "捕獲用麻酔弾") :level 1))
    "All bowgun bullet types (without inner bullet types).")

  (defparameter +inner-bullet-types+ (list (list :name (lang-text :ZH nil :EN nil :JP "ツブテ弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "竜撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 斬裂弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 斬裂弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 爆破弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 爆破弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "鬼人弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "鬼人会心弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "硬化弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "硬化強靭弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 強装弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 強装弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 重撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 重撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "痛撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 火炎弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 水冷弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 電撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 氷結弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 滅龍弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 貫通火炎弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 貫通火炎弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 貫通水冷弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 貫通水冷弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 貫通電撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 貫通電撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 貫通氷結弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 貫通氷結弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 大砲弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 大砲弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV1 遠撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "LV2 遠撃弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "連爆榴弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "榴散弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "烈光弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "毒煙弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "閃光弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "こやし弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "治癒活力弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "広域回復弾"))
                                           (list :name (lang-text :ZH nil :EN nil :JP "鬼人硬化弾")))
    "All inner bullet types."))

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

(defun translate-bow-shot-type (id)
  "Translate the bow shot type ID from dex database into actual
  type ID and level."
  (cond ((<= id 20) (list :id 0 :level (- id 15)))
        ((<= id 25) (list :id 1 :level (- id 20)))
        ((<= id 30) (list :id 2 :level (- id 25)))
        ((<= id 35) (list :id 3 :level (- id 30)))))

(defun encode-bullets (bullet-list)
  (assert (= (length bullet-list) 32))
  (with-output-to-string (stream)
    (loop for char in bullet-list do (princ char stream))))

(defun encode-coatings (coating-list)
  (assert (= (length coating-list) 11))
  (with-output-to-string (stream)
    (loop for bit in coating-list do (princ bit stream))))

(defun item-to-bullet (dex-item-id)
  (if (> dex-item-id 1000)
      (list :inner t
            :id (- dex-item-id 1853)
            :level 0)
      (let ((seq (- dex-item-id 98)))
        (loop
           for i = 0 then (+ i (getf bullet-type :level))
           for bullet-id from 0
           for bullet-type in +bullet-types+
           when (and (>= seq i)
                     (< (- seq i) (getf bullet-type :level)))
           return (list :inner :false
                        :id bullet-id
                        :level (if (= (getf bullet-type :level) 1)
                                   0
                                   (1+ (- seq i))))))))

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
                                       "GLShotType_ID"
                                       ;; Charge Blade and Axe
                                       "AxePhial_ID"
                                       ;; Hunting Horn
                                       "HHNote1_ID" "HHNote2_ID" "HHNote3_ID"
                                       ;; Bowgun
                                       "GunReloadSpd_ID" "GunSteadiness_ID" "GunRecoil_ID"
                                       ;; Bow
                                       "BowShot_ID" "BowShotType1_ID"
                                       "BowShotType2_ID" "BowShotType3_ID" "BowShotType4_ID")
                              (:from "DB_Wpn")
                              (:inner-join "ID_Wpn_Name" "DB_Wpn.Wpn_ID = ID_Wpn_Name.Wpn_ID")
                              (:order-by "Wpn_Type_ID" "DB_Wpn.Wpn_ID"))
                     (materials (:select "Wpn_ID" "Itm_ID" "Qty" "Type")
                                (:from "DB_ItmtoWpn"))
                     (bullets (:select "Wpn_ID" "Itm_ID" "LoadQty" "LoadQtyP" "SpFireQty")
                              (:from "DB_Wpn_Gun")
                              (:where "Itm_ID >= 98 AND Itm_ID <= 129")
                              (:order-by "Wpn_ID" "-Itm_ID"))
                     (inner-bullets (:select "Wpn_ID" "Itm_ID" "LoadQty" "Max" "SpFireQty")
                                    (:from "DB_Wpn_Gun")
                                    (:where "Itm_ID >= 1853 AND LoadQty > 0")
                                    (:order-by "Wpn_ID" "-Itm_ID"))
                     (coatings (:select "Wpn_ID" "Itm_ID" "LoadQty")
                               (:from "DB_Wpn_Gun")
                               (:where "Itm_ID >= 131 AND Itm_ID <= 141")
                               (:order-by "Wpn_ID" "-Itm_ID")))
                     
    (let ((material-table (make-hash-table))
          (bullet-table (make-hash-table))
          (inner-bullet-table (make-hash-table))
          (rapid-fire-table (make-hash-table))
          (coating-table (make-hash-table)))
      (loop for (dex-id item-id quantity type) in materials
         do (push (list :itemkey (make-item-key (1- item-id))
                        :quantity quantity)
                  ;; Use Dex ID here as it is what will be used to
                  ;; fetch the data below.
                  (getf (gethash dex-id material-table nil)
                        (if (equal type "P") :produce :upgrade))))

      ;; Create the mapping between bowgun and ordinary bullets
      (loop for (dex-id dex-item-id load load-plus rapid-number) in bullets
         do (progn (push (cond ((> load 0) (code-char (+ 48 load)))
                               ((> load-plus 0) (code-char (+ 97 load-plus)))
                               (t #\0))
                         (gethash dex-id bullet-table nil))
                   (when (> rapid-number 0)
                     (push (item-to-bullet dex-item-id)
                           (gethash dex-id rapid-fire-table)))))

      ;; Create the mapping between bowgun and inner bullets
      (loop for (dex-id dex-item-id load quantity rapid-number) in inner-bullets
         do (progn (push (list :id (- dex-item-id 1853)
                               :load load
                               :quantity quantity)
                         (gethash dex-id inner-bullet-table nil))
                   (when (> rapid-number 0)
                     ;; use +1000 encoding for inner-bullets
                     (push (item-to-bullet dex-item-id)
                           (gethash dex-id rapid-fire-table)))))
      
      ;; Create coating mapping from bow to coatings.
      (loop for (dex-id dex-item-id load) in coatings
         do (push (if (plusp load) #\1 #\0)
                  (gethash dex-id coating-table)))
      
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
              gunlance-shot-type
              axe-phial
              note-1 note-2 note-3
              reload steadiness recoil
              arc shot-type-1 shot-type-2 shot-type-3 shot-type-4) in weapons
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
                                           :plus (if (= 1 sharpness-plus) t :false))
                          :from (if stack
                                    (getf (cadar stack) :key)
                                    :null)
                          :material (gethash dex-id material-table nil)
                          :price (list :produce produce-price :upgrade upgrade-price))
                    (aref *weapons* type-id))

              ;; Type-specific Information
              ;;
              ;; type-id = 5, Hunting Horn
              (when (= type-id 5)
                (setf (getf (car (aref *weapons* type-id)) :notes)
                      (list note-1 note-2 note-3)))

              ;; type-id = 7, Gunlance
              (when (= type-id 7)
                (setf (getf (car (aref *weapons* type-id)) :shottype)
                      (translate-gunlance-shot-type gunlance-shot-type)))

              ;; type-id = 8, Switch Axe
              (when (= type-id 8)
                (setf (getf (car (aref *weapons* type-id)) :phial)
                      (1- axe-phial)))
              
              ;; type-id = 9, Charge Blade
              (when (= type-id 9)
                (setf (getf (car (aref *weapons* type-id)) :phial)
                      (ecase axe-phial
                        (5 0)
                        (7 1))))

              ;; typ-id = 11/12, Bowgun
              (when (or (= type-id 11) (= type-id 12))
                (setf (getf (car (aref *weapons* type-id)) :reload) (1- reload))
                (setf (getf (car (aref *weapons* type-id)) :steadiness) steadiness)
                (setf (getf (car (aref *weapons* type-id)) :recoil) (1- recoil))
                (setf (getf (car (aref *weapons* type-id)) :bullets)
                      (encode-bullets (gethash dex-id bullet-table)))
                (setf (getf (car (aref *weapons* type-id)) :inners)
                      (gethash dex-id inner-bullet-table nil)))

              ;; Light Bowgun
              (when (= type-id 11)
                (setf (getf (car (aref *weapons* type-id)) :rapid)
                      (gethash dex-id rapid-fire-table)))

              ;; type-id = 13, Bow
              (when (= type-id 13)
                (setf (getf (car (aref *weapons* type-id)) :coatings)
                      (encode-coatings (gethash dex-id coating-table)))
                (setf (getf (car (aref *weapons* type-id)) :arc) (1- arc))
                (setf (getf (car (aref *weapons* type-id)) :shottypes)
                      (loop for shot-type-id in (list shot-type-1 shot-type-2
                                                      shot-type-3 shot-type-4)
                         while (> shot-type-id 0)
                         collect (translate-bow-shot-type shot-type-id))))
              
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

(defun get-charge-blade-phials ()
  +charge-blade-phials+)

(defun get-switch-axe-phials ()
  +switch-axe-phials+)

(defun get-bow-shot-types ()
  +bow-shot-types+)

(defun get-bow-arc-types ()
  +bow-arc-types+)

(defun get-bullet-types ()
  +bullet-types+)

(defun get-inner-bullet-types ()
  +inner-bullet-types+)

(defun get-coating-types ()
  +coating-types+)

(defun get-reload-types ()
  +reload-types+)

(defun get-recoil-types ()
  +recoil-types+)

(defun get-steadiness-types ()
  +steadiness-types+)

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
