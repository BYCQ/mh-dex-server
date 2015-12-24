(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.weapon
    (:use :cl)
    (:import-from :mh-dex.common
                  :with-dex-queries
                  :lang-text)
    (:export :*weapons*
             :get-weapon-list
             :get-weapon-type-list
             :reload-weapons)))
(in-package :mh-dex.weapon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +weapon-types+ (list (lang-text :en "Great Sword" 
                                                :zh "大剑"
                                                :jp "大剣")
                                     (lang-text :en "Long Sword"
                                                :zh "太刀"
                                                :jp "太刀")
                                     (lang-text :en "Sowrd and Shield"
                                                :zh "片手剑"
                                                :jp "片手剣")
                                     (lang-text :en "Dual Blades"
                                                :zh "双剑"
                                                :jp "双剣")
                                     (lang-text :en "Hammer"
                                                :zh "大锤"
                                                :jp "ハンマー")
                                     (lang-text :en "Hunting Horn"
                                                :zh "狩猎笛"
                                                :jp "狩猟笛")
                                     (lang-text :en "Lance"
                                                :zh "长枪"
                                                :jp "ランス")
                                     (lang-text :en "Gunlance"
                                                :zh "铳枪"
                                                :jp "ガンランス")
                                     (lang-text :en "Switch Axe"
                                                :zh "斩击斧"
                                                :jp "スラッシュアックス")
                                     (lang-text :en "Charge Blade"
                                                :zh "盾斧"
                                                :jp "チャージアックス")
                                     (lang-text :en "Insect Glaive"
                                                :zh "操虫棍"
                                                :jp "操虫棍")
                                     (lang-text :en "Light Bowgun"
                                                :zh "轻弩"
                                                :jp "ライトボウガン")
                                     (lang-text :en "Heavy Bowgun"
                                                :zh "重弩"
                                                :jp "ヘビィボウガン")
                                     (lang-text :en "Bow"
                                                :zh "弓"
                                                :jp "弓"))
    "The names of the weapon-types."))

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
                                       "ProPx" "LvUpPx")
                              (:from "DB_Wpn")
                              (:inner-join "ID_Wpn_Name" "DB_Wpn.Wpn_ID = ID_Wpn_Name.Wpn_ID")
                              (:order-by "Wpn_Type_ID" "DB_Wpn.Wpn_ID")))
    (loop 
       ;; Stack element is (count weapon), where count means how many
       ;; remaining children there are for this weapon.
       with stack = nil
       with id = 0
       for previous-dex-type-id = -1 then dex-type-id
       for (dex-id ;; unused
            dex-type-id
            en zh jp 
            rare child
            attack affinity
            special-1 special-1-points special-2 special-2-points
            defense
            slot
            sharpness sharpness-plus
            produce-price upgrade-price) in weapons
       do (let ((type-id (1- dex-type-id)))
            ;; Reset the id when starting a new type.
            (if (= previous-dex-type-id dex-type-id)
                (incf id)
                (setf id 0))
            (push (list :type type-id
                        :id id
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
                        :defense defense
                        :children nil
                        :sharpness (list :blocks (parse-sharpness sharpness)
                                         :plus (if (= 1 dex-type-id) t :false))
                        :price (list :produce produce-price :upgrade upgrade-price))
                  (aref *weapons* type-id))
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
                        (pop stack))))))
         
    ;; Reverse the lists of each weapon type.
    (loop for type-id below (length *weapons*)
       do (setf (aref *weapons* type-id)
                (nreverse (aref *weapons* type-id)))))
  (format t "[ ok ] Weapons loaded.~%"))

(defun get-weapon-list (type)
  "Return the list of the weapons of the specified type."
  (aref *weapons* type))

(defun get-weapon-type-list ()
  "Return the list of all weapon types, which is a list of their names
   in different languages."
  +weapon-types+)
