(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.server
    (:import-from :realispic
                  :def-app
                  :def-service)
    (:import-from :mh-dex.weapon
                  :reload-weapons
                  :get-weapon-list
                  :get-weapon-type-list
                  :get-special-type-list
                  :get-sharpness-color-list)
    (:import-from :mh-dex.item
                  :reload-items
                  :get-item-type-list
                  :get-item-list)
    (:import-from :mh-dex.quest
                  :reload-quests
                  :get-quest-list)
    (:import-from :mh-dex.armor
                  :reload-armors
                  :get-armor-list)
    (:import-from :mh-dex.skill
                  :reload-skills
                  :get-skill-list)
    (:import-from :mh-dex.monster
                  :reload-monsters
                  :get-monster-list)
    (:export :dex
             :init-server)
    (:use :cl)))
(in-package :mh-dex.server)

;; -------------------- Operations --------------------

(defparameter *initialized* nil
  "Inidicating that the server is fully initialized.")

(defun init-server ()
  "Load all the data from the Dex databse to initialzie the server."
  (unless *initialized*
    (reload-skills)
    (reload-weapons)
    (reload-items)
    (reload-quests)
    (reload-armors)
    (reload-monsters)
    (setf *initialized* t)))

;; -------------------- RPCs --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-dex-service (name args &body body)
    `(def-service ,name ,args
       (init-server)
       ,@body)))

(def-dex-service environment ()
  (list :weapontypes (get-weapon-type-list)
        :specialtypes (get-special-type-list)
        :sharpnesscolors (get-sharpness-color-list)
        :itemtypes (get-item-type-list)))

(def-dex-service weapon-list (type)
  (get-weapon-list (parse-integer type)))

(def-dex-service item-list ()
  (get-item-list))

(def-dex-service quest-list ()
  (get-quest-list))

(def-dex-service armor-list ()
  (get-armor-list))

(def-dex-service skill-list ()
  (get-skill-list))

(def-dex-service monster-list ()
  (get-monster-list))

(def-app dex ()
  :title "Ping's Dex"
  :port 5120
  :system :mh-dex
  :widget (:div ()))
  





