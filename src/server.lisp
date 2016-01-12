(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.server
    (:import-from :realispic
                  :def-app
                  :def-service)
    (:import-from :mh-dex.weapon
                  :reload-weapons
                  :fetch-weapon-db
                  :get-weapon-list
                  :get-weapon-type-list
                  :get-special-type-list
                  :get-sharpness-color-list)
    (:import-from :mh-dex.item
                  :reload-items
                  :get-item-type-list
                  :fetch-item-db
                  :get-item-list)
    (:import-from :mh-dex.quest
                  :reload-quests
                  :fetch-quest-db
                  :get-quest-list)
    (:import-from :mh-dex.armor
                  :reload-armors
                  :fetch-armor-db
                  :get-armor-list)
    (:import-from :mh-dex.skill
                  :reload-skills
                  :fetch-skill-db
                  :get-skill-list)
    (:import-from :mh-dex.monster
                  :reload-monsters
                  :fetch-monster-db
                  :get-monster-list)
    (:import-from :mh-dex.jewel
                  :reload-jewels
                  :fetch-jewel-db
                  :get-jewel-list)
    (:import-from :alexandria
                  :symbolicate)
    (:export :dex
             :init-server)
    (:use :cl)))
(in-package :mh-dex.server)

;; -------------------- Operations --------------------

(defparameter *initialized* nil
  "Inidicating that the server is fully initialized.")

(defparameter *server-version* "88426"
  "The version of the server. Used check the staleness of the client
   data.")

(defun init-server ()
  "Load all the data from the Dex databse to initialzie the server."
  (unless *initialized*
    (reload-skills)
    (reload-weapons)
    (reload-items)
    (reload-quests)
    (reload-armors)
    (reload-monsters)
    (reload-jewels)
    (setf *initialized* t)))

;; -------------------- RPCs --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-dex-service (name args &body body)
    `(def-service ,name ,args
       (init-server)
       ,@body))

  (defmacro def-dex-db-service (name)
    `(def-service ,(symbolicate name '-db) (keys keyonly)
       (init-server)
       (list :version *server-version*
             :data (,(symbolicate 'fetch- name '-db)
                     (when keys
                       (let ((input (make-string-input-stream keys)))
                         (read input)))
                     keyonly)))))

(def-dex-service environment ()
  (list :version *server-version*
        :weapontypes (get-weapon-type-list)
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

(def-dex-service jewel-list ()
  (get-jewel-list))

;; -------------------- DB handlers --------------------

(def-dex-db-service skill)
(def-dex-db-service armor)
(def-dex-db-service jewel)
(def-dex-db-service monster)
(def-dex-db-service quest)
(def-dex-db-service item)

(def-dex-service weapon-db (type ids)
  (list :version *server-version*
        :data (fetch-weapon-db (parse-integer type)
                               (when ids
                                 (let ((input (make-string-input-stream ids)))
                                   (read input))))))

(def-app dex ()
  :title "Ping's Dex"
  :port 5120
  :system :mh-dex
  :widget (:div ()))
