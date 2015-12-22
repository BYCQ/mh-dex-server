(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.common
    (:use :cl)
    (:export :get-weapon-entries
             :ensure-weapon-list)))
(in-package :mh-dex.common)

;; -------------------- Constants --------------------

(defparameter *dex-database-path*
  (merge-pathnames "data/mhx.db"
                   (asdf:system-source-directory :mh-dex))
  "The path to the dex SQLite databse for Monster Hunter X.")


;; -------------------- Structs/Classes --------------------

(defclass weapon ()
  ((dex-id :type (unsigned-byte 32) :initarg :dex-id)
   (type-id :type (unsigned-byte 8) :initarg :type-id)
   (base-id :type (unsigned-byte 16) :initarg :base-id)
   (level :type (unsigned-byte 8) :initarg :level)
   (attack :type (unsigned-byte 32) :initarg :attack)))









