(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.weapon
    (:use :cl)
    (:export :get-weapon-entries
             :ensure-weapon-list)))
(in-package :mh-dex.weapon)

(defparameter *weapons* nil
  "An array of all dex weapons."





