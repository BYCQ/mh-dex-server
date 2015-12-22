(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
      (:use :cl)
      (:import-from :jonathan
                    :to-json
                    :%to-json
                    :write-key-value
                    :with-object)
      (:export :get-weapon-entries
               :ensure-weapon-list)))

