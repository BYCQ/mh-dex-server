(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.common
    (:use :cl)
    (:import-from :alexandria
		  :with-gensyms)
    (:export :do-query
             :with-dex-queries
             :lang-text)))
(in-package :mh-dex.common)

;; -------------------- Constants --------------------

(defparameter *dex-database-path*
  (merge-pathnames "data/mhx.db"
                   (asdf:system-source-directory :mh-dex))
  "The path to the dex SQLite databse for Monster Hunter X.")

;; -------------------- Helpers --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-query (database &body body)
    `(sqlite:execute-to-list
      ,database
      ,(format nil "~{~a~^ ~};"
               (loop for form in body
                  collect (ecase (car form)
                            (:select (format nil "SELECT ~{~a~^, ~}"
                                             (rest form)))
                            (:from (format nil "FROM ~a" (second form)))
                            (:where (format nil "WHERE ~{~a~^ AND ~}"
                                            (rest form)))
                            (:limit (format nil "LIMIT ~a" (second form)))
                            (:group-by (format nil "GROUP BY ~{~a~^, ~}"
                                               (rest form)))
                            (:inner-join (format nil "INNER JOIN ~a ON ~a"
                                                 (second form)
                                                 (third form)))
                            (:order-by (format nil "ORDER BY ~{~a~^, ~}"
                                               (rest form))))))))

  (defmacro with-dex-queries (query-bindings &body body)
    (with-gensyms (database)
      `(sqlite:with-open-database (,database *dex-database-path*)
  	 (let ,(mapcar (lambda (binding)
  			 (list (car binding)
  			       `(do-query ,database ,@(rest binding))))
                       query-bindings)
  	   ,@body))))

  (defmacro lang-text (&key (en nil) (zh nil) (jp nil))
    `(list ,@(when en (list :en en))
           ,@(when zh (list :zh zh))
           ,@(when jp (list :jp jp)))))

      
                    






