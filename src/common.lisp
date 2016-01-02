(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.common
    (:use :cl)
    (:import-from :alexandria
		  :with-gensyms)
    (:export :do-query
             :with-dex-queries
             :make-item-key
             :make-weapon-key
             :make-quest-key
             :make-skill-key
             :make-monster-key
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
    `(list ,@(list :en `(if ,en ,en ,jp))
           ,@(list :zh `(if ,zh ,zh ,jp))
           ,@(list :jp jp))))

(defun make-item-key (id)
  (format nil "~4,'0d" id))

(defun make-weapon-key (type id)
  (format nil "~2,'0d.~4,'0d" type id))

(defun make-quest-key (id)
  (format nil "~4,'0d" id))

(defun make-armor-key (id)
  (format nil "~4,'0d" id))

(defun make-skill-key (id)
  (format nil "~4,'0d" id))

(defun make-monster-key (id)
  (format nil "~3,'0d" id))

