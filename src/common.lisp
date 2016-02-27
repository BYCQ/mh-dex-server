(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage mh-dex.common
    (:use :cl)
    (:import-from :alexandria
		  :with-gensyms
                  :symbolicate)
    (:export :do-query
             :with-dex-queries
             :def-db-interface
             :make-used-item-map
             :make-item-key
             :make-weapon-key
             :make-quest-key
             :make-quest-key-from-dex
             :make-skill-key
             :make-monster-key
             :make-jewel-key
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
                            (:outer-join (format nil "LEFT OUTER JOIN ~a ON ~a"
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

  (defmacro def-db-interface (name db)
    (let ((index (symbolicate '* name '-index*)))
      (with-gensyms (key keys entry keyonly)
        `(let ((,index nil))
           (defun ,(symbolicate 'fetch- name '-db) (&optional (,keys nil) (,keyonly nil))
             (cond (,keyonly (mapcar (lambda (x) (getf x :key)) ,db))
                   ((null ,keys) ,db)
                   (t (when (null ,index)
                        (setf ,index (make-hash-table :test #'equal))
                        (loop for ,entry in ,db
                           do (setf (gethash (getf ,entry :id)
                                             ,index)
                                    ,entry)))
                      (loop for ,key in ,keys
                         collect (gethash ,key ,index :null)))))))))

  (defmacro lang-text (&key (en nil) (zh nil) (jp nil))
    `(list ,@(list :en `(if ,en ,en ,jp))
           ,@(list :zh `(if ,zh ,zh ,jp))
           ,@(list :jp jp))))

(defun make-used-item-map (entries)
  "Make a hashtable that maps id to a list of items with
   quantity. This is for item usage tables."
  (let ((result (make-hash-table)))
    (loop for (dex-id item-dex-id quantity) in entries
       do (push (list :itemkey (make-item-key (1- item-dex-id))
                      :quantity quantity)
                (gethash dex-id result nil)))
    result))

(defun make-item-key (id)
  (format nil "~4,'0d" id))

(defun make-weapon-key (type id)
  (format nil "~2,'0d.~4,'0d" type id))

(defun make-quest-key (id)
  (format nil "~4,'0d" id))

(defun make-quest-key-from-dex (dex-id)
  "Dex has ID gap between normal quests and DLC quests. This is a hack
   to fix it."
  (format nil "~4,'0d" 
          (if (<= dex-id 643)
              (1- dex-id)
              (- dex-id 58))))

(defun make-armor-key (id)
  (format nil "~4,'0d" id))

(defun make-skill-key (id)
  (format nil "~4,'0d" id))

(defun make-monster-key (id)
  (format nil "~3,'0d" id))

(defun make-jewel-key (id)
  (format nil "~3,'0d" id))

(defun make-location-key (id)
  (format nil "~2,'0d" id))



