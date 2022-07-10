;;; -*- Mode: LISP; Base: 10; Syntax: Ansi-Common-Lisp; Package: SQLDF -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:sqldf)

(defun execute-to-column (db sql &rest parameters)
  (declare (dynamic-extent parameters))
  (sqlite::with-prepared-statement stmt (db sql parameters)
    (loop while (sqlite:step-statement stmt)
          collect (sqlite:statement-column-value stmt 0))))

(defun statement-column-type (stmt column-number)
  "Return the type string of a column of a query statement"
  (ecase (sqlite-ffi:sqlite3-column-type (sqlite::handle stmt) column-number)
    (:integer "INTEGER")
    (:float "REAL")
    (:text "TEXT")
    (:blob "BLOB")
    (:null nil)))

;;; to-sql-name and from-sql-name came from postmodern
(defparameter *sqlite-reserved-words* nil)
(defparameter *downcase-symbols* nil)
(defparameter *ESCAPE-SQL-NAMES-P* nil)

(defun to-sql-name (name &optional (escape-p *escape-sql-names-p*)
                           (ignore-reserved-words nil))
  "Convert a symbol or string into a name that can be a sql table, column, or
operation name. Add quotes when escape-p is true, or escape-p is :auto and the
name contains reserved words. Quoted or delimited identifiers can be used by
passing :literal as the value of escape-p. If escape-p is :literal, and the
name is a string then the string is still escaped but the symbol or string is
not downcased, regardless of the setting for *downcase-symbols* and the
hyphen and forward slash characters are not replaced with underscores.
Ignore-reserved-words is only used internally for column names which are allowed
to be reserved words, but it is not recommended."
  (declare (optimize (speed 3) (debug 0)))
  (let ((*print-pretty* nil)
        (name (if (and (consp name) (eq (car name) 'quote) (equal (length name)
                                                                  2))
                  (string (cadr name))
                  (string name))))
    (with-output-to-string (*standard-output*)
      (flet ((subseq-downcase (str from to)
               (let ((result (make-string (- to from))))
                 (loop :for i :from from :below to
                       :for p :from 0
                       :do (setf (char result p)
                                 (if (and *downcase-symbols*
                                          (not (eq escape-p :literal)))
                                     (char-downcase (char str i))
                                     (char str i))))
                 result))
             (write-element (str)
               (declare (type string str))
               (let ((escape-p (cond ((and (eq escape-p :auto)
                                           (not ignore-reserved-words))
                                      (gethash str *sqlite-reserved-words*))
                                     (ignore-reserved-words nil)
                                     (t escape-p))))
                 (when escape-p
                   (write-char #\"))
                 (if (and (> (length str) 1) ;; Placeholders like $2
                          (char= (char str 0) #\$)
                          (every #'digit-char-p (the string (subseq str 1))))
                     (princ str)
                     (loop :for ch :of-type character :across str
                           :do (if (or (eq ch #\*)
                                       (alphanumericp ch)
                                       (eq escape-p :literal))
                                   (write-char ch)
                                   (write-char #\_))))
                 (when escape-p
                   (write-char #\")))))

        (loop :for start := 0 :then (1+ dot)
              :for dot := (position #\. name) :then (position #\. name
                                                              :start start)
              :do (write-element (subseq-downcase name start
                                                  (or dot (length name))))
              :if dot :do (princ #\_)
                :else :do (return))))))

(defun from-sql-name (str)
  "Convert a string to a symbol, upcasing and replacing underscores with
hyphens."
  (map 'string (lambda (x) (if (eq x #\_) #\- x))
       (if (eq (readtable-case *readtable*) :upcase)
           (string-upcase str)
           str)))

(defun sqlite-column-type (sequence)
  "Return a format string for the most general type found in sequence
Use this for sequences of type T to determine how to declare the column to SQLite."
  (when (bit-vector-p sequence) (return-from sqlite-column-type "INTEGER"))
  (case (df:column-type sequence)
    (:single-float "REAL")
    (:double-float "REAL")
    (:integer "INTEGER")
    (:bit "INTEGER")
    (:symbol "TEXT")
    (t "TEXT")))
