;;; -*- Mode: LISP; Base: 10; Syntax: Ansi-Common-Lisp; Package: SQLDF -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:sqldf)

;; TODO: handle packages for the keys too
(defun sqldf (sql)
  "Execute SQL (a string) on a data frame and return a new data frame with the results.
The data frame is identified by the word following FROM (case insensitive) in the SQL string.  An in-memory SQLite database is creaetd, the contents of the data frame loaded, the query performed and a new DATA-FRAME returned with the results and the database deleted.  In most cases, using this library is faster, from a developers time perspective, than writing the code to perform the same query.  SQLDF has been tested with data frames of 350K rows with no slow-down noted.  The R documentation for their version of SQLDF suggests that it could be faster than Lisp native queries.  Note that the SQL query must use SQL style names for columns and not the Lisp versions, e.g. flight-time becomes flight_time."
  (let* ((db    (sqlite:connect ":memory:"))
	 (words (uiop:split-string sql))
	 (table (nth (1+ (position "from" words :test #'string=)) words))
	 (df-name (find-symbol (string-upcase table)))
	 df nrows data)

    (if (not (and (boundp df-name)
		  (typep (symbol-value df-name) 'df:data-frame)))
	(error "Could not find data frame ~A" table)
	(setf df (symbol-value df-name)))

    (create-df-table db table df)
    (write-table     db table df)

    ;; There's no good way to get the number of rows in a query in SQLite
    (setf nrows (sqlite:execute-single db (format nil "select count (1) from (~A)" sql)))

    (sqlite::with-prepared-statement stmt (db sql nil)
      (loop while (sqlite:step-statement stmt)
	    for i = 0 then (1+ i)
	    ;; with column-names = (sqlite:statement-column-names stmt)
	    with column-names = (map 'list
				     #'(lambda (x)
					 (from-sql-name x))
				     (sqlite:statement-column-names stmt))
	    for types = (loop for i below (length column-names)
			      collect (statement-column-type stmt i))

	    ;; Allocate the column memory & types
	    do (if (not data)
		   (setf data (loop
			       for name in column-names
			       for type in types
			       collect (cons (if (find-symbol name) (find-symbol name) (intern (string-upcase name)))
					     (alexandria:switch (type :test #'string=)
					       ("REAL"    (make-array nrows :element-type 'double-float))
					       ("INTEGER" (make-array nrows :element-type 'integer))
					       ("TEXT"    (make-array nrows)))))))

	    ;; Copy the data into the data-frame
	    do (loop for j below (length column-names)
		     do (setf (aref (cdr (nth j data)) i) (sqlite:statement-column-value stmt j)))))
    (sqlite:disconnect db)
    (data-frame:alist-df data)))


;;;
;;; Reading
;;;
(defun read-table (db table)
  "Read TABLE and return a data frame with the contents. Keys are interned in a package with the same name as TABLE."
  (let* ((nrows (sqlite:execute-single db (format nil "select count (*) from ~A" table)))
	 (ncols (sqlite:execute-single db (format nil "select count(*) from pragma_table_info('~A')" table)))
	 (names (execute-to-column db (format nil "select name from pragma_table_info('~A')" table)))
	 (types (execute-to-column db (format nil "select type from pragma_table_info('~A')" table)))
	 (*package* (cond
		      ((find-package (string-upcase table)) (find-package (string-upcase table)))
		      (t (make-package (string-upcase table)))))
	 (columns (loop
		    for name in names
		    for type in types
		    collect (cons (if (find-symbol name) (find-symbol name) (intern (string-upcase name)))
				  (alexandria:switch (type :test #'string=)
				    ("REAL"    (make-array nrows :element-type 'double-float))
				    ("INTEGER" (make-array nrows :element-type 'integer))
				    ("TEXT"    (make-array nrows)))))))

    (sqlite::with-prepared-statement stmt (db (format nil "select * from ~A" table) nil)
      (loop while (sqlite:step-statement stmt)
	    for i = 0 then (1+ i)
	    do (loop for j from 0 below ncols
		     do (setf (aref (cdr (nth j columns)) i) (sqlite:statement-column-value stmt j)))))
    (data-frame:alist-df columns)))


;;;
;;; Writing
;;;

(defun create-df-table (db table df)
  "Create a database table of NAME in DB according to the schema of DF.  This function is to create a table for DF prior to loading.  Lisp style symbol names are converted to SQL compatible names."
  (sqlite:execute-non-query db
			    (let (columns)
			      (map nil
				   #'(lambda (x)
				       (push (to-sql-name (symbol-name x)) columns)
				       (push (sqlite-column-type (column df x)) columns))
				   (keys df))
			      (format nil
				      "create table if not exists ~A (~{~A ~A~^, ~});"
				      table
				      (reverse columns)))))

(defun write-table (db table df)
  "Write data-frame DF to TABLE on connection DB. :na symbols are converted to \"NA\" strings in the database."
  (let (columns col-values)
    (map nil
	 #'(lambda (x)
	     (push (to-sql-name (symbol-name x)) columns))
	 (keys df))
    (map nil
	 #'(lambda (x)
	     (declare (ignore x))
	     (push "?" col-values))
	 (keys df))
    (sqlite:with-transaction db
      (loop
	for i below (aops:nrow df)
	with statement = (sqlite:prepare-statement db
						   (format nil
							   "insert into ~A (~{~A~^, ~}) values (~{~A~^, ~})"
							   table (reverse columns) col-values))
	do (loop
	     for j below (aops:ncol df)
	     do (sqlite:bind-parameter statement (1+ j) (if (eq (select df i j) :na)
							    "NA"
							    (select df i j)))
	     finally (sqlite:step-statement statement)
		     (sqlite:reset-statement statement))

      finally (sqlite:finalize-statement statement)))))
