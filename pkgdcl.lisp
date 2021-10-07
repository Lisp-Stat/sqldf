;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte Ltd. All rights reserved.

(uiop:define-package #:sqldf
    (:use :cl)
  (:import-from #:select
		#:select)
  (:import-from #:data-frame
		#:keys
		#:df
		#:column
		#:alist-df)
  (:export #:sqldf			;query a data frame
	   #:read-table	        	;read a SQLite table into a data frame
	   #:write-table)		;write a data frame into a SQLite table
  (:documentation "SQLDF is a facility for querying data frames with SQL"))

