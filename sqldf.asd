;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte Ltd. All rights reserved.

(asdf:defsystem #:sqldf
  :description "SQL for Data Frames"
  :long-description "SQLDF is a library for querying data frames using SQL, optimised for convenience over memory consumption. It uses an in-memory data base for transparent queries."
  :license  :MS-PL
  :version     (:read-file-form "version.sexp")
  :author  "Steve Nunez <steve@symbolics.tech>"
  :source-control (:git "git://github.com/Lisp-Stat/sqldf")
  :homepage "http://lisp-stat.dev/docs/reference/sqldf/"
  :serial t
  :depends-on (#:sqlite #:data-frame #:select)
  :in-order-to ((test-op (test-op "sqldf/tests")))
  :components ((:file #:pkgdcl)
	       (:file #:utils)
               (:file #:sqldf)))

#+nil
(asdf:defsystem "sqldf/tests"
  :license :MS-PL
  :depends-on (#:sqldf
               #:parachute)
  :serial t
  :pathname "tests/"
  :components ((:file "test-package")
	       (:file "main"))
  :perform (asdf:test-op (o s)
  		    (uiop:symbol-call :fiveam :run! ; this is '#:run! in Fare's Postmodern ASD file
  				      (uiop:find-symbol* :all-tests
							 :gpdf-t))))

