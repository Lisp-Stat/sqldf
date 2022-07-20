;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte Ltd. All rights reserved.

(asdf:defsystem #:sqldf
  :version "1.0.0"
  :license  :MS-PL
  :author  "Steve Nunez <steve@symbolics.tech>"
  :long-name "SQL queries for Data Frames"
  :description "A wrapper for SQLite allowing querying of  Data Frames"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage "http://lisp-stat.dev/docs/manuals/sqldf"
  :source-control (:git "git://github.com/Lisp-Stat/sqldf")
  :bug-tracker "https://github.com/Lisp-Stat/sqldf/issues"

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

