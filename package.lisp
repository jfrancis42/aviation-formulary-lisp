;;;; package.lisp

(defpackage #:aviation-formulary
  (:use #:cl)
  (:export :pp
	   :point-user
	   :point-generated
	   :rad-to-km
	   :km-to-rad
	   :deg-to-rad
	   :rad-to-deg
	   :deg-to-cardinal-course
	   :url-photo
	   :url-map
	   :google-url-hybrid
	   :google-url-photo
	   :google-url-map
	   :point-serialize
	   :point-deserialize
	   :calc-distance
	   :calc-distance-shorter
	   :calc-gc-bearing
	   :calc-new-point
	   :serialize-points-to-file
	   :deserialize-points-from-file
	   :true-to-magnetic
	   :magnetic-to-true))
