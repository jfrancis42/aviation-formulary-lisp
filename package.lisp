;;;; package.lisp

(defpackage #:aviation-formulary
  (:nicknames :af)
  (:use #:cl)
  (:export :2d-point
	   :3d-point
	   :pp
	   :to-maidenhead
	   :from-maidenhead
	   :point-serial-number
	   :point-creation-time
	   :point-creation-source
	   :point-name
	   :point-description
	   :point-lat
	   :point-lon
	   :point-datum
	   :point-alt
	   :point-user
	   :point-generated
	   :rad-to-km
	   :rad-to-sm
	   :rad-to-nm
	   :km-to-rad
	   :deg-to-rad
	   :rad-to-deg
	   :ft-to-sm
	   :sm-to-ft
	   :deg-to-cardinal-course
	   :point-serialize
	   :point-deserialize
	   :calc-distance
	   :calc-distance-shorter
	   :calc-gc-bearing
	   :calc-new-point
	   :point-metadata-serialize
	   :serialize-points-to-file
	   :deserialize-points-from-file
	   :true-to-magnetic
	   :magnetic-to-true))
