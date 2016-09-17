;;;; package.lisp

(defpackage #:aviation-formulary
  (:nicknames :af)
  (:use #:cl)
  (:export :2d-point
	   :3d-point
	   :pp
	   :point-serial-number
	   :point-creation-time
	   :point-creation-source
	   :point-name
	   :point-description
	   :point-lat
	   :point-lon
	   :point-datum
	   :point-alt
	   :message-type
	   :unix-time
	   :point-user
	   :point-generated
	   :address
	   :rad-to-km
	   :km-to-rad
	   :deg-to-rad
	   :rad-to-deg
	   :deg-to-cardinal-course
	   :url-photo
	   :url-map
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
