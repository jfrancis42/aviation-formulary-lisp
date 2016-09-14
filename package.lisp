;;;; package.lisp

(defpackage #:aviation-formulary
  (:nicknames :af)
  (:use #:cl)
  (:export :2d-point
	   :3d-point
	   :spot-point
	   :gps-point
	   :geocode-point
	   :pp
	   :message-type
	   :battery-state
	   :unix-time
	   :point-creation-time
	   :point-lat
	   :point-lon
	   :point-datum
	   :point-sats
	   :point-mode
	   :point-user
	   :point-generated
	   :point-gps
	   :point-spot
	   :point-geocode
	   :address
	   :creation-source
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
