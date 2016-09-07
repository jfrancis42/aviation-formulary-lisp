;;;; package.lisp

(defpackage #:aviation-formulary
  (:nicknames :af)
  (:use #:cl)
  (:export :2d-point
	   :3d-point
	   :spot-point
	   :gps-point
	   :pp
	   :message-type
	   :battery-state
	   :unix-time
	   :point-lat
	   :point-lon
	   :point-sats
	   :point-mode
	   :point-user
	   :point-generated
	   :point-gps
	   :point-spot
	   :creation-source
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
