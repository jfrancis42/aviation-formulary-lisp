;;;; package.lisp

(defpackage #:aviation-formulary
  (:nicknames :af)
  (:use #:cl)
  (:export
   ;; Point classes and accessors
   :2d-point
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
   :point-serialize
   :point-deserialize
   :point-metadata-serialize
   :serialize-points-to-file
   :deserialize-points-from-file

   ;; Unit conversions
   :rad-to-km
   :rad-to-sm
   :rad-to-nm
   :km-to-rad
   :deg-to-rad
   :rad-to-deg
   :ft-to-sm
   :sm-to-ft
   :deg-to-cardinal-course

   ;; Earth radius constants
   :earth-radius-nm-historic
   :earth-radius-wgs84-eq-radius
   :earth-radius-wgs84-polar-radius
   :earth-radius-fai-sphere
   :*earth-radius*

   ;; WGS84 ellipsoid constants
   :wgs84-a
   :wgs84-f
   :wgs84-e2

   ;; Great circle navigation
   :calc-distance
   :calc-distance-shorter
   :calc-gc-bearing
   :calc-new-point
   :calc-gc-intermediate-point
   :calc-cross-track-error
   :calc-along-track-distance
   :calc-radial-intersection
   :calc-gc-lat-at-lon
   :calc-gc-max-lat
   :calc-gc-parallel-crossings

   ;; Rhumb line navigation
   :calc-rhumb-bearing
   :calc-rhumb-distance
   :calc-rhumb-new-point

   ;; Local flat earth approximation
   :flat-earth-north-radius
   :flat-earth-east-radius
   :calc-flat-earth-offset
   :calc-flat-earth-distance
   :calc-flat-earth-bearing

   ;; Wind triangle
   :calc-unknown-wind
   :calc-heading-groundspeed
   :calc-course-groundspeed
   :calc-wind-components
   :calc-tas-from-three-groundspeeds

   ;; Magnetic variation
   :true-to-magnetic
   :magnetic-to-true
   :magnetic-variation-conus
   :magnetic-variation-alaska
   :magnetic-variation-europe
   :true-heading-to-magnetic
   :magnetic-heading-to-true

   ;; Standard atmosphere and altimetry
   :standard-temperature
   :standard-pressure-inhg
   :pressure-altitude
   :density-altitude
   :true-altitude

   ;; Airspeed conversions
   :speed-of-sound
   :mach-number
   :tas-from-mach
   :tas-from-cas
   :cas-from-tas

   ;; Humidity and meteorology
   :relative-humidity-from-dewpoint
   :dewpoint-from-humidity
   :frostpoint-from-humidity
   :density-altitude-humidity-correction

   ;; Bellamy's formula
   :bellamy-drift
   :bellamy-wind-correction-angle

   ;; Turn performance
   :turn-radius
   :turn-rate
   :standard-rate-bank-angle
   :pivotal-altitude

   ;; Distance to horizon
   :distance-to-horizon))
