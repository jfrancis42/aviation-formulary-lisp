;;;; aviation-formulary.lisp

(in-package #:aviation-formulary)

;; Unit conversions, etc.
;;
;;  1 knot = 1.852000 km/hr*
;;  1 knot = 185200/109728 ft/sec* = 1.687810 ft/sec
;;  1 knot = 1852000/1609344 mph* = 1.150779 mph
;;  1 mph  = 0.868976 knot
;;  1 mph  = 1.609344 km/hr*
;;  1 mph  = 1.466667 ft/sec
;;  1 km/hr= 0.539968 knot
;;  1 km/hr= 0.911344 ft/sec
;;  1 km/hr= 0.621371 mph
;;
;; * = exact conversion factor

;; serial number mutex
(defparameter *serial-lock* (bt:make-lock))

;; Different Earth radius values.
(defconstant earth-radius-nm-historic 6366.71)
(defconstant earth-radius-wgs84-eq-radius 6378.137)
(defconstant earth-radius-wgs84-polar-radius 6356.752)
(defconstant earth-radius-fai-sphere 6371.0)
(defvar *earth-radius* earth-radius-nm-historic)

;; WGS84 ellipsoid parameters for flat-earth approximation
(defconstant wgs84-a 6378.137d0)              ; equatorial radius in km
(defconstant wgs84-f (/ 1d0 298.257223563d0)) ; flattening
(defconstant wgs84-e2 (* wgs84-f (- 2d0 wgs84-f))) ; eccentricity squared

;; Sources for points.
(defconstant point-user 0)
(defconstant point-generated 1)

;; radians to km
(defmacro rad-to-km (r) `(* ,r *earth-radius*))

;; radians to sm
(defmacro rad-to-sm (r) `(* ,r *earth-radius* 0.621371))

;; radians to nm
(defmacro rad-to-nm (r) `(* ,r *earth-radius* 0.539957))

;; km to radians
(defmacro km-to-rad (km) `(/ ,km *earth-radius*))

;; degrees to radians
(defmacro deg-to-rad (d) `(/ (* ,d pi) 180))

;; radians to degrees
(defmacro rad-to-deg (r) `(/ (* ,r 180) pi))

;; feet to statute miles
(defmacro ft-to-sm (ft) `(/ ,ft 5280))

;; statute miles to feet
(defmacro sm-to-ft (mi) `(* ,mi 5280))

;; my own modulus
(defun my-mod (y x) (- y (* x (floor (/ y x)))))

(defun deg-to-cardinal-course (course &optional (short nil))
  "Given a compass heading in degrees, return a string with the
English cardinal name."
  (cond
    ((and (>= course 337.5) (<= course 360.0))
     (if short
	 (format nil "N")
	 (format nil "North")))
    ((and (>= course 0) (< course 22.5))
     (if short
	 (format nil "N")
	 (format nil "North")))
    ((and (>= course 22.5) (< course 67.5))
     (if short
	 (format nil "NE")
	 (format nil "Northeast")))
    ((and (>= course 67.5) (< course 112.5))
     (if short
	 (format nil "E")
	 (format nil "East")))
    ((and (>= course 112.5) (< course 157.5))
     (if short
	 (format nil "SE")
	 (format nil "Southeast")))
    ((and (>= course 157.5) (< course 202.5))
     (if short
	 (format nil "S")
	 (format nil "South")))
    ((and (>= course 202.5) (< course 247.5))
     (if short
	 (format nil "SW")
	 (format nil "Southwest")))
    ((and (>= course 247.5) (< course 292.5))
     (if short
	 (format nil "W")
	 (format nil "West")))
    ((and (>= course 292.5) (< course 337.5))
     (if short
	 (format nil "NW")
	 (format nil "Northwest")))
    (t
     (format nil "This can't happen"))))

;; Serial number for points.  Start at -1 so the first point is point
;; 0.
(defvar *point-serial-number* -1)

;; -=-=-=-=-=-=- OBJECTS and METHODS -=-=-=-=-=-=-

;; This class provides creation, source, user, and modification
;; information.  It is not intended to be instantiated, it's here for
;; the sole purpose of being inherited.
(defclass point-metadata ()
  ((serial-number :accessor point-serial-number
		  :initarg :serial-number
		  :initform (bt:with-lock-held (*serial-lock*)
			      (setf *point-serial-number*
				    (+ 1 *point-serial-number*))))
   (creation-time :accessor point-creation-time
		  :initarg :creation-time
		  :initform (local-time:timestamp-to-unix
			     (local-time:now)))
   (creation-source :accessor point-creation-source
		    :initarg :creation-source
		    :initform point-user)
   (name :accessor point-name
	 :initarg :name
	 :initform "")
   (description :accessor point-description
		:initarg :description
		:initform "")))

(defmethod point-metadata-serialize ((p point-metadata))
  "Serialize the point-metadata part of the object."
  (list
   (list 'serial-number (point-serial-number p))
   (list 'creation-time (point-creation-time p))
   (list 'creation-source (point-creation-source p))
   (list 'name (format nil "\"~A\"" (point-name p)))
   (list 'description (format nil "\"~A\""
			      (point-description p)))))

(defmethod point-metadata-deserialize-method ((p point-metadata) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (mapcar #'(lambda (n)
	      (cond
		((equal (first n) 'serial-number)
		 (setf (point-serial-number p) (second n)))
		((equal (first n) 'creation-time)
		 (setf (point-creation-time p) (second n)))
		((equal (first n) 'creation-source)
		 (setf (point-creation-source p) (second n)))
		((equal (first n) 'name)
		 (setf (point-name p) (second n)))
		((equal (first n) 'description)
		 (setf (point-description p) (second n)))
		))
	  point-data)
  )

;; This is the 'base class' of geographical points.  Defines the
;; minimum possible data, latitude, longitude, and datum.  It is not
;; intended to be instantiated, it's here for the sole purpose of
;; being inherited.
(defclass point ()
  ((lat :accessor point-lat
	:initarg :lat
	:initform nil)
   (lon :accessor point-lon
	:initarg :lon
	:initform nil)
   (datum :accessor point-datum
	  :initarg :datum
	  :initform "WGS84")))

;; The 2d point is a superclass of 'point' and 'point-metadata', and
;; becomes the base class of all the other types of points we come up
;; with.
(defclass 2d-point (point point-metadata)
  ())

(defun from-maidenhead (grid)
;; lat  =(CODE(MID(A1,2,1))-65)*10 + VALUE(MID(A1,4,1)) + (CODE(MID(A1,6,1))-97)/24 + 1/48 - 90
;; lon  =(CODE(MID(A1,1,1))-65)*20 + VALUE(MID(A1,3,1))*2 + (CODE(MID(A1,5,1))-97)/12 + 1/24 - 180
  (let* ((chars (coerce (if (= 4 (length grid)) (concatenate 'string grid "ll") grid) 'list))
	 (lat (+ (* 10 (- (char-int (coerce (second chars) 'character)) 65))
		 (parse-integer (string (fourth chars)))
		 (/ (- (char-int (coerce (string-downcase (sixth chars)) 'character)) 97) 24)
		 (/ 1 48)
		 -90))
	 (lon (+ (* 20 (- (char-int (coerce (first chars) 'character)) 65))
		 (* 2 (parse-integer (string (third chars))))
		 (/ (- (char-int (coerce (string-downcase (fifth chars)) 'character)) 97) 12)
		 (/ 1 24)
		 -180)))
    (make-instance '2d-point
		   :creation-source point-generated
		   :lat (* 1.0 lat)
		   :lon (* 1.0 lon))))

(defmethod to-maidenhead ((p 2d-point))
  "Derived from WA5ZNU's code at
http://wa5znu.org/log/2004/04/qra-maidenhead-grid-in-emacs-lisp.html"
  (let ((lat (point-lat p)) (lon (point-lon p)))
    (setf lon (+ lon 180.0))
    (setf lat (+ lat 90.0))
    (format nil
	    "~c~c~c~c~c~c"
	    (code-char (+ 65 (floor lon 20.0)))
	    (code-char (+ 65 (floor lat 10.0)))
	    (code-char (+ 48 (floor
			      (* 10 (- (/ lon 20.0) (floor lon 20.0))))))
	    (code-char (+ 48 (floor
			      (* 10 (- (/ lat 10.0) (floor lat 10.0))))))
	    (code-char (+ 32 65 (floor
				 (* 24 (- (/ lon 2.0) (floor lon 2.0))))))
	    (code-char (+ 32 65 (floor
				 (* 24 (- lat (floor lat)))))))))

(defmethod point-serialize ((p 2d-point))
  "Serialize a 2d point."
  (append
   (list
    '(type 2d-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p)))
   (point-metadata-serialize p)))

(defmethod pp ((p 2d-point))
  "Pretty print a 2d point."
  (format t "Name:  ~A~%" (point-name p))
  (format t "Descr:  ~A~%" (point-description p))
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p 2d-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (point-metadata-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
		((equal (first n) 'lat)
		 (setf (point-lat p) (second n)))
		((equal (first n) 'lon)
		 (setf (point-lon p) (second n)))
		((equal (first n) 'datum)
		 (setf (point-datum p) (second n)))
		))
	  point-data))

;;  Decendant of 2d-point.  Adds altitude.
(defclass 3d-point (2d-point)
  ((alt :accessor point-alt
	:initarg :alt
	:initform nil)))

(defmethod point-serialize ((p 3d-point))
  "Serialize a 3d point."
  (append
   (list
    '(type 3d-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'alt (point-alt p)))
   (point-metadata-serialize p)))

(defmethod pp ((p 3d-point))
  "Pretty print a 3d point."
  (format t "Name:  ~A~%" (point-name p))
  (format t "Descr:  ~A~%" (point-description p))
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Alt:  ~F~%" (point-alt p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p 3d-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (point-metadata-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
		((equal (first n) 'lat)
		 (setf (point-lat p) (second n)))
		((equal (first n) 'lon)
		 (setf (point-lon p) (second n)))
		((equal (first n) 'alt)
		 (setf (point-alt p) (second n)))
		((equal (first n) 'datum)
		 (setf (point-datum p) (second n)))
		))
	  point-data))

;; -=-=-=-=-=-=- FUNCTIONS -=-=-=-=-=-=-

(defun point-deserialize (point-data &optional point-type)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (let ((new-obj nil))
    (if (null point-type)
	(setf new-obj (make-instance
		       (second (assoc 'type point-data))))
	(setf new-obj (make-instance point-type)))
    (point-deserialize-method new-obj point-data)
    new-obj
    )
  )

;; Are the points the same?
(defmacro point-same-p (a b)
  `(if (and
	(equal (point-lat ,a)
	       (point-lat ,b))
	(equal (point-lon ,a)
	       (point-lon ,b)))
       t
       nil))

(defun calc-distance (point-a point-b)
  "Given two points, return the distance between them in radians."
  (if
   (point-same-p point-a point-b)
   0
   (let
       ((lat1 (deg-to-rad (point-lat point-a)))
	(lon1 (deg-to-rad (- 0 (point-lon point-a))))
	(lat2 (deg-to-rad (point-lat point-b)))
	(lon2 (deg-to-rad (- 0 (point-lon point-b)))))
     (acos
      (+
       (* (sin lat1) (sin lat2))
       (* (cos lat1) (cos lat2) (cos (- lon1 lon2))))))))

(defun calc-distance-shorter (point-a point-b)
  "Given two points, return the distance between them in radians. More
accurate than calc-distance."
  (if
   (point-same-p point-a point-b)
   0
   (let
       ((lat1 (deg-to-rad (point-lat point-a)))
	(lon1 (deg-to-rad (- 0 (point-lon point-a))))
	(lat2 (deg-to-rad (point-lat point-b)))
	(lon2 (deg-to-rad (- 0 (point-lon point-b)))))
     (* 2 (asin (sqrt (+ (expt (sin (/ (- lat1 lat2) 2)) 2)
			 (* (expt (sin (/ (- lon1 lon2) 2)) 2)
			    (cos lat2) (cos lat1)))))))))

(defun calc-gc-bearing (point-a point-b)
  "Calculate the initial great-circle bearing in radians from point-a to
point-b, measured clockwise from true North. Returns 0 for coincident
points. Handles all cases including poles and antipodal points."
  (if (point-same-p point-a point-b)
      0
      (let* ((lat1 (deg-to-rad (point-lat point-a)))
	     (lon1 (deg-to-rad (point-lon point-a)))
	     (lat2 (deg-to-rad (point-lat point-b)))
	     (lon2 (deg-to-rad (point-lon point-b)))
	     (dlon (- lon2 lon1)))
	(my-mod
	 (atan (* (sin dlon) (cos lat2))
	       (- (* (cos lat1) (sin lat2))
		  (* (sin lat1) (cos lat2) (cos dlon))))
	 (* 2 pi)))))

(defun calc-new-point (point-a d az)
  "Given a point, return a new point that is d radians away from the
original point at azimuth az radians."
  (let
      ((lat1 (deg-to-rad (point-lat point-a)))
       (lon1 (deg-to-rad (- 0 (point-lon point-a))))
       (lat nil) (lon nil) (dlon nil))
    (setf lat (asin (+ (* (sin lat1) (cos d)) (* (cos lat1) (sin d) (cos az)))))
    (setf dlon (atan (* (sin az) (sin d) (cos lat1)) (- (cos d) (* (sin lat1) (sin lat)))))
    (setf lon (- 0 (- (my-mod (+ (- lon1 dlon) pi) (* 2 pi)) pi)))
    (make-instance '2d-point
		   :creation-source point-generated
		   :lat (rad-to-deg lat)
		   :lon (rad-to-deg lon))))


(defun serialize-points-to-file (points filename)
  "Write a list of points to a file."
  (with-open-file
      (file-handle filename
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (mapcar
     #'(lambda (n)
	 (format file-handle "~A~%" (point-serialize n)))
     points)))

(defun deserialize-points-from-file (filename)
  "Read in a list of points from a file."
  (let ((tmp nil))
    (with-open-file (file-handle filename :direction :input)
      (do
       ((line (read file-handle nil 'eof)
	      (read file-handle nil 'eof)))
       ((eql line 'eof)
	(return tmp))
	(setf tmp (cons (point-deserialize line) tmp))))))


;; ============================================================
;; Great Circle Navigation (additional)
;; ============================================================

(defun calc-gc-intermediate-point (point-a point-b f)
  "Returns the intermediate point at fraction f along the great circle
from point-a (f=0) to point-b (f=1). Uses the Sinnott formula."
  (if (point-same-p point-a point-b)
      point-a
      (let* ((d (calc-distance-shorter point-a point-b))
	     (lat1 (deg-to-rad (point-lat point-a)))
	     (lon1 (deg-to-rad (point-lon point-a)))
	     (lat2 (deg-to-rad (point-lat point-b)))
	     (lon2 (deg-to-rad (point-lon point-b)))
	     (a (/ (sin (* (- 1d0 f) d)) (sin d)))
	     (b (/ (sin (* f d)) (sin d)))
	     (x (+ (* a (cos lat1) (cos lon1)) (* b (cos lat2) (cos lon2))))
	     (y (+ (* a (cos lat1) (sin lon1)) (* b (cos lat2) (sin lon2))))
	     (z (+ (* a (sin lat1))            (* b (sin lat2))))
	     (lat (atan z (sqrt (+ (* x x) (* y y)))))
	     (lon (atan y x)))
	(make-instance '2d-point
		       :creation-source point-generated
		       :lat (rad-to-deg lat)
		       :lon (rad-to-deg lon)))))

(defun calc-cross-track-error (point-a point-b point-d)
  "Returns the cross-track error in radians of point-d relative to the
great circle course from point-a to point-b. Positive = right of course,
negative = left of course."
  (let ((crs-ab (calc-gc-bearing point-a point-b))
	(crs-ad (calc-gc-bearing point-a point-d))
	(dist-ad (calc-distance-shorter point-a point-d)))
    (asin (* (sin dist-ad) (sin (- crs-ad crs-ab))))))

(defun calc-along-track-distance (point-a point-b point-d)
  "Returns the along-track distance in radians from point-a to the
closest point on the great circle from point-a to point-b to point-d.
Uses the numerically stable form for short distances."
  (let* ((xtd (calc-cross-track-error point-a point-b point-d))
	 (dist-ad (calc-distance-shorter point-a point-d))
	 (val (/ (sqrt (max 0d0 (- (expt (sin dist-ad) 2)
				   (expt (sin xtd) 2))))
		 (cos xtd))))
    (asin (max -1d0 (min 1d0 val)))))

(defun calc-radial-intersection (point-1 crs13-rad point-2 crs23-rad)
  "Returns the point at the intersection of the radial from point-1 on
initial course crs13-rad and the radial from point-2 on initial course
crs23-rad (both in radians, clockwise from North). Returns nil if no
unique intersection exists (parallel radials or ambiguous result)."
  (let* (;; Work in West-positive convention to match the formulary directly.
	 (lat1 (deg-to-rad (point-lat point-1)))
	 (lon1 (deg-to-rad (- (point-lon point-1))))
	 (lat2 (deg-to-rad (point-lat point-2)))
	 (lon2 (deg-to-rad (- (point-lon point-2))))
	 (dst12 (* 2d0 (asin (sqrt (+ (expt (sin (/ (- lat1 lat2) 2d0)) 2d0)
				      (* (cos lat1) (cos lat2)
					 (expt (sin (/ (- lon1 lon2) 2d0)) 2d0)))))))
	 (sin-lon-diff (sin (- lon2 lon1)))
	 (cos-crs12 (/ (- (sin lat2) (* (sin lat1) (cos dst12)))
		       (* (sin dst12) (cos lat1))))
	 (cos-crs21 (/ (- (sin lat1) (* (sin lat2) (cos dst12)))
		       (* (sin dst12) (cos lat2))))
	 (crs12 (if (< sin-lon-diff 0d0)
		    (acos (max -1d0 (min 1d0 cos-crs12)))
		    (- (* 2d0 pi) (acos (max -1d0 (min 1d0 cos-crs12))))))
	 (crs21 (if (< sin-lon-diff 0d0)
		    (- (* 2d0 pi) (acos (max -1d0 (min 1d0 cos-crs21))))
		    (acos (max -1d0 (min 1d0 cos-crs21)))))
	 (ang1 (- (my-mod (+ (- crs13-rad crs12) pi) (* 2d0 pi)) pi))
	 (ang2 (- (my-mod (+ (- crs21 crs23-rad) pi) (* 2d0 pi)) pi)))
    (cond
      ((and (< (abs (sin ang1)) 1d-10) (< (abs (sin ang2)) 1d-10))
       nil)
      ((< (* (sin ang1) (sin ang2)) 0d0)
       nil)
      (t
       (let* ((ang1a (abs ang1))
	      (ang2a (abs ang2))
	      (ang3 (acos (max -1d0 (min 1d0
			  (+ (* -1d0 (cos ang1a) (cos ang2a))
			     (* (sin ang1a) (sin ang2a) (cos dst12)))))))
	      (dst13 (atan (* (sin dst12) (sin ang1a) (sin ang2a))
			   (+ (cos ang2a) (* (cos ang1a) (cos ang3)))))
	      (lat3 (asin (max -1d0 (min 1d0
			  (+ (* (sin lat1) (cos dst13))
			     (* (cos lat1) (sin dst13) (cos crs13-rad)))))))
	      (dlon (atan (* (sin crs13-rad) (sin dst13) (cos lat1))
			  (- (cos dst13) (* (sin lat1) (sin lat3)))))
	      ;; lon3 computed in West-positive; negate to get East-positive
	      (lon3 (- (- (my-mod (+ (- lon1 dlon) pi) (* 2d0 pi)) pi))))
	 (make-instance '2d-point
			:creation-source point-generated
			:lat (rad-to-deg lat3)
			:lon (rad-to-deg lon3)))))))

(defun calc-gc-lat-at-lon (point-a point-b lon-deg)
  "Returns the latitude in degrees at which the great circle through
point-a and point-b crosses the given longitude. Returns nil if the
great circle is a meridian (no unique crossing latitude)."
  (let* (;; Work in West-positive convention to match the formulary.
	 (lat1 (deg-to-rad (point-lat point-a)))
	 (lat2 (deg-to-rad (point-lat point-b)))
	 (lon1 (deg-to-rad (- (point-lon point-a))))
	 (lon2 (deg-to-rad (- (point-lon point-b))))
	 (lon  (deg-to-rad (- lon-deg)))
	 (denom (* (cos lat1) (cos lat2) (sin (- lon1 lon2)))))
    (if (< (abs denom) 1d-10)
	nil
	(rad-to-deg
	 (atan (/ (- (* (sin lat1) (cos lat2) (sin (- lon lon2)))
		     (* (sin lat2) (cos lat1) (sin (- lon lon1))))
		  denom))))))

(defun calc-gc-max-lat (point bearing-rad)
  "Returns the maximum latitude in degrees reached on the great circle
through the given point at the given initial bearing (radians), using
Clairaut's formula."
  (let ((lat (deg-to-rad (point-lat point))))
    (rad-to-deg (acos (abs (* (sin bearing-rad) (cos lat)))))))

(defun calc-gc-parallel-crossings (point-a point-b lat3-deg)
  "Returns a list of two East-positive longitudes (degrees) where the
great circle through point-a and point-b crosses the parallel at
lat3-deg degrees. Returns nil if no crossing exists."
  (let* (;; Work in West-positive convention to match the formulary.
	 (lat1 (deg-to-rad (point-lat point-a)))
	 (lat2 (deg-to-rad (point-lat point-b)))
	 (lat3 (deg-to-rad lat3-deg))
	 (lon1 (deg-to-rad (- (point-lon point-a))))
	 (lon2 (deg-to-rad (- (point-lon point-b))))
	 (l12 (- lon1 lon2))
	 (aa (* (sin lat1) (cos lat2) (cos lat3) (sin l12)))
	 (bb (- (* (sin lat1) (cos lat2) (cos lat3) (cos l12))
		(* (cos lat1) (sin lat2) (cos lat3))))
	 (cc (* (cos lat1) (cos lat2) (sin lat3) (sin l12)))
	 (sq (sqrt (+ (* aa aa) (* bb bb)))))
    (if (> (* cc cc) (* sq sq))
	nil
	(let* ((lon    (atan bb aa))  ; atan2(B, A) per formulary
	       (dlon   (acos (/ cc sq)))
	       ;; lon3 values in West-positive; negate to get East-positive
	       (lon3-1 (- (- (my-mod (+ lon1 dlon lon pi) (* 2d0 pi)) pi)))
	       (lon3-2 (- (- (my-mod (+ lon1 (- dlon) lon pi) (* 2d0 pi)) pi))))
	  (list (rad-to-deg lon3-1)
		(rad-to-deg lon3-2))))))


;; ============================================================
;; Rhumb Line Navigation
;; ============================================================

(defun calc-rhumb-bearing (point-a point-b)
  "Returns the rhumb line (constant bearing) course in radians from
point-a to point-b, measured clockwise from true North. Selects the
shorter path when the route would cross the 180th meridian."
  (let* ((lat1 (deg-to-rad (point-lat point-a)))
	 (lat2 (deg-to-rad (point-lat point-b)))
	 (lon1 (deg-to-rad (point-lon point-a)))
	 (lon2 (deg-to-rad (point-lon point-b)))
	 (dphi (log (/ (tan (+ (/ lat2 2d0) (/ pi 4d0)))
		       (tan (+ (/ lat1 2d0) (/ pi 4d0))))))
	 ;; Choose the shorter of the easterly and westerly longitude spans
	 (dlon-e (my-mod (- lon2 lon1) (* 2d0 pi)))
	 (dlon-w (my-mod (- lon1 lon2) (* 2d0 pi))))
    (if (< dlon-w dlon-e)
	(my-mod (atan (- dlon-w) dphi) (* 2d0 pi))
	(my-mod (atan dlon-e dphi) (* 2d0 pi)))))

(defun calc-rhumb-distance (point-a point-b)
  "Returns the rhumb line distance in radians between point-a and
point-b. Uses the shorter path across the date line if applicable."
  (let* ((lat1 (deg-to-rad (point-lat point-a)))
	 (lat2 (deg-to-rad (point-lat point-b)))
	 (lon1 (deg-to-rad (point-lon point-a)))
	 (lon2 (deg-to-rad (point-lon point-b)))
	 (dphi (log (/ (tan (+ (/ lat2 2d0) (/ pi 4d0)))
		       (tan (+ (/ lat1 2d0) (/ pi 4d0))))))
	 (q (if (< (abs (- lat2 lat1)) 1d-10)
		(cos lat1)
		(/ (- lat2 lat1) dphi)))
	 (dlon-e (my-mod (- lon2 lon1) (* 2d0 pi)))
	 (dlon-w (my-mod (- lon1 lon2) (* 2d0 pi)))
	 (dlon (min dlon-e dlon-w)))
    (sqrt (+ (expt (- lat2 lat1) 2d0) (* q q dlon dlon)))))

(defun calc-rhumb-new-point (point-a d tc)
  "Given a starting point, a distance d in radians, and a true course tc
in radians, returns a new 2d-point on the rhumb line. Signals an error
if the distance is too large for the given course."
  (let* ((lat1 (deg-to-rad (point-lat point-a)))
	 (lon1 (deg-to-rad (point-lon point-a)))
	 (lat  (+ lat1 (* d (cos tc)))))
    (when (> (abs lat) (/ pi 2d0))
      (error "Distance too large: rhumb line would cross a pole"))
    (let* ((dphi (log (/ (tan (+ (/ lat 2d0) (/ pi 4d0)))
			 (tan (+ (/ lat1 2d0) (/ pi 4d0))))))
	   (q    (if (< (abs (- lat lat1)) 1d-10)
		     (cos lat1)
		     (/ (- lat lat1) dphi)))
	   (dlon (/ (* d (sin tc)) q))
	   (lon  (- (my-mod (+ lon1 dlon pi) (* 2d0 pi)) pi)))
      (make-instance '2d-point
		     :creation-source point-generated
		     :lat (rad-to-deg lat)
		     :lon (rad-to-deg lon)))))


;; ============================================================
;; Local Flat Earth Approximation (WGS84)
;; ============================================================

(defun flat-earth-north-radius (lat-deg)
  "Returns the meridional radius of curvature R1 in km at the given
latitude (degrees) using the WGS84 ellipsoid. Used for local flat-earth
distance-north calculations."
  (let* ((s (sin (deg-to-rad lat-deg)))
	 (d (expt (- 1d0 (* wgs84-e2 s s)) 1.5d0)))
    (/ (* wgs84-a (- 1d0 wgs84-e2)) d)))

(defun flat-earth-east-radius (lat-deg)
  "Returns the transverse radius of curvature R2 in km at the given
latitude (degrees) using the WGS84 ellipsoid. Used for local flat-earth
distance-east calculations."
  (let* ((s (sin (deg-to-rad lat-deg)))
	 (d (sqrt (- 1d0 (* wgs84-e2 s s)))))
    (/ wgs84-a d)))

(defun calc-flat-earth-offset (origin point)
  "Returns (values north-km east-km), the approximate displacement from
origin to point using the WGS84 local flat-earth approximation. Accurate
for short distances (< ~500 km from the origin)."
  (let* ((lat0 (deg-to-rad (point-lat origin)))
	 (dlat (deg-to-rad (- (point-lat point) (point-lat origin))))
	 (dlon (deg-to-rad (- (point-lon point) (point-lon origin))))
	 (r1 (flat-earth-north-radius (point-lat origin)))
	 (r2 (flat-earth-east-radius  (point-lat origin))))
    (values (* r1 dlat)
	    (* r2 (cos lat0) dlon))))

(defun calc-flat-earth-distance (origin point)
  "Returns the approximate distance in km from origin to point using the
WGS84 local flat-earth approximation."
  (multiple-value-bind (north east)
      (calc-flat-earth-offset origin point)
    (sqrt (+ (* north north) (* east east)))))

(defun calc-flat-earth-bearing (origin point)
  "Returns the approximate bearing in radians from origin to point using
the WGS84 local flat-earth approximation."
  (multiple-value-bind (north east)
      (calc-flat-earth-offset origin point)
    (my-mod (atan east north) (* 2d0 pi))))


;; ============================================================
;; Wind Triangle
;; ============================================================

(defun calc-unknown-wind (tas gs heading-rad course-rad)
  "Given true airspeed (knots), groundspeed (knots), magnetic heading
(radians), and actual ground course (radians), returns
  (values wind-direction-rad wind-speed-knots).
Wind direction is the direction the wind is blowing FROM."
  (let* ((ws (sqrt (+ (expt (- tas gs) 2d0)
		      (* 4d0 tas gs
			 (expt (sin (/ (- heading-rad course-rad) 2d0)) 2d0)))))
	 (wd (my-mod (+ course-rad
			(atan (* tas (sin (- heading-rad course-rad)))
			      (- (* tas (cos (- heading-rad course-rad))) gs)))
		     (* 2d0 pi))))
    (values wd ws)))

(defun calc-heading-groundspeed (course-rad tas ws wd)
  "Given desired course (radians), true airspeed (knots), wind speed
(knots), and wind direction (radians, blowing FROM), returns
  (values heading-rad groundspeed-knots)
or (values nil nil) if the course cannot be flown (wind too strong)."
  (let ((swc (* (/ ws tas) (sin (- wd course-rad)))))
    (if (> (abs swc) 1d0)
	(values nil nil)
	(let* ((hd (my-mod (+ course-rad (asin swc)) (* 2d0 pi)))
	       (gs (- (* tas (sqrt (- 1d0 (* swc swc))))
		      (* ws (cos (- wd course-rad))))))
	  (if (< gs 0d0)
	      (values nil nil)
	      (values hd gs))))))

(defun calc-course-groundspeed (heading-rad tas ws wd)
  "Given heading (radians), true airspeed (knots), wind speed (knots),
and wind direction (radians, blowing FROM), returns
  (values course-rad groundspeed-knots)."
  (let* ((gs  (sqrt (- (+ (* ws ws) (* tas tas))
		       (* 2d0 ws tas (cos (- heading-rad wd))))))
	 (wca (atan (* ws (sin (- heading-rad wd)))
		    (- tas (* ws (cos (- heading-rad wd))))))
	 (crs (my-mod (+ heading-rad wca) (* 2d0 pi))))
    (values crs gs)))

(defun calc-wind-components (ws wd runway-direction-rad)
  "Given wind speed (knots), wind direction (radians, blowing FROM), and
runway/heading direction (radians), returns
  (values headwind-knots crosswind-knots).
Positive headwind = into the wind. Positive crosswind = wind from right."
  (values (* ws (cos (- wd runway-direction-rad)))
	  (* ws (sin (- wd runway-direction-rad)))))

(defun calc-tas-from-three-groundspeeds (v1 v2 v3)
  "Given three groundspeeds (knots) measured on headings 120 degrees
apart, returns (values tas-knots wind-speed-knots)."
  (let* ((vms (/ (+ (* v1 v1) (* v2 v2) (* v3 v3)) 3d0))
	 (a1 (- (/ (* v1 v1) vms) 1d0))
	 (a2 (- (/ (* v2 v2) vms) 1d0))
	 (a3 (- (/ (* v3 v3) vms) 1d0))
	 (mu (/ (+ (* a1 a1) (* a2 a2) (* a3 a3)) 6d0))
	 (bp (+ 0.5d0 (sqrt (max 0d0 (- 0.25d0 mu)))))
	 (bm (/ mu bp))
	 (r1 (sqrt (* vms bp)))
	 (r2 (sqrt (* vms bm))))
    ;; TAS must exceed wind speed; exchange roots if necessary
    (if (< r1 r2)
	(values r2 r1)
	(values r1 r2))))


;; ============================================================
;; Magnetic Variation
;; ============================================================

;; All three polynomial fits use x = latitude (degrees N) and
;; y = longitude (degrees W, i.e. the negative of the point's stored lon).
;; The returned value is variation in degrees where:
;;   positive = West variation (compass reads lower than true course)
;;   negative = East variation (compass reads higher than true course)
;;
;; To convert headings:
;;   magnetic = true - variation
;;   true     = magnetic + variation

(defun true-to-magnetic (point)
  "Returns the magnetic variation in degrees at the given point using
the Continental US polynomial fit. Positive values indicate West
variation (compass reads lower than true); negative values indicate East
variation (compass reads higher than true). Valid for Continental US
(24 < lat < 50, 66 < lon_W < 125). RMS error ~1 degree.

Note: this function returns variation, not a converted heading. Use
true-heading-to-magnetic and magnetic-heading-to-true for heading
conversions."
  (let ((x (point-lat point))
	(y (- (point-lon point))))   ; West-positive
    (+
     (- 0.0 65.6811)
     (* 0.99 x)
     (* 0.0128899 (expt x 2))
     (- 0.0 (* 0.0000905928 (expt x 3)))
     (* 2.87622 y)
     (- 0.0 (* 0.0116268 x y))
     (- 0.0 (* 0.00000603925 (expt x 2) y))
     (- 0.0 (* 0.0389806 (expt y 2)))
     (- 0.0 (* 0.0000403488 x (expt y 2)))
     (* 0.000168556 (expt y 3)))))

(defun magnetic-to-true (point)
  "Returns the negation of the magnetic variation at the given point
(equivalent to adding East variation or subtracting West variation).
Only valid for Continental US. See true-to-magnetic for sign convention.

Note: for heading conversion use magnetic-heading-to-true instead."
  (- 0.0 (true-to-magnetic point)))

;; Alias with a clearer name.
(defun magnetic-variation-conus (point)
  "Returns the magnetic variation in degrees at the given point using
the Continental US polynomial fit. Positive = West variation, negative =
East variation. Valid for Continental US (24 < lat < 50,
66 < lon_W < 125). RMS error ~1 degree. Alias for true-to-magnetic."
  (true-to-magnetic point))

(defun magnetic-variation-alaska (point)
  "Returns the magnetic variation in degrees at the given point using
the Alaska polynomial fit. Positive = West variation, negative = East
variation. Valid for Alaska (lat > 54, 130 < lon_W < 172).
Better than 1 degree error for all covered points."
  (let ((x (point-lat point))
	(y (- (point-lon point))))   ; West-positive
    (+ 618.854d0
       (* 2.76049d0    x)
       (* -0.556206d0  (expt x 2))
       (* 0.00251582d0 (expt x 3))
       (* -12.7974d0   y)
       (* 0.408161d0   x y)
       (* 0.000434097d0 (expt x 2) y)
       (* -0.00602173d0 (expt y 2))
       (* -0.00144712d0 x (expt y 2))
       (* 0.000222521d0 (expt y 3)))))

(defun magnetic-variation-europe (point)
  "Returns the magnetic variation in degrees at the given point using
the Western Europe polynomial fit. Positive = West variation, negative =
East variation. Uses East-positive longitude (standard convention).
Valid for Western Europe (-10 < lon < 28, 36 < lat < 68)."
  (let ((lat (point-lat point))
	(lon (point-lon point)))   ; East-positive (as given in this formula)
    (+ 10.4768771667158d0
       (* -0.507385322418858d0    lon)
       (* 0.00753170031703826d0   (expt lon 2))
       (* -1.40596203924748d-5    (expt lon 3))
       (* -0.535560699962353d0    lat)
       (* 0.0154348808069955d0    lat lon)
       (* -8.07756425110592d-5    lat (expt lon 2))
       (* 0.00976887198864442d0   (expt lat 2))
       (* -0.000259163929798334d0 (expt lat 2) lon)
       (* -3.69056939266123d-5    (expt lat 3)))))

(defun true-heading-to-magnetic (true-heading-deg point)
  "Converts a true heading in degrees to a magnetic heading in degrees
using the CONUS magnetic variation polynomial. Valid for Continental US."
  (- true-heading-deg (true-to-magnetic point)))

(defun magnetic-heading-to-true (magnetic-heading-deg point)
  "Converts a magnetic heading in degrees to a true heading in degrees
using the CONUS magnetic variation polynomial. Valid for Continental US."
  (+ magnetic-heading-deg (true-to-magnetic point)))


;; ============================================================
;; Standard Atmosphere and Altimetry
;; ============================================================

(defun standard-temperature (altitude-ft)
  "Returns the standard atmospheric temperature in Celsius at the given
altitude in feet. Uses the ICAO standard atmosphere lapse rate below the
tropopause (36,089 ft) and isothermal above it."
  (if (< altitude-ft 36089.24d0)
      (- 15d0 (* 0.0019812d0 altitude-ft))
      -56.5d0))

(defun standard-pressure-inhg (altitude-ft)
  "Returns the standard atmospheric pressure in inches Hg at the given
altitude in feet."
  (if (< altitude-ft 36089.24d0)
      (* 29.92126d0
	 (expt (- 1d0 (* 6.8755856d-6 altitude-ft)) 5.2558797d0))
      (* 0.2233609d0 29.92126d0
	 (exp (* -4.806346d-5 (- altitude-ft 36089.24d0))))))

(defun pressure-altitude (indicated-altitude-ft altimeter-setting-inhg)
  "Returns pressure altitude in feet given indicated altitude in feet
and altimeter setting in inches Hg."
  (+ indicated-altitude-ft
     (* 145442.2d0
	(- 1d0 (expt (/ altimeter-setting-inhg 29.92126d0) 0.190261d0)))))

(defun density-altitude (pressure-altitude-ft temperature-c)
  "Returns density altitude in feet given pressure altitude in feet and
actual outside air temperature in Celsius."
  (let* ((ts-c (standard-temperature pressure-altitude-ft))
	 (ts-k (+ ts-c 273.15d0))
	 (t-k  (+ temperature-c 273.15d0))
	 (tr   0.0019812d0))
    (+ pressure-altitude-ft
       (* (/ ts-k tr)
	  (- 1d0 (expt (/ ts-k t-k) 0.2349690d0))))))

(defun true-altitude (calibrated-altitude-ft field-elevation-ft
		      isa-deviation-c oat-c)
  "Returns true altitude in feet. calibrated-altitude-ft is the
altimeter reading, field-elevation-ft is the elevation of the reporting
station, isa-deviation-c is the average temperature deviation from ISA
in the air column in Celsius, oat-c is the outside air temperature at
altitude in Celsius."
  (+ calibrated-altitude-ft
     (* (- calibrated-altitude-ft field-elevation-ft)
	(/ isa-deviation-c (+ 273d0 oat-c)))))


;; ============================================================
;; Airspeed Conversions
;; ============================================================

(defun speed-of-sound (temperature-c)
  "Returns the speed of sound in knots at the given outside air
temperature in Celsius."
  (* 38.967854d0 (sqrt (+ temperature-c 273.15d0))))

(defun mach-number (tas-knots temperature-c)
  "Returns the Mach number given true airspeed in knots and outside air
temperature in Celsius."
  (/ tas-knots (speed-of-sound temperature-c)))

(defun tas-from-mach (mach temperature-c)
  "Returns true airspeed in knots given Mach number and outside air
temperature in Celsius."
  (* mach (speed-of-sound temperature-c)))

(defun tas-from-cas (cas-knots density-altitude-ft)
  "Returns true airspeed in knots given calibrated airspeed in knots and
density altitude in feet. Valid below the tropopause (36,089 ft)."
  (* cas-knots
     (expt (- 1d0 (* 6.8755856d-6 density-altitude-ft)) -2.127940d0)))

(defun cas-from-tas (tas-knots density-altitude-ft)
  "Returns calibrated airspeed in knots given true airspeed in knots and
density altitude in feet. Valid below the tropopause (36,089 ft)."
  (* tas-knots
     (expt (- 1d0 (* 6.8755856d-6 density-altitude-ft)) 2.127940d0)))


;; ============================================================
;; Humidity and Meteorology
;; ============================================================

(defun relative-humidity-from-dewpoint (temperature-c dewpoint-c)
  "Returns relative humidity as a fraction (0 to 1) given dry-bulb
temperature and dewpoint in Celsius. Uses the Magnus (Tetens) formula."
  (exp (* 17.27d0 (- (/ dewpoint-c (+ dewpoint-c 237.3d0))
		     (/ temperature-c (+ temperature-c 237.3d0))))))

(defun dewpoint-from-humidity (temperature-c relative-humidity)
  "Returns dewpoint in Celsius given temperature in Celsius and relative
humidity as a fraction (0 to 1). Uses the Magnus formula."
  (let* ((lf (log relative-humidity))
	 (v  (+ (/ lf 17.27d0)
		(/ temperature-c (+ temperature-c 237.3d0)))))
    (/ (* 237.3d0 v) (- 1d0 v))))

(defun frostpoint-from-humidity (temperature-c relative-humidity)
  "Returns frostpoint in Celsius given temperature in Celsius and relative
humidity as a fraction (0 to 1). Uses the Magnus formula over ice."
  (let* ((lf (log relative-humidity))
	 (v  (+ (/ lf 21.87d0)
		(/ temperature-c (+ temperature-c 265.5d0)))))
    (/ (* 265.5d0 v) (- 1d0 v))))

(defun density-altitude-humidity-correction (relative-humidity temperature-c
					     pressure-altitude-ft)
  "Returns the increase in density altitude in feet due to humidity.
relative-humidity is a fraction (0 to 1), temperature in Celsius,
pressure-altitude-ft in feet."
  (* 0.267d0 relative-humidity
     (+ temperature-c 273d0)
     (exp (* 17.3d0 (/ temperature-c (+ temperature-c 237d0))))
     (expt (- 1d0 (* 6.88d-6 pressure-altitude-ft)) -5.26d0)))


;; ============================================================
;; Bellamy's Formula for Wind Drift
;; ============================================================

(defun bellamy-drift (pressure-difference-inhg latitude-rad tas-knots)
  "Returns wind drift distance in nautical miles using Bellamy's formula.
pressure-difference-inhg is destination minus departure pressure in
inches Hg. latitude-rad is the average route latitude in radians.
Positive result = drift to the left when destination pressure is higher."
  (/ (* 21500d0 pressure-difference-inhg)
     (* (sin latitude-rad) tas-knots)))

(defun bellamy-wind-correction-angle (pressure-difference-inhg latitude-rad
				      tas-knots distance-nm)
  "Returns the wind correction angle in degrees using Bellamy's formula.
pressure-difference-inhg is destination minus departure pressure in
inches Hg. latitude-rad is the average route latitude in radians.
Positive result requires a right correction when destination pressure
is higher."
  (/ (* 1230000d0 pressure-difference-inhg)
     (* (sin latitude-rad) tas-knots distance-nm)))


;; ============================================================
;; Turn Performance
;; ============================================================

(defun turn-radius (airspeed-knots bank-angle-deg)
  "Returns the turn radius in feet given true airspeed in knots and bank
angle in degrees."
  (/ (* airspeed-knots airspeed-knots)
     (* 11.23d0 (tan (deg-to-rad bank-angle-deg)))))

(defun turn-rate (airspeed-knots radius-ft)
  "Returns the rate of turn in degrees per second given true airspeed in
knots and turn radius in feet."
  (/ (* 96.7d0 airspeed-knots) radius-ft))

(defun standard-rate-bank-angle (airspeed-knots)
  "Returns the bank angle in degrees required for a standard rate turn
(3 degrees/second) at the given true airspeed in knots."
  (* 57.3d0 (atan (/ airspeed-knots 362.1d0))))

(defun pivotal-altitude (groundspeed-knots)
  "Returns the pivotal altitude in feet for the given groundspeed in
knots. At the pivotal altitude an aircraft in a level turn appears to
pivot on a fixed point on the ground."
  (/ (* groundspeed-knots groundspeed-knots) 11.23d0))


;; ============================================================
;; Distance to Horizon
;; ============================================================

(defun distance-to-horizon (altitude-ft)
  "Returns the distance to the visual horizon in nautical miles from an
observer at the given altitude in feet above the ground. Accounts for
standard atmospheric refraction."
  (* 1.17d0 (sqrt altitude-ft)))
