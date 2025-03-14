;;;; aviation-formulary-lisp.lisp

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

;; radians per degree
(defconstant radians-per-degree (/ pi 180))

;; Different Earth radius values.
(defconstant earth-radius-nm-historic 6366.71)
(defconstant earth-radius-wgs84-eq-radius 6378.137)
(defconstant earth-radius-wgs84-polar-radius 6356.752)
(defconstant earth-radius-fai-sphere 6371.0)
(defvar *earth-radius* earth-radius-nm-historic)

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
   (equal point-a point-b)
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
   (equal point-a point-b)
   0
   (let
       ((lat1 (deg-to-rad (point-lat point-a)))
	(lon1 (deg-to-rad (- 0 (point-lon point-a))))
	(lat2 (deg-to-rad (point-lat point-b)))
	(lon2 (deg-to-rad (- 0 (point-lon point-b)))))
     (* 2 (asin (sqrt (+ (expt (sin (/ (- lat1 lat2) 2)) 2)
			 (* (expt (sin (/ (- lon1 lon2) 2)) 2)
			    (cos lat2) (cos lat1)))))))))

(defun calc-gc-bearing (point-a point-b) ;; TODO: validate logic (specifcally nsew) before commit.
  "Calculate the bearing in radians between point-a and point-b. Fails
if either point is a pole."
  (cond
    ((point-same-p point-a point-b)
     0)
    ((and (= (point-lat point-a) (point-lat point-b))
	  (> (point-lon point-a) (point-lon point-b)))
     ;; west 
     (* 1.5 pi))
    ((and (= (point-lat point-a) (point-lat point-b))
	  (< (point-lon point-a) (point-lon point-b)))
     ;; east 
     (* 0.5 pi))
    ((and (= (point-lon point-a) (point-lon point-b))
	  (> (point-lat point-a) (point-lat point-b)))
     ;; north
     0.0)
    ((and (= (point-lon point-a) (point-lon point-b))
	  (< (point-lat point-a) (point-lat point-b)))
     ;; south
     pi)
    (t (let
	   ((lat1 (deg-to-rad (point-lat point-a)))
	    (lon1 (deg-to-rad (- 0 (point-lon point-a))))
	    (lat2 (deg-to-rad (point-lat point-b)))
	    (lon2 (deg-to-rad (- 0 (point-lon point-b))))
	    (d (calc-distance point-a point-b)))
	 (if (< (sin (- lon2 lon1)) 0)
	     (acos (/ (- (sin lat2)
			 (* (sin lat1) (cos d))) (* (sin d) (cos lat1))))
	     (- (* 2 pi) (acos (/ (- (sin lat2)
				     (* (sin lat1) (cos d)))
				  (* (sin d) (cos lat1))))))))))

(defun calc-gc-bearing-orig (point-a point-b)
  "Calculate the bearing in radians between point-a and point-b. Fails
if either point is a pole."
  (if
   (point-same-p point-a point-b)
   0
   (let
       ((lat1 (deg-to-rad (point-lat point-a)))
	(lon1 (deg-to-rad (- 0 (point-lon point-a))))
	(lat2 (deg-to-rad (point-lat point-b)))
	(lon2 (deg-to-rad (- 0 (point-lon point-b))))
	(d (calc-distance point-a point-b)))
     (if (< (sin (- lon2 lon1)) 0)
	 (acos (/ (- (sin lat2)
		     (* (sin lat1) (cos d))) (* (sin d) (cos lat1))))
	 (- (* 2 pi) (acos (/ (- (sin lat2)
				 (* (sin lat1) (cos d)))
			      (* (sin d) (cos lat1)))))))))

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

; (defun calc-intersecting-radials (point-a crs-a point-b crs-b)
;   "Given two points and two courses, calculate the intersection
; point."
; ; # xxx
; ; # 
; ; # Now how to compute the latitude, latc, and longitude, lonc of an
; ; # intersection formed by the crsac true bearing from point 1 and the
; ; # crsbc true bearing from point 2:
; ; #
; ; # dstab=2*asin(sqrt((sin((lata-latb)/2))^2+
; ; #                    cos(lata)*cos(latb)*sin((lona-lonb)/2)^2))
;   (let* ((dstab (calc-distance-shorter (point-a point-b)))
; ; # IF sin(lonb-lona)<0
; ; #    crsab=acos((sin(latb)-sin(lata)*cos(dstab))/(sin(dstab)*cos(lata)))
; ; #    crsba=2.*pi-acos((sin(lata)-sin(latb)*cos(dstab))/(sin(dstab)*cos(latb)))
; ; # ELSE
; ; #    crsab=2.*pi-acos((sin(latb)-sin(lata)*cos(dstab))/(sin(dstab)*cos(lata)))
; ; #    crsba=acos((sin(lata)-sin(latb)*cos(dstab))/(sin(dstab)*cos(latb)))
; ; # ENDIF
; ; 	 (crsab (calc-gc-bearing point-a point-b))
; ; 	 (crsba (calc-gc-bearing point-b point-a))
; ; # ang1=mod(crsac-crsab+pi,2.*pi)-pi
; ; # ang2=mod(crsba-crsbc+pi,2.*pi)-pi
; ; 	 (ang1 (- (my-mod (+ pi (- crs-a crsab)) (* 2 pi)) pi))
; ; 	 (ang2 (- (my-mod (+ pi (- crsba crs-b )) (* 2 pi)) pi))
; ; 	 (ang3 nil)))
; ; # IF (sin(ang1)=0 AND sin(ang2)=0)
; ; #    "infinity of intersections"
; ; # ELSEIF sin(ang1)*sin(ang2)<0
; ; #    "intersection ambiguous"
; 	 (when (or (and (= 0.0 (sin ang1))
;  			(= 0.0 (sin ang2)))
;  		   (< (* (sin ang1) (sin ang2)) 0))
; 	   (return nil))
; ; # ELSE
; ; #    ang1=abs(ang1)
; ; #    ang2=abs(ang2)
; ; #    ang3=acos(-cos(ang1)*cos(ang2)+sin(ang1)*sin(ang2)*cos(dstab)) 
; ; #    dstac=atan2(sin(dstab)*sin(ang1)*sin(ang2),cos(ang2)+cos(ang1)*cos(ang3))
; ; #    latc=asin(sin(lata)*cos(dstac)+cos(lata)*sin(dstac)*cos(crsac))
; ; #    dlon=atan2(sin(crsac)*sin(dstac)*cos(lata),cos(dstac)-sin(lata)*sin(latc))
; ; #    lonc=mod(lona-dlon+pi,2*pi)-pi
; ; # ENDIF
; 	 (setf ang1 (abs ang1))
; 	 (setf ang2 (abs ang2))
; 	 (setf ang3 (acos (+ (* (- 0(cos ang1)) (cos ang2))
; 			     (* (sin ang1) (sin ang2) (cos dstab)))))
; 	 (setf dstac (atan (* (sin dstab) (sin ang1) (sin ang2))
; 			   (+ (cos ang2) (* (cos ang1) (cos ang3)))))
; 	 (setf latc (asin (+ (* (sin lata) (cos dstac))
; 			     (* (cos lata) (sin dstac) (cos crsac)))))
; 	 (setf dlon (atan (* (sin crsac) (sin dstac) (cos lata))
; 			     (- (cos dstac) (* (sin lata) (sin latc)))))
; 
; 
; 
; ; #    lonc=mod(lona-dlon+pi,2*pi)-pi
; 
; 
; ; #
; ; # The points 1,2 and the (if unique) intersection 3 form a spherical
; ; # triangle with interior angles abs(ang1), abs(ang2) and ang3. To find
; ; # the pair of antipodal intersections of two great circles uses the
; ; # following reference.

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

;; -= other stuff =-

;;x=latitude (N degrees) y=longitude (W degrees) var= variation (degrees)

;;   var=  -65.6811 + 0.99*x + 0.0128899*x^2 - 0.0000905928*x^3 + 2.87622*y -
;;        0.0116268*x*y - 0.00000603925*x^2*y - 0.0389806*y^2 -
;;        0.0000403488*x*y^2 + 0.000168556*y^3

;; Continental US only, 3771 points, RMS error 1 degree All within 2
;; degrees except for the following airports: MO49 MO86 MO50 3K6 02K
;; and KOOA (24 < lat < 50, 66 < lon < 125)

(defun true-to-magnetic (point)
  "Converts a true heading in degrees to a magnetic heading in
degrees. Only valid for Continental US."
  (+
   (- 0.0 65.6811)
   (* 0.99 (point-lat point))
   (* 0.0128899 (expt (point-lat point) 2))
   (- 0.0 (* 0.0000905928 (expt (point-lat point) 3)))
   (* 2.87622 (- 0.0 (point-lon point)))
   (- 0.0 (* 0.0116268 (point-lat point) (- 0.0 (point-lon point))))
   (- 0.0 (* 0.00000603925 (expt (point-lat point) 2) (- 0.0 (point-lon point))))
   (- 0.0 (* 0.0389806 (expt (- 0.0 (point-lon point)) 2)))
   (- 0.0 (* 0.0000403488 (point-lat point) (expt (- 0.0 (point-lon point)) 2)))
   (* 0.000168556 (expt (- 0.0 (point-lon point)) 3))))

;; Continental US only, 3771 points, RMS error 1 degree All within 2
;; degrees except for the following airports: MO49 MO86 MO50 3K6 02K
;; and KOOA (24 < lat < 50, 66 < lon < 125)

(defun magnetic-to-true (point)
  "Converts a magnetic heading in degrees to a true heading in
degrees. Only valid for Continental US."
  (- 0.0 (true2magnetic point)))

;; Alaska Fit, better than 1 degree, all points:
;;   var=  618.854 + 2.76049*x - 0.556206*x^2 + 0.00251582*x^3 - 12.7974*y +
;;         0.408161*x*y + 0.000434097*x^2*y - 0.00602173*y^2 -
;;         0.00144712*x*y^2 + 0.000222521*y^3

;;     55 points (x > 54, 130 < y < 172)

;; (defvar *lax* (make-instance '2d-point :lat 33.95 :lon -118.4))
;; (defvar *jfk* (make-instance '2d-point :lat 40.6333333 :lon -73.78333336))
