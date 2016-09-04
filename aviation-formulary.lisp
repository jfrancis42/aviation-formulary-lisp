;;;; aviation-formulary-lisp.lisp

(in-package #:aviation-formulary)

;; Unit conversions, etc.
;;
;;  1 knot = 1.852000 km/hr*
;;  1 knot = 185200/109728 ft/sec* =1.687810 ft/sec
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
(defconstant *radians-per-degree* (/ pi 180))

;; The radius of the Earth in miles (roughly the WGS84/NAD83 value).
(defconstant *earth-radius* 3956.09)

;; Sources for points.
(defconstant *point-gps* 0)
(defconstant *point-interpolated* 1)
(defconstant *point-user* 2)
(defconstant *point-generated* 3)

;; degrees to radians
(defmacro d2r (d) `(/ (* ,d pi) 180))

;; radians to degrees
(defmacro r2d (r) `(/ (* ,r 180) pi))

;; feet to statute miles
(defmacro ft2sm (ft) `(/ ,ft 5280))

;; statute miles to feet
(defmacro sm2ft (mi) `(* ,mi 5280))

;; my own modulus
(defun my-mod (y x) (- y (* x (floor (/ y x)))))

(defun decimal-to-cardinal-course (course)
  "Given a decimal compass heading, return a string with the English
cardinal name."
  (cond
   ((and (>= course 337.5) (<= course 360.0))
    (format nil "North"))
   ((and (>= course 0) (< course 22.5))
    (format nil "North"))
   ((and (>= course 22.5) (< course 67.5))
    (format nil "Northeast"))
   ((and (>= course 67.5) (< course 112.5))
    (format nil "East"))
   ((and (>= course 112.5) (< course 157.5))
    (format nil "Southeast"))
   ((and (>= course 157.5) (< course 202.5))
    (format nil "South"))
   ((and (>= course 202.5) (< course 247.5))
    (format nil "Southwest"))
   ((and (>= course 247.5) (< course 292.5))
    (format nil "West"))
   ((and (>= course 292.5) (< course 337.5))
    (format nil "Northwest"))
   (t
    (format nil "This can't happen"))))

(defun dms2deg (d m &optional (s 0.0))
  "Convert Degrees, Minutes, and (optionally) seconds to decimal
degrees."
  (* 1.0 (+ d (/ (+ m (/ s 60)) 60))))

(defun string-to-number (str)
  "Convert a number in string form to a number."
  (if
      (or
       (null str)
       (equal "" str))
      0
      (let ((tmp 0.0))
	(setf tmp (read-from-string str))
	(if (numberp tmp)
	    tmp
	    nil))))

;; Serial number for points.  Start at -1 so the first point is point
;; 0.
(defvar *point-serial-number* -1)

;; -=-=-=-=-=-=- OBJECTS and METHODS -=-=-=-=-=-=-

;; This class provides creation, source, user, and modification
;; information.  It is not intended to be instantiated, it's here for
;; the sole purpose of being inherited.
(defclass user-info ()
  ((serial-number :accessor point-serial-number
		  :initarg :serial-number
		  :initform (bt:with-lock-held (*serial-lock*) (setf *point-serial-number* (+ 1 *point-serial-number*))))
   (creation-time-t :accessor point-creation-time-t
		    :initarg :creation-time-t
		    :initform nil)
   (creation-time-string :accessor point-creation-time-string
			 :initarg :creation-time-string
			 :initform nil)
   (creation-source :accessor point-creation-source
		    :initarg :creation-source
		    :initform nil)
   (update-time-t :accessor point-update-time-t
		  :initarg :update-time-t
		  :initform nil)
   (update-source :accessor point-update-source
		  :initarg :update-source
		  :initform nil)
   (description :accessor point-description
		:initarg :description
		:initform nil)))

(defmethod user-info-serialize ((p user-info))
  "Serialize the user-info part of the object."
  (list
   (list 'serial-number (point-serial-number p))
   (list 'creation-time-t (point-creation-time-t p))
   (list 'creation-time-string (point-creation-time-string p))
   (list 'creation-source (point-creation-source p))
   (list 'update-time-t (point-update-time-t p))
   (list 'update-source (point-update-source p))
   (list 'description (format nil "\"~A\"" (point-description p)))))

(defmethod user-info-deserialize-method ((p user-info) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'serial-number)
		(setf (point-serial-number p) (second n)))
	       ((equal (first n) 'creation-time-t)
		(setf (point-creation-time-t p) (second n)))
	       ((equal (first n) 'creation-time-string)
		(setf (point-creation-time-string p) (second n)))
	       ((equal (first n) 'update-time-t)
		(setf (point-update-time-t p) (second n)))
	       ((equal (first n) 'creation-source)
		(setf (point-creation-source p) (second n)))
	       ((equal (first n) 'update-source)
		(setf (point-update-source p) (second n)))
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

;; The 2d point is a superclass of 'point' and 'user-info', and
;; becomes the base class of all the other types of points we come up
;; with.
(defclass 2d-point (point user-info)
  ())

(defmethod url-photo ((p 2d-point))
  "Print an ACME url for this point."
  (format nil "http://www.acme.com/mapper/?lat=~F&long=~F&scale=10&theme=Image&width=4&height=3&dot=Yes"
	  (point-lat p) (point-lon p)))

(defmethod url-map ((p 2d-point))
  "Print a Mapquest url for this point."
  (format nil "http://www.mapquest.com/maps/map.adp?latlongtype=decimal&latitude=~F&longitude=~F"
	  (point-lat p) (point-lon p)))

(defmethod google-url-hybrid ((p 2d-point))
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?ll=~F,~F&spn=0.006362199783325195,0.009344816207885742&t=h&hl=en"
	  (point-lat p) (point-lon p)))

(defmethod google-url-photo ((p 2d-point))
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?ll=~F,~F&spn=0.006362199783325195,0.009344816207885742&t=k&hl=en"
	  (point-lat p) (point-lon p)))

(defmethod google-url-map ((p 2d-point))
  "Print a google url for this point.."
  (format nil "http://maps.google.com/maps?spn=~F,~F&hl=en"
	  (point-lat p) (point-lon p)))

(defmethod point-serialize ((p 2d-point))
  "Serialize a 2d point."
  (append
   (list
    '(type 2d-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p)))
   (user-info-serialize p)))

(defmethod pp ((p 2d-point))
  "Pretty print a 2d point."
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p 2d-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (user-info-deserialize-method p point-data)
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

;;  Decendant of 2d-point.  Marks stops.
(defclass stop-point (2d-point)
  ((count :accessor point-count
	  :initarg :count
	  :initform 1)
   (ignore :accessor point-ignore
	   :initarg ignore
	   :initform nil)))

(defmethod point-serialize ((p stop-point))
  "Serialize a stop point."
  (append
   (list
    '(type stop-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'count (point-count p))
    (list 'ignore (point-ignore p)))
   (user-info-serialize p)))

(defmethod point-deserialize-method ((p stop-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (user-info-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'lat)
		(setf (point-lat p) (second n)))
	       ((equal (first n) 'lon)
		(setf (point-lon p) (second n)))
	       ((equal (first n) 'count)
		(setf (point-count p) (second n)))
	       ((equal (first n) 'ignore)
		(setf (point-ignore p) (second n)))
	       ((equal (first n) 'datum)
		(setf (point-datum p) (second n)))
	       ))
	  point-data))

;;  Decendant of 2d-point.  Adds depth, magnitude, reporting station, number of observations.
(defclass eqs-point (2d-point)
  ((depth :accessor point-depth
	:initarg :depth
	:initform nil)
   (magnitude :accessor point-magnitude
	      :initarg :magnitude
	      :initform nil)
   (reporting-station :accessor point-reporting-station
		      :initarg :reporting-station
		      :initform nil)
   (number-of-observations :accessor point-number-of-observations
			   :initarg :number-of-observations
			   :initform nil)))

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
   (user-info-serialize p)))

(defmethod pp ((p 3d-point))
  "Pretty print a 3d point."
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Alt:  ~F~%" (point-alt p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p 3d-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (user-info-deserialize-method p point-data)
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

;;  Decendant of 2d-point.
(defclass nav-point (2d-point)
  ((alt :accessor point-freq
	:initarg :freq
	:initform nil)
   (var :accessor point-var
	:initarg :var
	:initform nil)))

(defmethod point-serialize ((p nav-point))
  "Serialize a 3d point."
  (append
   (list
    '(type nav-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'var (point-var p))
    (list 'freq (point-freq p)))
   (user-info-serialize p)))

(defmethod pp ((p nav-point))
  "Pretty print a 3d point."
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Freq:  ~F~%" (point-freq p))
  (format t "Variation:  ~F~%" (point-var p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p nav-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (user-info-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'lat)
		(setf (point-lat p) (second n)))
	       ((equal (first n) 'lon)
		(setf (point-lon p) (second n)))
	       ((equal (first n) 'var)
		(setf (point-var p) (second n)))
	       ((equal (first n) 'freq)
		(setf (point-freq p) (second n)))
	       ((equal (first n) 'datum)
		(setf (point-datum p) (second n)))
	       ))
	  point-data))

(defclass eq-point (3d-point)
  ((src :accessor point-src
	:initarg :src
	:initform nil)
   (eqid :accessor point-eqid
	 :initarg :eqid
	 :initform nil)
   (version :accessor point-version
	    :initarg :version
	    :initform nil)
   (magnitude :accessor point-magnitude
	      :initarg :magnitude
	      :initform nil)
   (nst :accessor point-nst
	:initarg :nst
	:initform nil)))

;; -=-=-=-=-=-=- FUNCTIONS -=-=-=-=-=-=-

(defun point-deserialize (point-data &optional point-type)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (let ((new-obj nil))
    (if (null point-type)
	(setf new-obj (make-instance (second (assoc 'type point-data))))
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

(defun calc-bearing (point-a point-b)
  "Calculate the bearing between point-a and point-b.  Returns a
value between 0 and 360."
  (if
      ;; If the points are the same, why do the math?
      (point-same-p point-a point-b)
      0
    (let
	((lat1 (d2r (- 0 (point-lat point-b))))
	 (lon1 (d2r (point-lon point-b)))
	 (lat2 (d2r (- 0 (point-lat point-a))))
	 (lon2 (d2r (point-lon point-a))))

      (r2d (my-mod (atan (* (sin (- lon1 lon2)) (cos lat2))
			 (- (* (cos lat1) (sin lat2)) (* (sin lat1) (cos lat2) (cos (- lon1 lon2))))) (* 2 pi))))))

(defun calc-bearing-180 (point-a point-b)
  "Identical to 'calc-bearing', except returns an angle from -180 to
+180 instead of 0 to 360."
  (let
      ((tmp (calc-bearing point-a point-b)))
    (if (< tmp 180)
	tmp
      (- tmp 360))))

(defun bearing-diff (point-a point-b point-c)
  "Return the difference in angles between point-a and point-b
vs. point-b and point-c."
  (if
      (and
       (point-same-p point-a point-b)
       (point-same-p point-a point-c))
      0
    (let*
	((a (calc-bearing point-a point-b))
	 (b (calc-bearing point-b point-c))
	 (c (- b a)))

      (cond
       ((and
	 (<= c 180)
	 (>= c -180))
	c)
       ((< c -180)
	(+ 360 c))
       ((> c 180)
	(* -1 (- 360 c)))
       (t
	nil)))))

(defun calc-distance (point-a point-b)
  "Calculate the distance in statute miles between point-a and
point-b."
  (if
      (equal point-a point-b)
      0
    (let
	((lat1 (d2r (point-lat point-a)))
	 (lon1 (d2r (point-lon point-a)))
	 (lat2 (d2r (point-lat point-b)))
	 (lon2 (d2r (point-lon point-b))))
      (/ (*
	  pi *earth-radius*
	  (* 2 (asin (sqrt (+ (expt (sin (/ (- lat1 lat2) 2)) 2)
			      (* (expt (sin (/ (- lon1 lon2) 2)) 2) (cos lat2) (cos lat1))))))
	  ) pi)
      )))

(defun calc-distance-flat (point-a point-b)
  "Calculate the distance in statute miles between point-a and point-b
on a plane.  This is much more accurate for very small distances than
'calc-distance', but only works for distances of up to about ten
miles, because it does not take the curvature of the earth into
account.  If neither supplied point includes altitude, the altitude is
considered to be zero.  If one point has altitude and the other does
not, the altitude used will be that of the point that supplied an
altitude.  If both points include altitude, the actual altitudes of
both points will be used.  This function assumes that altitude will be
supplied in feet."
  (let
      ((r (* 5280 *earth-radius*))
       (point-a-alt (assoc 'alt (point-serialize point-a)))
       (point-b-alt (assoc 'alt (point-serialize point-b)))
       (point-a-lat (d2r (point-lat point-a)))
       (point-a-lon (d2r (point-lon point-a)))
       (point-b-lat (d2r (point-lat point-b)))
       (point-b-lon (d2r (point-lon point-b))))

    (unless (null point-a-alt)
      (setf point-a-alt (point-alt point-a)))
    (unless (null point-b-alt)
      (setf point-b-alt (point-alt point-b)))

    (cond
     ((and (null point-a-alt) (null point-b-alt))
      (setf point-a-alt 0)
      (setf point-b-alt 0))
     ((and (null point-a-alt) (not (null point-b-alt)))
      (setf point-a-alt point-b-alt))
     ((and (not (null point-a-alt)) (null point-b-alt))
      (setf point-b-alt point-a-alt)))

    (/ (sqrt (+
	      (expt (- (* (+ r point-b-alt) (cos point-b-lat) (cos point-b-lon)) (* (+ r point-a-alt) (cos point-a-lat) (cos point-a-lon))) 2)
	      (expt (- (* (+ r point-b-alt) (sin point-b-lat)) (* (+ r point-a-alt) (sin point-a-lat))) 2)
	      (expt (- (* (+ r point-b-alt) (cos point-b-lat) (cos point-b-lon)) (* (+ r point-a-alt) (cos point-a-lat) (cos point-a-lon))) 2))) 5280)))

(defun calc-new-point (point-a d az)
  "Returns an object of type 2d-point given a starting point,
distance in statute miles, and azimuth.  Using this simple (and
fast) algorythm, you're limited to distances where the longitude
is <= 1/4 the circumference of the Earth.  Allowing for longer
distances requires a much more complex (and slow) solution.  For
my purposes (usually <= 100 miles), this is fine."
  (let
      ((point-a-lat (d2r (point-lat point-a)))
       (point-a-lon (d2r (point-lon point-a)))
       (lat nil)
       (lon nil)
       (dist (/ d *earth-radius*))
       (azimuth (- 0 (d2r az))))

    (setf lat (asin (+ (* (sin point-a-lat) (cos dist)) (* (cos point-a-lat) (sin dist) (cos azimuth)))))
    (if (eq (cos point-a-lat) 0.0)
	(setf lon point-a-lon)
      (setf lon (- (my-mod (+ pi (- point-a-lon (asin (/ (* (sin azimuth) (sin dist)) (cos lat))))) (* 2 pi)) pi)))

    (make-instance '2d-point :creation-source *point-generated* :lat (r2d lat) :lon (r2d lon))))

(defun point-predict (point-a step)
  "This function predicts the location of where you'll be given your
current course and speed 'step' seconds from now."
  (let*
      ((now (point-serialize point-a))
       (spd (second (assoc 'spd now)))
       (crs (second (assoc 'crs now))))
    (calc-new-point point-a (* step (/ spd 3600)) crs)))

;; -= other other =-


;; per the aviation formulary:

;;x=latitude (N degrees) y=longitude (W degrees) var= variation (degrees)

;;   var=  -65.6811 + 0.99*x + 0.0128899*x^2 - 0.0000905928*x^3 + 2.87622*y -
;;        0.0116268*x*y - 0.00000603925*x^2*y - 0.0389806*y^2 -
;;        0.0000403488*x*y^2 + 0.000168556*y^3

;; convert a true heading to magnetic
;; Continental US only, 3771 points, RMS error 1 degree All within 2 degrees except for the following airports: MO49 MO86 MO50 3K6 02K and KOOA
;; (24 < lat < 50,  66 < lon < 125)
(defun true2magnetic (point)
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

;; convert a magnetic heading to true
;; Continental US only, 3771 points, RMS error 1 degree All within 2 degrees except for the following airports: MO49 MO86 MO50 3K6 02K and KOOA
;; (24 < lat < 50,  66 < lon < 125)
(defun magnetic2true (point)
  (- 0.0 (true2magnetic point)))

;; TODO

;; Alaska Fit, better than 1 degree, all points:
;;   var=  618.854 + 2.76049*x - 0.556206*x^2 + 0.00251582*x^3 - 12.7974*y +
;;         0.408161*x*y + 0.000434097*x^2*y - 0.00602173*y^2 -
;;         0.00144712*x*y^2 + 0.000222521*y^3

;;     55 points (x > 54, 130 < y < 172)

