# aviation-formulary
A Common Lisp library implementing the algorithms in the Aviation
Formulary v1.47 found at: https://www.edwilliams.org/avform147.htm

The Aviation Formulary is a document written by Ed Williams that (in his own words), "Contains a fairly complete collection of great circle and rhumb line navigation formulae. It also contains standard atmosphere properties and E6B functions." This library is an implementation of the formulas in this document. This work has been done completely independently of Mr. Williams and (at least so far as I know), without his knowledge.

While the spirit of the Formulary's document is maintained, a few changes were necessary in order to implement this as a practical solution. First among these was a switch from Mr. Williams' representation of longitude to the more common standard. Mr. Williams represents West Longitude as a positive number, whereas the much more common standard is to represent East Longitude as positive and West as negative. The code has been written to support the more common standard of West Longitude being negative, for easier integration with other geo systems.

Second among the changes is the introduction of objects to represent locations upon which the calculations operate. There are various objects, including both 2D and 3D location objects, which contain what you would expect (latitude, longitude, altitude, for example) as well as metadata about the object (a serial number, a creation timestamp, a source, and a few other fields). These classes also have methods for printing their contents, doing various calculations, and serializing and deserializing their data for saving/loading to disk and/or streaming across a network connection.

Note that the package can be referred to by its nickname "af" rather than the full "aviation-formulary" name, which makes functions shorter and easier to read.

## Point Objects

The simplest object is a 2d-point object. Points can be created like this:

```
CL-USER> (defvar *trinity-site* (make-instance 'af:2d-point :lat 33.6772929 :lon -106.4752871 :name "Trinity Site" :description "Location of the first atomic explosion."))
*TRINITY-SITE*
CL-USER> (defvar *alamogordo* (make-instance 'af:2d-point :lat 32.8995 :lon -105.9603 :name "Alamogordo" :description "The city of Alamogordo, NM."))
*ALAMOGORDO*
```

These points become the basis of the calculations that can be done with the library.

## Unit Conventions

- **Latitude/Longitude**: Stored in point objects as decimal degrees, East-positive (standard convention).
- **Angles and distances**: All navigation functions accept and return angles in **radians** and distances in **radians** (arc distance on the Earth's surface).
- **Altitudes**: Feet throughout, consistent with aviation convention.
- **Airspeeds**: Knots throughout.
- **Temperatures**: Celsius for user-facing functions; internal Kelvin conversion as needed.
- **Pressure**: Inches Hg for altimetry functions.
- **Wind direction**: The direction the wind is blowing **from**, in radians (0 = from the North, π/2 = from the East).

To convert between radians and more common units, use these macros:

```
af:rad-to-km      af:km-to-rad
af:rad-to-nm
af:rad-to-sm
af:rad-to-deg     af:deg-to-rad
af:ft-to-sm       af:sm-to-ft
```

The variable `*earth-radius*` controls the Earth radius used by the distance-conversion macros and is set by default to the historically accepted value of 6366.71 km. It can be changed:

```
CL-USER> (setf af::*earth-radius* af:earth-radius-fai-sphere)  ; 6371.0 km
```

Available radius constants: `earth-radius-nm-historic`, `earth-radius-wgs84-eq-radius`, `earth-radius-wgs84-polar-radius`, `earth-radius-fai-sphere`.

## Great Circle Navigation

```
;; Distance between two points (radians)
(af:calc-distance point-a point-b)              ; acos formula
(af:calc-distance-shorter point-a point-b)      ; haversine, more accurate

;; Initial bearing from point-a to point-b (radians, clockwise from North)
(af:calc-gc-bearing point-a point-b)

;; New point at distance d (radians) on azimuth az (radians) from point-a
(af:calc-new-point point-a d az)

;; Intermediate point at fraction f (0=point-a, 1=point-b)
(af:calc-gc-intermediate-point point-a point-b f)

;; Cross-track error of point-d from the course point-a -> point-b (radians)
;; Positive = right of course, negative = left
(af:calc-cross-track-error point-a point-b point-d)

;; Along-track distance from point-a to the closest point on the course to point-d
(af:calc-along-track-distance point-a point-b point-d)

;; Intersection of two radials (returns a 2d-point or nil if ambiguous)
(af:calc-radial-intersection point-1 crs13-rad point-2 crs23-rad)

;; Latitude (degrees) where the great circle crosses the given longitude
(af:calc-gc-lat-at-lon point-a point-b lon-deg)  ; returns nil on meridians

;; Maximum latitude reached on a great circle (Clairaut's formula)
(af:calc-gc-max-lat point bearing-rad)

;; Longitudes (degrees, list of two) where the GC crosses a given parallel
(af:calc-gc-parallel-crossings point-a point-b lat3-deg)  ; nil if no crossing
```

Example — distance and direction from Alamogordo to Trinity Site:

```
CL-USER> (af:rad-to-km (af:calc-distance-shorter *alamogordo* *trinity-site*))
98.78273346962875d0
CL-USER> (af:rad-to-deg (af:calc-gc-bearing *alamogordo* *trinity-site*))
331.17733610371045d0
CL-USER> (af:deg-to-cardinal-course (af:rad-to-deg (af:calc-gc-bearing *alamogordo* *trinity-site*)))
"Northwest"
```

## Rhumb Line Navigation

Rhumb line routes maintain a constant compass bearing; they are longer than great circle routes but simpler to fly.

```
;; Bearing from point-a to point-b on a rhumb line (radians)
(af:calc-rhumb-bearing point-a point-b)

;; Distance on a rhumb line (radians)
(af:calc-rhumb-distance point-a point-b)

;; New point at distance d (radians) on rhumb course tc (radians)
(af:calc-rhumb-new-point point-a d tc)
```

Both `calc-rhumb-bearing` and `calc-rhumb-distance` select the shorter path when the route would cross the 180th meridian.

## Local Flat Earth Approximation

For short distances (< ~500 km) a local flat-earth approximation using the WGS84 ellipsoid is available. Results are in kilometres.

```
;; Meridional (N/S) and transverse (E/W) radii of curvature at a latitude
(af:flat-earth-north-radius lat-deg)   ; km
(af:flat-earth-east-radius  lat-deg)   ; km

;; Displacement from origin to point as (values north-km east-km)
(af:calc-flat-earth-offset origin point)

;; Distance and bearing using the flat-earth approximation
(af:calc-flat-earth-distance origin point)   ; km
(af:calc-flat-earth-bearing  origin point)   ; radians
```

The WGS84 ellipsoid constants are also exported: `wgs84-a` (equatorial radius, km), `wgs84-f` (flattening), `wgs84-e2` (eccentricity squared).

## Wind Triangle

All angles are in radians; speeds in knots. Wind direction is the direction the wind is blowing **from**.

```
;; Determine wind from known TAS, groundspeed, heading, and actual course
;; Returns (values wind-direction-rad wind-speed-knots)
(af:calc-unknown-wind tas gs heading-rad course-rad)

;; Find heading and groundspeed to fly a desired course
;; Returns (values heading-rad groundspeed-knots) or (values nil nil) if impossible
(af:calc-heading-groundspeed course-rad tas wind-speed wind-direction-rad)

;; Find actual course and groundspeed given heading and wind
;; Returns (values course-rad groundspeed-knots)
(af:calc-course-groundspeed heading-rad tas wind-speed wind-direction-rad)

;; Headwind and crosswind components on a runway/heading
;; Returns (values headwind-knots crosswind-knots)
;; Positive headwind = into the wind; positive crosswind = wind from the right
(af:calc-wind-components wind-speed wind-direction-rad runway-direction-rad)

;; Estimate TAS and wind speed from three GPS groundspeeds 120° apart
;; Returns (values tas-knots wind-speed-knots)
(af:calc-tas-from-three-groundspeeds v1 v2 v3)
```

## Magnetic Variation

Three regional polynomial fits are provided. All return variation in degrees where **positive = West variation** (compass reads lower than true course) and **negative = East variation**.

```
;; Continental US (24 < lat < 50, 66 < lon_W < 125), RMS error ~1°
(af:magnetic-variation-conus point)   ; also available as (af:true-to-magnetic point)

;; Alaska (lat > 54, 130 < lon_W < 172), better than 1° for all points
(af:magnetic-variation-alaska point)

;; Western Europe (-10 < lon < 28, 36 < lat < 68)
(af:magnetic-variation-europe point)
```

To convert headings using the CONUS polynomial:

```
;; magnetic = true - variation
(af:true-heading-to-magnetic true-heading-deg point)

;; true = magnetic + variation
(af:magnetic-heading-to-true magnetic-heading-deg point)
```

## Standard Atmosphere and Altimetry

```
;; Standard temperature in Celsius at a given altitude (feet)
(af:standard-temperature altitude-ft)

;; Standard pressure in inches Hg at a given altitude (feet)
(af:standard-pressure-inhg altitude-ft)

;; Pressure altitude (feet) from indicated altitude and altimeter setting (in Hg)
(af:pressure-altitude indicated-altitude-ft altimeter-setting-inhg)

;; Density altitude (feet) from pressure altitude and actual temperature
(af:density-altitude pressure-altitude-ft temperature-c)

;; True altitude (feet) from calibrated altitude, station elevation,
;; ISA temperature deviation, and OAT
(af:true-altitude calibrated-altitude-ft field-elevation-ft isa-deviation-c oat-c)
```

## Airspeed Conversions

```
;; Speed of sound (knots) at a given temperature (Celsius)
(af:speed-of-sound temperature-c)

;; Mach number from TAS and OAT
(af:mach-number tas-knots temperature-c)

;; TAS from Mach and OAT
(af:tas-from-mach mach temperature-c)

;; TAS from CAS (calibrated airspeed) and density altitude — valid below 36,089 ft
(af:tas-from-cas cas-knots density-altitude-ft)

;; CAS from TAS and density altitude — valid below 36,089 ft
(af:cas-from-tas tas-knots density-altitude-ft)
```

## Humidity and Meteorology

```
;; Relative humidity (fraction) from temperature and dewpoint (Celsius)
(af:relative-humidity-from-dewpoint temperature-c dewpoint-c)

;; Dewpoint (Celsius) from temperature and relative humidity (fraction)
(af:dewpoint-from-humidity temperature-c relative-humidity)

;; Frostpoint (Celsius) from temperature and relative humidity (fraction)
(af:frostpoint-from-humidity temperature-c relative-humidity)

;; Density altitude increase (feet) due to humidity
(af:density-altitude-humidity-correction relative-humidity temperature-c pressure-altitude-ft)
```

## Bellamy's Formula

```
;; Wind drift in nautical miles (pressure difference in inches Hg)
(af:bellamy-drift pressure-difference-inhg latitude-rad tas-knots)

;; Wind correction angle in degrees
(af:bellamy-wind-correction-angle pressure-difference-inhg latitude-rad tas-knots distance-nm)
```

Positive pressure difference (destination higher than departure) produces left drift; the required WCA is to the right.

## Turn Performance

```
;; Turn radius (feet) from airspeed (knots) and bank angle (degrees)
(af:turn-radius airspeed-knots bank-angle-deg)

;; Rate of turn (degrees/second) from airspeed (knots) and radius (feet)
(af:turn-rate airspeed-knots radius-ft)

;; Bank angle (degrees) for a standard rate turn (3°/sec)
(af:standard-rate-bank-angle airspeed-knots)

;; Pivotal altitude (feet) from groundspeed (knots)
(af:pivotal-altitude groundspeed-knots)
```

## Distance to Horizon

```
;; Distance to the visual horizon (nautical miles) from altitude (feet)
;; Accounts for standard atmospheric refraction
(af:distance-to-horizon altitude-ft)
```

## Maidenhead Grid Locators

```
;; Convert a Maidenhead grid locator string to a 2d-point
(af:from-maidenhead "DM65")       ; 4-character or 6-character grids

;; Convert a 2d-point to a 6-character Maidenhead grid locator
(af:to-maidenhead point)
```
