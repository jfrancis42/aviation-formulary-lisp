# aviation-formulary
A Common Lisp library implementing the algorithms in the Aviation
Formulary v1.46 found at: https://www.edwilliams.org/avform.htm

The Aviation Formulary is a document written by Ed Williams that (in his own words), "Contains a fairly complete collection of great circle and rhumb line navigation formulae. It also contains standard atmosphere properties and E6B functions." This library is an implementation of the formulas in this document. This work has been done completely independently of Mr. Williams and (at least so far as I know), without his knowledge.

While the spirit of the Formulary's document is maintained, a few changes were necessary in order to implement this as a practical solution. First among these was a switch from Mr. Williams' representation of longitude to the more common standard. Mr. Williams represents West Longitude as a postive number, whereas the much more common standard is to represent East Longitude as positive and West as negative. The code has been written to support the more common standard of West Longitude being negative, for easier integration with other geo systems.

Second among the changes is the introduction of objects to represent locations upon which the calculations operate. There are various objects, including both 2D and 3D location objects, which contain what you would expect (latitude, longitude, altitude, for example) as well as metadata about the object (a serial number, a creation timestamp, a source, and a few other fields). These classes also have methods for printing their contents, doing various calculations, and serializing and deserializing their data for saving/loading to disk and/or streaming across a network connection.

Note that the package can be referred to by it's nickname "af" rather than the full "aviation-formulary" name, which makes functions shorter and easier to read.

The simplest object is a 2d-point object. Points can be created like this:

```
CL-USER> (defvar *trinity-site* (make-instance 'af:2d-point :lat 33.6772929 :lon -106.4752871 :name "Trinity Site" :description "Location of the first atomic explosion."))
*TRINITY-SITE*
CL-USER> (defvar *alamogordo* (make-instance 'af:2d-point :lat 32.8995 :lon -105.9603 :name "Alamogordo" :description "The city of Alamogordo, NM."))
*ALAMOGORDO*
CL-USER>
```

These point become the basis of the calculations that can be done with the library. While implementation of the formulas in the document is a work in progress, the very basics are complete, and it's now possible to calculate distances and directions with the various library functions. Additional functions (density altitude, etc) will be implemented as time permits (code submissions welcome). Note that while the location objects themselves are specified using lat and lon as decimal degrees, essentially every other function in the library requires the specification of arguments in terms of radians, both for distances and angles. To facilitate the easier use of the library, various macros are provided for converting back and forth between radians and more common measurement units. These macros include:

* rad-to-km
* km-to-rad
* rad-to-deg
* deg-to-rad

The length of a radian in km is non-trivial, and depends heavily on the reference model of the Earth in use. As the Earth has a larger radius around the Equator than around the Poles, there is no single "right" answer. As part of the library initialization, the variable \*earth-radius\* is set to the historically accepted value of 6366.71km. This falls conveniently between the WGS84 values for Equatorial and Polar radius of 6378.137km and 6356.752km. This value can be changed, if required for specific uses, and will be used as the default for all subsequent calculations:

```
CL-USER> (setf af::*earth-radius* 6371.0)
6371.0
CL-USER>
```

Alamogordo, NM is often mistakenly quoted as the site of the world's first atomic explosion. This annoys me, as I grew up in Alamogordo. Using the two location objects we created above, let's see how far off this "fact" is from reality. Notice I'm using the unit conversion macros to turn the output of the various functions into more useful units:

```
CL-USER> (af:rad-to-km (af:calc-distance *alamogordo* *trinity-site*))
98.78273346962875d0
CL-USER>
```

As you can see, the explosion site is almost 100km away. But in what direction? Glad you asked:

```
CL-USER> (af:rad-to-deg (af:calc-gc-bearing *alamogordo* *trinity-site*))
331.17733610371045d0
CL-USER>
```

331 degrees. There are a few extra "goodies" in the library, such as a function to turn decimal degrees into their cardinal equivalents:

```
CL-USER> (af:deg-to-cardinal-course (af:rad-to-deg (af:calc-gc-bearing *alamogordo* *trinity-site*)))
"Northwest"
CL-USER>
```
