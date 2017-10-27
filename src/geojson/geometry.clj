(ns geojson.geometry
  "Geometric calculations on GeoJSON maps."
  (:require [clojure.walk :refer [keywordize-keys]]))

(def ^{:private true} sum (partial apply +))

(defn- mean
  "Calculates the mean of a list of numeric values."
  [values]
  (/ (sum values) (count values)))


;;;; Centroids

(defn- mean-point [points]
  (let [x (map first points)
        y (map second points)]
    [(mean x) (mean y)]))

(defmulti centroid
  "Obtains a centroid of the given geometry.

  For Points, use the point's coordinates.

  For Polygons and Multipolygons, use an averaging methodology:
  http://en.wikipedia.org/wiki/Centroid#Of_a_finite_set_of_points
  This method may be invalid for Polygons and MultiPolygons, but is sufficient
  for our current needs."
  :type)

(defmethod centroid "Feature" [geometry]
  (centroid (:geometry geometry)))

(defmethod centroid "Point" [geometry]
  (:coordinates geometry))

(defmethod centroid "Polygon" [geometry]
  (let [coords (-> geometry
                   :coordinates
                   first
                   rest)]
    (when-not (empty? coords)
      (mean-point coords))))

(defmethod centroid "MultiPolygon" [geometry]
  (let [coords (->> geometry
                    :coordinates
                    (map (comp rest first))
                    (apply concat))]
    (when-not (empty? coords)
      (mean-point coords))))



;;;; Area

; The Earth's radius is 6,378,137 meters.
(def ^{:private true} earth-radius-square (Math/pow 6378137 2))

(defn- ring-area
  "Calculates the area contained within a ring of points.

  See polygon-area for more details."
  [coordinates]
  (->> (partition 2 1 (take 1 coordinates) coordinates)
       (map (fn [[p1 p2]]
              (let [p1-lng (Math/toRadians (first p1))
                    p1-lat (Math/toRadians (second p1))
                    p2-lng (Math/toRadians (first p2))
                    p2-lat (Math/toRadians (second p2))]
                (* (- p2-lng p1-lng) (+ 2 (Math/sin p2-lat) (Math/sin p1-lat))))))
       sum
       Math/abs))

(defn- polygon-area
  "Calculates the square meter area contained within a polygon, subtracting holes.

  Projects each vertex and reduces polygon area with http://en.wikipedia.org/wiki/Shoelace_formula"
  [coordinates]
  (let [area (ring-area (first coordinates))
        holes (sum (map ring-area (rest coordinates)))
        net (- area holes)]
    (/ (* net earth-radius-square) 2.0)))

(defmulti area
  "Calculate the area of a polygon projected on a sphere. Jacked from `getGeodesicArea` at
  http://dev.openlayers.org/releases/OpenLayers-2.10/lib/OpenLayers/Geometry/LinearRing.js"
  :type)

(defmethod area "Feature" [geometry]
  (area (:geometry geometry)))

(defmethod area "Polygon" [geometry]
  (polygon-area (:coordinates geometry)))

(defmethod area "MultiPolygon" [geometry]
  (->> geometry
       :coordinates
       (map polygon-area)
       sum))

(defmethod area "Point" [geometry] 0.0)
(defmethod area "MultiPoint" [geometry] 0.0)
(defmethod area "LineString" [geometry] 0.0)
(defmethod area "MultiLineString" [geometry] 0.0)

(defn acreage
  "Calculate the area of a geometry, in acreage."
  [geometry]
  (when-let [sqm (area geometry)]
    (/ sqm 4046.86)))



;;;; Geometry Cleaning

(defn- clean-coords
  "Given a list of arbitrarily nested coordinates:
    * take only the x, y coordinates
    * trim the value down to 8 decimal places"
  [coords]
  (if (sequential? (first coords))
    (map clean-coords coords)
    (map #(Double/parseDouble (format "%.8f" (double %))) (take 2 coords))))

(defn clean
  "Removes excess precision from a 2 dimensional geometry."
  [geometry]
  (let [ks (if (= "Feature" (:type geometry)) [:geometry :coordinates] [:coordinates])]
    (update-in geometry ks clean-coords)))


;;;; Point in polygon problem

(defn- crossing-number
  "Taken from Joshua Miller with permission
   https://github.com/joshuamiller/hbg-crime/blob/master/src/clj/hbg_crime/geometry.clj

   Determine crossing number for given point and segment of a polygon.
   See http://geomalgorithms.com/a03-_inclusion.html"
  [[px py] [[x1 y1] [x2 y2]]]
  (if (or (and (<= y1 py) (> y2 py))
          (and (> y1 py) (<= y2 py)))
    (let [vt (/ (- py y1) (- y2 y1))]
      (if (< px (+ x1 (* vt (- x2 x1))))
        1 0))
    0))

(defn inside?
  "Is point inside the given polygon?
  inside? point polygon -> boolean
  input: point, [x y], polygon, vector of points
  output: a boolean indicating whether or not point is in polygon
  assumes non-self-intersecting polygons"
  [point polygon]
  (odd? (reduce + (for [n (range (dec (count polygon)))]
                    (crossing-number point [(nth polygon n)
                                            (nth polygon (inc n))])))))

(defn bounding-box
  "Given a geo-json geometry, returns the geojson that should be the exact same as
  ST_envelope function that represents the Bounding Box"
  [geo-json]
  (let [geo (keywordize-keys geo-json)
        coords (->> geo
                     :coordinates
                     flatten
                     (partition 2))
        lats (map second coords)
        lons (map first coords)
        max-lon (apply max lons)
        max-lat (apply max lats)
        min-lon (apply min lons)
        min-lat (apply min lats)]
    {:coordinates [[[min-lon min-lat]
                    [min-lon max-lat]
                    [max-lon max-lat]
                    [max-lon min-lat]
                    [min-lon min-lat]]]
     :type "Polygon"}))
