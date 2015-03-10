(ns geojson.generators
  "A set of test.check GeoJSON generators."
  (:require [clojure.test.check.generators :as gen]))

(def ^{:private true} max-count
  "Keeps the vector generation within a reasonable upper bound."
  100)

(def ^{:private true} lng
  "Generates a valid longitude."
  (->> (gen/fmap float gen/ratio)
       (gen/such-that #(>= % -180))
       (gen/such-that #(<= % 180))))

(def ^{:private true} lat
  "Generates a valid latitude."
  (->> (gen/fmap float gen/ratio)
       (gen/such-that #(>= % -90))
       (gen/such-that #(<= % 90))))

(def ^{:private true} point-coords
  "Generates a valid [x y] point."
  (gen/tuple lng lat))

(def ^{:private true} linestring-coords
  "Generates a list of points for linestring types."
  (->> (gen/vector point-coords 2 max-count)
       (gen/fmap #(map first (partition-by identity %)))
       (gen/such-that #(>= (count %) 2))))

(defn- overlapping?
  "Determines whether the provided points are arranged in a line which overlaps
   itself.  This is a subset of self intersection and is invalid.
   We know the lines between a set of points overlap if one axis remains
   constant while the other is neither constantly ascending nor descending."
  [& points]
  (let [xs (map first points)
        ys (map second points)]
    (or (and (apply = xs)
             (or (= (sort ys) ys)
                 (= (reverse (sort ys)) ys)))
        (and (apply = ys)
             (or (= (sort xs) xs)
                 (= (reverse (sort xs)) xs))))))

(defn- sort-points
  "Arranges points such that there is no self-intersection.
   This works by calculating the angle between the bounding box's center and
   each point, then sorting those points by their angle in a counterclockwise
   direction."
  [points]
  (let [xs (map first points)
        minx (apply min xs)
        maxx (apply max xs)
        ys (map second points)
        miny (apply min ys)
        maxy (apply max ys)
        mean [(+ (/ (- maxx minx) 2) minx)
              (+ (/ (- maxy miny ) 2) miny)]]
    (sort-by #(Math/atan2 (- (second mean) (second %)) (- (first mean) (first %))) points)))

(def ^{:private true} linear-ring-coords
  "Generates a valid linear ring, ie for a Polygon geometry."
  (->> (gen/vector point-coords 3 max-count)
       (gen/fmap distinct)
       (gen/such-that #(>= (count %) 3))
       (gen/fmap sort-points)
       (gen/such-that #(every? false? (map overlapping? % (drop 1 %) (drop 2 %))))
       (gen/fmap #(concat (take 1 %)
                          (rest %)
                          (take 1 %)))))

(def ^{:private true} polygon-coords
  "Generates coordinates for a polygon.
   TODO: add support polygons with interior rings."
  (gen/vector linear-ring-coords 1))

(def point
  "Generates a GeoJSON Point."
  (gen/hash-map :type (gen/return "Point")
                :coordinates point-coords))

(def multi-point
  "Generates a GeoJSON MultiPoint."
  (gen/hash-map :type (gen/return "MultiPoint")
                :coordinates (gen/fmap distinct (gen/vector point-coords 1 max-count))))

(def linestring
  "Generates a GeoJSON LineString."
  (gen/hash-map :type (gen/return "LineString")
                :coordinates linestring-coords))

(def multi-linestring
  "Generates a GeoJSON MultiLineString."
  (gen/hash-map :type (gen/return "MultiLineString")
                :coordinates (gen/vector linestring-coords)))

(def polygon
  "Generates a GeoJSON Polygon.
   TODO: add support polygons with interior rings."
  (gen/hash-map :type (gen/return "Polygon")
                :coordinates polygon-coords))

(def multi-polygon
  "Generates a GeoJSON MultiPolygon.
   TODO: make sure polygons do not intersect each other."
  (gen/hash-map :type (gen/return "MultiPolygon")
                :coordinates (gen/vector polygon-coords)))

(def geometry
  "Generates a GeoJSON geometry object of any basic type.
   Note that this does not include GeometryCollection objects."
  (gen/one-of [point multi-point linestring multi-linestring polygon multi-polygon]))

(def geometry-collection
  "Generates a GeoJSON GeometryCollection."
  (gen/hash-map :type (gen/return "GeometryCollection")
                :geometries (gen/vector geometry)))

(def feature
  "Generates a GeoJSON Feature."
  (gen/hash-map :type (gen/return "Feature")
                :geometry (gen/one-of [(gen/return nil) geometry])
                :properties (gen/one-of [(gen/return nil) (gen/map gen/keyword gen/any)])))

(def feature-collection
  "Generates a GeoJSON FeatureCollection."
  (gen/hash-map :type (gen/return "FeatureCollection")
                :features (gen/vector feature)))

(def geojson
  "Generates any valid GeoJSON object."
  (gen/one-of [geometry geometry-collection feature feature-collection]))
