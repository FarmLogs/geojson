(ns geojson.test.generators
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [geojson.validate :as validate]
            [geojson.generators :as geojson-gen]))

(deftest test-geojson-generators
  (are [g] (every? validate/valid? (gen/sample g))
       geojson-gen/multi-point
       geojson-gen/linestring
       geojson-gen/multi-linestring
       geojson-gen/polygon
       geojson-gen/multi-polygon
       geojson-gen/geometry
       geojson-gen/geometry-collection
       geojson-gen/feature
       geojson-gen/feature-collection
       geojson-gen/geojson))
