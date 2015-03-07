(ns geojson.test.validate
  (:require [clojure.test :refer :all]
            [geojson.validate :refer :all]))

(deftest test-valid-coordinates?
  (let [valid-coordinates? @#'geojson.validate/valid-coordinates?]
    (are [depth coords] (true? (valid-coordinates? depth coords))
         1 [1 2]
         2 [[1 2]]
         3 [[[1 2] [3 4]]]
         1 [1 2 3])
    (are [depth coords] (false? (valid-coordinates? depth coords))
         1 nil
         1 [1]
         2 [1 2]
         3 [1 2]
         1 [[1 2]]
         2 [[1 2] 3]
         1 ["a" "b"])))

(deftest test-linear-ring?
  (let [linear-ring? @#'geojson.validate/linear-ring?]
    (are [coords] (true? (linear-ring? coords))
         [[0 0] [0 1] [1 1] [0 0]]
         [[0 0] [0 1] [1 1] [1 0] [0 0]])
    (are [coords] (false? (linear-ring? coords))
         true
         false
         nil
         "abc"
         {}
         []
         [[0 0] [0 1] [0 0]]
         [[0 0] [0 1] [1 1] [1 0]])))

(deftest test-valid?
  (are [geometry] (true? (valid? geometry))
       ; Some edge cases
       {:type "GeometryCollection" :geometries []}
       {:type "FeatureCollection" :features []}
       ; All the GeoJSON objects from geojson.org
       {:type "Point" :coordinates [100.0 0.0]}
       {:type "LineString" :coordinates [[100.0 0.0] [101.0 1.0]]}
       {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]]]}
       {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]] [[100.2 0.2] [100.8 0.2] [100.8 0.8] [100.2 0.8] [100.2 0.2]]]}
       {:type "MultiPoint" :coordinates [[100.0 0.0] [101.0 1.0]]}
       {:type "MultiLineString" :coordinates [[[100.0 0.0] [101.0 1.0]] [[102.0 2.0] [103.0 3.0]]]}
       {:type "MultiPolygon" :coordinates [[[[102.0 2.0] [103.0 2.0] [103.0 3.0] [102.0 3.0] [102.0 2.0]]] [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]] [[100.2 0.2] [100.8 0.2] [100.8 0.8] [100.2 0.8] [100.2 0.2]]]]}
       {:type "GeometryCollection" :geometries [{:type "Point" :coordinates [100.0 0.0]} {:type "LineString" :coordinates [[101.0 0.0] [102.0 1.0]]}]}
       {:type "FeatureCollection" :features [{:type "Feature" :geometry {:type "Point" :coordinates [102.0 0.5]} :properties {:prop0 "value0"}} {:type "Feature" :geometry {:type "LineString" :coordinates [[102.0 0.0] [103.0 1.0] [104.0 0.0] [105.0 1.0]]} :properties {:prop0 "value0" :prop1 0.0}} {:type "Feature" :geometry {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]]]} :properties {:prop0 "value0" :prop1 {:this "that"}}}]}
       ; Features
       {:type "Feature" :geometry nil :properties {}}
       {:type "Feature" :geometry {:type "Point" :coordinates [102.0 0.5]} :properties nil}
       {:type "Feature" :geometry {:type "Point" :coordinates [102.0 0.5]} :properties {:prop0 "value0"}})
  (are [geometry] (false? (valid? geometry))
       ; Some edge cases
       true
       false
       nil
       {}
       []
       "abc"
       ; Invalid type
       {:type "Dodecahedron" :coordinates [1 0]}
       ; nil coordinates
       {:type "Point" :coordinates nil}
       {:type "LineString" :coordinates nil}
       {:type "Polygon" :coordinates nil}
       {:type "MultiPoint" :coordinates nil}
       {:type "MultiLineString" :coordinates nil}
       {:type "MultiPolygon" :coordinates nil}
       {:type "GeometryCollection" :geometries nil}
       {:type "FeatureCollection" :features nil}
       ; string coords
       {:type "Point" :coordinates "abc"}
       {:type "LineString" :coordinates "abc"}
       {:type "Polygon" :coordinates "abc"}
       {:type "MultiPoint" :coordinates "abc"}
       {:type "MultiLineString" :coordinates "abc"}
       {:type "MultiPolygon" :coordinates "abc"}
       {:type "Feature" :geometry "abc" :properties {}}
       {:type "Feature" :geometry {:type "Point" :coordinates [1 2]} :properties "abc"}
       {:type "GeometryCollection" :geometries "abc"}
       {:type "FeatureCollection" :features "abc"}
       ; incorrect coordinates
       {:type "Point" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]]]}
       {:type "LineString" :coordinates [1 0]}
       {:type "Polygon" :coordinates [1 0]}
       {:type "MultiPoint" :coordinates [1 0]}
       {:type "MultiLineString" :coordinates [[1 0] [1 2]]}
       {:type "MultiPolygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]]]}
       ; linear ring violations
       {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [100.0 0.0]]]}
       {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0]]]}
       {:type "Polygon" :coordinates [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]] [[100.2 0.2] [100.8 0.2] [100.8 0.8]]]}
       {:type "MultiPolygon" :coordinates [[[[103.0 3.0] [102.0 3.0] [102.0 2.0]]] [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]] [[100.2 0.2] [100.8 0.2] [100.8 0.8] [100.2 0.8] [100.2 0.2]]]]}
       {:type "MultiPolygon" :coordinates [[[[102.0 2.0] [103.0 2.0] [103.0 3.0] [102.0 3.0] [102.0 2.0]]] [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]] [[100.2 0.2] [100.8 0.2] [100.8 0.8]]]]}
       ; bad collections
       {:type "GeometryCollection"}
       {:type "FeatureCollection"}
       {:type "GeometryCollection" :geometries {}}
       {:type "FeatureCollection" :features {}}
       ))
