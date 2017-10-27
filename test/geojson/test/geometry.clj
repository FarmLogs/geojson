(ns geojson.test.geometry
  (:require [clojure.test :refer :all]
            [geojson.geometry :refer :all]))

(deftest test-centroid
  (are [g] (nil? (centroid g))
       {:type "Point"}
       {:type "Polygon"}
       {:type "MultiPolygon"}
       {:type "Polygon" :coordinates []}
       {:type "MultiPolygon" :coordinates []})
  (let [geojson {:type "Point" :coordinates [-94.928583379114 44.45393043491]}
        expected (geojson :coordinates)
        result (centroid geojson)]
    (is (= expected result)))
  (let [geojson {:type "Feature" :geometry {:type "Point" :coordinates [-94.928583379114 44.45393043491]}}
        expected (get-in geojson [:geometry :coordinates])
        result (centroid geojson)]
    (is (= expected result)))
  (let [geojson {:type "Polygon" :coordinates [[[-83.503807481825 43.534605806937] [-83.503842495751 43.537707512412] [-83.5018536491 43.537738651455] [-83.501840863734 43.537223194231] [-83.500636098307 43.537212282045] [-83.500667100581 43.538168517512] [-83.500482138452 43.538171971044] [-83.500465330526 43.537765484549] [-83.496922661971 43.537784862769] [-83.496917405687 43.537671349767] [-83.495512812732 43.537703322279] [-83.495516669522 43.537806079372] [-83.492421709608 43.53786517692] [-83.492266881956 43.532954434518] [-83.494642399981 43.532902596434] [-83.494555864528 43.531075381507] [-83.496558182427 43.531056816196] [-83.496611023858 43.532337019529] [-83.49891929123 43.532313663274] [-83.498956015685 43.534688755173] [-83.503807481825 43.534605806937]]]}
        expected [-83.49816980387305 43.535737643896155]
        result (centroid geojson)]
    (is (= (format "%.7f" (first expected))
           (format "%.7f" (first result))))
    (is (= (format "%.7f" (second expected))
           (format "%.7f" (second result)))))
  (let [geojson {:type "MultiPolygon" :coordinates [[[[102.0 2.0] [103.0 2.0] [103.0 3.0] [102.0 3.0] [102.0 2.0]]]
                                                    [[[100.0 0.0] [101.0 0.0] [101.0 1.0] [100.0 1.0] [100.0 0.0]]
                                                     [[100.2 0.2] [100.8 0.2] [100.8 0.8] [100.2 0.8] [100.2 0.2]]]]}
        expected [101.5 1.5]
        result (centroid geojson)]
    (is (= (format "%.7f" (first expected))
           (format "%.7f" (first result))))
    (is (= (format "%.7f" (second expected))
           (format "%.7f" (second result))))))

(deftest test-acreage
  (let [geometry {:type "Polygon" :coordinates [[[0.01 0.01] [0.02 0.01] [0.02 0.02] [0.01 0.02] [0.01 0.01]]]}]
    (is (= 306 (Math/round (acreage geometry))))))

(deftest test-area
  (are [geom] (zero? (area geom))
       {:type "Feature" :geometry {:type "Point" :coordinates [100.0 0.0]}}
       {:type "Point" :coordinates [100.0 0.0]}
       {:type "MultiPoint" :coordinates [[100.0 0.0] [101.0 1.0]]}
       {:type "LineString" :coordinates [[100.0 0.0] [101.0 1.0]]}
       {:type "MultiLineString" :coordinates [[[100.0 0.0] [101.0 1.0]] [[102.0 2.0] [103.0 3.0]]]}))

(deftest test-clean
  (let [geometry {:type "Polygon" :coordinates [[[0.01 0.01] [0.02 0.01] [0.02 0.02] [0.01 0.02] [0.01 0.01]]]}]
    (is (= (clean geometry) geometry)))
  (let [geometry {:type "Point" :coordinates [1.0 2.0 3.0]}
        expected {:type "Point" :coordinates [1.0 2.0]}]
    (is (= (clean geometry) expected)))
  (let [geometry {:type "Feature" :geometry {:type "Point" :coordinates [1.0 2.0 3.0]}}
        expected {:type "Feature" :geometry {:type "Point" :coordinates [1.0 2.0]}}]
    (is (= (clean geometry) expected)))
  (let [geometry {:type "Polygon" :coordinates [[[1.0 2.0 3.0] [0.0 2.0 3.0] [1.0 1.0 3.0] [1.0 2.0 3.0]]]}
        expected {:type "Polygon" :coordinates [[[1.0 2.0] [0.0 2.0] [1.0 1.0] [1.0 2.0]]]}]
    (is (= (clean geometry) expected)))
  (let [geometry {:type "Polygon" :coordinates [[[1000.0 2000.0 3000.0]
                                                 [0.0 2000.0 3000.0]
                                                 [1000.0 1000.0 3000.0]
                                                 [1000.0 2000.0 3000.0]]]}
        expected {:type "Polygon" :coordinates [[[1000.0 2000.0]
                                                 [0.0 2000.0]
                                                 [1000.0 1000.0]
                                                 [1000.0 2000.0]]]}]
    (is (= (clean geometry) expected)))
  (let [geometry {:type "Polygon" :coordinates [[[1000.123456789634 2000.123456789634 3000.0]
                                                 [0.123456789634 2000.123456789634 3000.0]
                                                 [1000.123456789634 1000.123456789634 3000.0]
                                                 [1000.123456789634 2000.123456789634 3000.0]]]}
        expected {:type "Polygon" :coordinates [[[1000.12345679 2000.12345679]
                                                 [0.12345679 2000.12345679]
                                                 [1000.12345679 1000.12345679]
                                                 [1000.12345679 2000.12345679]]]}]
    (is (= (clean geometry) expected))))

(deftest test-inside?
  (let [triangle [[0 0] [0 1] [1 0]]
        square [[0 0] [0 1] [1 1] [1 0]]
        negative-square [[0 -1] [0 1] [1 1] [1 -1]]
        odd-shape [[0 0] [0 1] [0.5 0.7] [1 1] [1 0]]]
  (is (inside? [0.2 0.2] triangle))
  (is (not (inside? [0.2 1] triangle)))
  (is (inside? [0.5 0.5] square))
  (is (not (inside? [0.5 1.1] square)))
  (is (inside? [0.5 -0.5] negative-square))
  (is (not (inside? [-0.5 1] negative-square)))
  (is (inside? [0.5 0.6] odd-shape))
  (is (not (inside? [0.5 0.8] odd-shape)))))

(deftest test-bounding-box
  (let [f-1 [{"type" "Polygon" "coordinates" [[[-86.9032984972 40.4685311103941] [-86.9031429290771 40.468727000429] [-86.9031429290771 40.4685474345855] [-86.9033092260361 40.4687229193924] [-86.9034540653229 40.4685474345855] [-86.9034540653229 40.4687065952437] [-86.9032984972 40.4685311103941]]]} {"type" "Polygon" "coordinates" [[[-86.9034540653229 40.4685311103941] [-86.9034540653229 40.468727000429] [-86.9031429290771 40.468727000429] [-86.9031429290771 40.4685311103941] [-86.9034540653229 40.4685311103941]]]}]
        f-2 [{"type" "MultiPolygon" "coordinates" [[[[-83.3292103432114 42.4574693584808] [-83.3292925357819 42.4569997887664] [-83.3297592401505 42.457747805216] [-83.3293319157919 42.4577990889355] [-83.3292979001999 42.4577636361554] [-83.3292103432114 42.4574693584808]]] [[[-83.3293319157919 42.4577990889355] [-83.3296358585358 42.4581158735222] [-83.3290833234787 42.458195027714] [-83.3291487808023 42.4578210671899] [-83.3293319157919 42.4577990889355]]] [[[-83.3291487808023 42.4578210671899] [-83.3288135507552 42.4578612985583] [-83.3288151025772 42.457506382894] [-83.3281385147938 42.4578124065808] [-83.3278548717499 42.457423270076] [-83.3292013406754 42.4574391010974] [-83.3292103432114 42.4574693584808] [-83.3291487808023 42.4578210671899]]] [[[-83.3288135507552 42.4578612985583] [-83.3288097381592 42.4587332735648] [-83.3282255855509 42.4579318609825] [-83.3288135507552 42.4578612985583]]] [[[-83.3282255855509 42.4579318609825] [-83.3277475833893 42.4579892266072] [-83.3281385147938 42.4578124065808] [-83.3282255855509 42.4579318609825]]]]} {"type" "Polygon" "coordinates" [[[-83.3297592401505 42.4569997887664] [-83.3297592401505 42.4587332735648] [-83.3277475833893 42.4587332735648] [-83.3277475833893 42.4569997887664] [-83.3297592401505 42.4569997887664]]]}]
        f-3 [{"type" "Polygon" "coordinates" [[[-83.9654588699341 42.6405412694602] [-83.9630770683289 42.6403992074571] [-83.9637637138367 42.6409832380622] [-83.9654588699341 42.6405412694602]]]} {"type" "Polygon" "coordinates" [[[-83.9654588699341 42.6403992074571] [-83.9654588699341 42.6409832380622] [-83.9630770683289 42.6409832380622] [-83.9630770683289 42.6403992074571] [-83.9654588699341 42.6403992074571]]]}]]
    (are [original envelope] (= (:coordinates (bounding-box original)) (get envelope "coordinates"))
         (first f-1) (second f-1)
         (first f-2) (second f-2)
         (first f-3) (second f-3))))
