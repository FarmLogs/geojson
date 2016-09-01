# GeoJSON

Clojure utilities for using [GeoJSON](http://geojson.org).

## Installation

Add the following dependency to your `project.clj`:

    [com.farmlogs/geojson "1.0.0"]

## Usage

This library's utilities act on clojure maps structured as GeoJSON.

The validation namespace verifies that a given GeoJSON is valid, following the
details of the geojson [spec](http://geojson.org/geojson-spec.html).

    (require '[geojson.validate :as gv])
    
    (gv/valid? {:type "Point" :coordinates [0 0]})
    (gv/valid? {:type "Feature"
                :properties {:name "Null Island"}
                :geometry {:type "Point" :coordinates [0 0]}})
    (gv/valid? {:type "GeometryCollection"
                :geometries [{:type "Point" :coordinates [0 0]}
                             {:type "Polygon" :coordinates [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}
                             {:type "LineString" :coordinates [[0 0] [1 1] [1 2]]}]})
