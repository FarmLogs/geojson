(ns geojson.validate)

(defn- valid-coordinates?
  "Given a sequence of coordinates, ensure that, for the given depth:
   1) they contain only sequences until
   2) they contain only numbers at depth 1"
  [depth coordinates]
  {:pre [(pos? depth)]}
  (if (= 1 depth)
    (and (every? number? coordinates)
         (<= 2 (count coordinates)))
    (and (not (nil? coordinates))
         (every? sequential? coordinates)
         (->> coordinates
              (map #(valid-coordinates? (dec depth) %))
              (every? true?)))))

(defn- linear-ring?
  "Checks to make sure that the given coordinates are valid linear rings, which
   is a requirement for Polygon types."
  [coordinates]
  (and (sequential? coordinates)
       (<= 4 (count coordinates))
       (= (first coordinates) (last coordinates))))

(defmulti valid? :type)

(defmethod valid? "Feature" [geometry]
  (and (or (valid? (:geometry geometry))
           (nil? (:geometry geometry)))
       (or (map? (:properties geometry))
           (nil? (:properties geometry)))))

(defmethod valid? "FeatureCollection" [geometry]
  (and (sequential? (:features geometry))
       (->> geometry
            :features
            (map valid?)
            (every? true?))))

(defmethod valid? "GeometryCollection" [geometry]
  (and (sequential? (:geometries geometry))
       (->> geometry
            :geometries
            (map valid?)
            (every? true?))))

(defmethod valid? "Point" [geometry]
  (valid-coordinates? 1 (:coordinates geometry)))

(defmethod valid? "LineString" [geometry]
  (valid-coordinates? 2 (:coordinates geometry)))

(defmethod valid? "Polygon" [geometry]
  (and (valid-coordinates? 3 (:coordinates geometry))
       (every? linear-ring? (:coordinates geometry))))

(defmethod valid? "MultiPoint" [geometry]
  (valid-coordinates? 2 (:coordinates geometry)))

(defmethod valid? "MultiLineString" [geometry]
  (valid-coordinates? 3 (:coordinates geometry)))

(defmethod valid? "MultiPolygon" [geometry]
  (and (valid-coordinates? 4 (:coordinates geometry))
       (every? #(every? linear-ring? %) (:coordinates geometry))))

(defmethod valid? :default [geometry] false)
