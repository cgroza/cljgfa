(ns cljgfa
  (:require [clojure.string]
            [clojure.java.io :as io
              BufferedWriter
              BufferedReader
              OutputStreamWriter
              InputStreamReader
              FileOutputStream
              FileInputStream])
  )

(defrecord refs [^String sid ^String strand])

(defrecord ogroup [^String opt_id refs tags])
(defrecord ugroup [^String opt_id ids tags])

(defrecord gap [^String opt_id ref1 ref2 dist var tags])

(defrecord header [^String version tags])

(defrecord segment [^String sid ^int len ^String seq tags])

(defrecord edge [^String eid ^refs sid1 ^refs sid2
               beg1 end1 beg2 end2
               alignment tags])

(defrecord fragment [^String eid ^refs sid1
                 beg1 end1 beg2 end2
                 alignment tags])

(defn some-tags [s]
  (doall (map (fn [x]
         (let [[match tag type value] (re-matches #"([A-Za-z0-9][A-Za-z0-9]):([ABHJZif]):([ -~]*)" x)]
           {:tag tag :type type :value value}
           ))
       s)))

(defn some-alignment [s]
  (cond
    (re-find #"[0-9]+,?$" s) {:trace (map #(Integer/parseInt %) (clojure.string/split s #","))}
    (re-find #"([0-9]+[MDIP])+" s) {:cigar s}
    (re-find #"\*" s) nil
    )
  )

(defn some-pos [^String s]
  {:pos (Integer/parseInt (clojure.string/replace s "$" ""))
   :end (clojure.string/ends-with? s "$")})

(defn some-refs [^String s]
  (let [strand (dec (count s))]
    (refs. (subs s 0 strand) (subs s strand (count s)))))

(defn some-edge [s]
  (edge. (s 0) (some-refs (s 1)) (some-refs (s 2))
         (some-pos (s 3))
         (some-pos (s 4))
         (some-pos (s 5))
         (some-pos (s 6))
         (some-alignment (s 7))
         (some-tags (drop 8 s))
         ))

(defn some-fragment [s]
  (fragment. (s 0) (some-refs (s 1))
         (some-pos (s 2))
         (some-pos (s 3))
         (some-pos (s 4))
         (some-pos (s 5))
         (some-alignment (s 6))
         (some-tags (drop 7 s))))

(defn some-segment [s]
  (segment. (s 0) (Integer/parseInt (s 1)) (s 2) (some-tags (drop 3 s))))

(defn some-header [s]
  (condp = (count s)
    0 (header. "" [])
    (if (= (s 0) "VN:Z:2.0")
      (header. (s 0) (some-tags (drop 1 s)))      ;version number, no tags
      (header. nil (some-tags s))                 ;no version number, tags
      )))

(defn some-o-group [s]
  (ogroup. (s 0) (vec (map some-refs (clojure.string/split (s 1) #" "))) (some-tags (drop 2 s)))
  )

(defn some-u-group [s]
  (ugroup. (s 0) (vec (clojure.string/split (s 1) #" ")) (some-tags (drop 2 s)))
  )

(defn some-gap [s]
  (let [var (if (= (s 4) "*") nil (Integer/parseInt (s 4)))]
    (gap. (s 0) (s 1) (s 2) (Integer/parseInt (s 3)) var (some-tags (drop 5 s)))
    )
  )

(defn some-line [^String s]
  (let [fields (clojure.string/split s #"\t")]
    ((case (get s 0)
       \H some-header
       \S some-segment
       \F some-fragment
       \E some-edge
       \O some-o-group
       \U some-u-group
       \G some-gap) (vec (drop 1 fields)))))

(defn parse-gfa2 [in]
  (map some-line (line-seq in)))
