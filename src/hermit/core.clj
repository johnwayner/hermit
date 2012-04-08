(ns hermit.core
  (:require (clojure.contrib (io :as io))))

(defn- byte-pair-to-word [p]
                       (bit-or (bit-shift-left (bit-and 0xFF (second p)) 8)
                               (bit-and 0xFF (first p))))


(defn file-to-words
  "Returns a vector of words (2 bytes) read from file-name"
  [file-name] (map byte-pair-to-word
                   (partition 2 (io/to-byte-array (io/file file-name)))))
