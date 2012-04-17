(ns hermit.core
  (:require (clojure.contrib (io :as io)))
  (:import [java.io FileOutputStream]))

(defn byte-pair-to-word [p]
                       (bit-or (bit-shift-left (bit-and 0xFF (second p)) 8)
                               (bit-and 0xFF (first p))))
(defn to-signed-byte
  "Takes a number from 0x00 to 0xFF and returns a signed byte with the same bit pattern for writing."
  [b] (byte (- (bit-and 0x7f b) (bit-and 0x80 b))))

(defn words-to-bytes
  [ws]
  (map to-signed-byte (flatten (map #(vector (bit-and % 0xFF)
                                             (bit-and 0xFF (bit-shift-right % 8)))
                                    ws))))

(defn bytes-to-words
  [bs]
  (->> bs (partition 2) (map byte-pair-to-word)))

(defn file-to-words
  "Returns a vector of words (2 bytes) read from file-name."
  [file-name] (map byte-pair-to-word
                   (partition 2 (io/to-byte-array (io/file file-name)))))

(defn write-bin-file [file byte-seq]
  (with-open [out (FileOutputStream. file)]
    (.write out (byte-array byte-seq))))

(defn words-to-file
  "Writes the given word sequence to the file named file-name."
  [file-name ws] (write-bin-file (io/file file-name)
                                 (words-to-bytes ws)))

;; read wants *in* set to a java.io.PushbackReader.
;; with-open sets *in* and closes it after it's done.
;; *read-eval* specifies whether to evaluate #=() forms
;; when reading.  This is a very good idea if you are
;; reading forms from untrusted sources.
(defn read-from-file-safely [filename]
  (with-open
    [r (java.io.PushbackReader.
         (clojure.java.io/reader filename))]
      (binding [*read-eval* false]
        (read r))))

