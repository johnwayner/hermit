(ns hermit.assembler
  (:use (hermit core disassembler)))

(comment bit-range indexes are from least to most significant bit and zero indexed.)
(defn basic-op-props
  [val] {:offsets {:op 0
                   :a 4
                   :b 10}
         :val val})
(defn ext-op-props
  [val] {:offsets {:op 4
                   :a 10
                   :b 0}
         :val val})

(def op-map (merge {:jsr (ext-op-props 0x1)}
                   (apply merge (map #(hash-map % (basic-op-props %2))
                                     ops
                                     (iterate inc 0)))))

(defn encode-operand
  [{:keys [val literal]}]
  (if (= :literal-20 val)
    (+ literal 0x20)
    (let [idx (.indexOf op-vals val)]
      (if (< idx 0)
        0
        idx))))

(defn encode-iml
  "Renders the intermediate instruction format to an actual word.

   The iml format is the same outputted by hermit.disassembler/instruction."
  [iml]
  (let [omap ((:op iml) op-map)]
    (reduce #(bit-or %1
                     (bit-shift-left (case (%2 0)
                                       :op (:val omap)
                                       (:a
                                        :b) (encode-operand ((%2 0) iml)))
                                     (%2 1)))
            0x0000
            (:offsets omap))))

