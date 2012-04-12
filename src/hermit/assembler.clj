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


;;; Now code to convert lispy looking stuff to iml

(defn ify-operand
  [operand ify] (keyword (str (name operand) ify)))

(defn dasm-operand
  [{:keys [operand literal]} env]
  (let [resolved-literal (if-let [env-l (env literal)]
                           env-l
                           literal)]
    (cond
     (= :literal operand) (if (> resolved-literal 0x1f)
                            {:val :nxt :nxt resolved-literal}
                            {:val :literal-20 :literal (+ 0x20 resolved-literal)})
     (= :ptr operand) {:val :ptr :nxt resolved-literal}
     resolved-literal {:val (ify-operand operand "-nxt") :nxt resolved-literal}
     :default {:val operand})))

(defn prepare-operand
  [operand] (cond
             (map? operand) operand
             (keyword? operand) {:operand operand}
             (symbol? operand) {:operand (keyword operand)}
             :default {:operand :literal
                       :literal operand}))

(defn dasm-ptr
  [operand] (let [{p-operand :operand p-literal :literal} (prepare-operand operand)]
              (if (= p-operand :literal)
                {:operand :ptr :literal p-literal }
                {:operand (ify-operand p-operand "-ptr")
                 :literal p-literal})))

(defn dasm-op
  [op a b env] (let [asm-a (dasm-operand a env)
                     asm-b (dasm-operand b env)]
                 [{:op op :a asm-a :b asm-b}
                  (if-let [a-word (:nxt asm-a)]
                    a-word)
                  (if-let [b-word (:nxt asm-b)]
                    b-word)]))

(defmacro asm
  [&body] (for [[op a b] body]
            `(encode-iml (first (~(symbol (str "dasm-" op))
                                 ~(prepare-operand a)
                                 ~(prepare-operand b) {})))))