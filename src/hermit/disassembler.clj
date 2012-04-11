(ns hermit.disassembler
  (:use (hermit core)))

(def ops [:ext
          :set
          :add :sub :mul :div :mod
          :shl :shr
          :and :bor :xor
          :ife :ifn :ifg :ifb])
(def op-props {:ext {:cost 0}
               :set {:cost 1}
               :add {:cost 2}
               :sub {:cost 2}
               :mul {:cost 2}
               :div {:cost 3}
               :mod {:cost 3}               
               :shl {:cost 2}
               :shr {:cost 2}
               :and {:cost 1}
               :bor {:cost 1}
               :xor {:cost 1}
               :ife {:cost 2}
               :ifn {:cost 2}
               :ifg {:cost 2}
               :ifb {:cost 2}

               :jsr {:cost 2}})
               
(def ops-exts [:ext :jsr])

(def op-vals (into [:a :b :c :x :y :z :i :j

                    :a-ptr :b-ptr :c-ptr :x-ptr :y-ptr :z-ptr :i-ptr :j-ptr

                    :a-ptr-nxt :b-ptr-nxt :c-ptr-nxt
                    :x-ptr-nxt :y-ptr-nxt :z-ptr-nxt
                    :i-ptr-nxt :j-ptr-nxt

                    :pop :peek :push

                    :sp
                    :pc
                    :o

                    :ptr-nxt
                    :nxt]
                   (repeat 0x20 :literal-20)))


(defn op
  "Get operator from word."
  [w] (let [op (ops (bit-and 0xF w))]
        (if (= :ext op)
          (let [ext-idx (bit-shift-right (bit-and 2r0000001111110000 w) 4)]
            (if (< ext-idx (count ops-exts))
              (ops-exts ext-idx)
              :unk))
          op)))

(defn op-val-reads-next
  "Returns true if the op-val requires a read of the next word; nil otherwise."
  [{val :val}]
  (case val
    (:a-ptr-nxt
     :b-ptr-nxt
     :c-ptr-nxt
     :x-ptr-nxt
     :y-ptr-nxt
     :z-ptr-nxt
     :i-ptr-nxt
     :j-ptr-nxt
     :ptr-nxt
     :nxt) true
     false))

(defn op-val-needs-next
  "Returns true if op-val v reads next, but doesn't have one yet."
  [v] (and (op-val-reads-next v) (not (:nxt v))))

(defn op-val
  "Get the value info for w at offset."
  [w offset] (let [idx (bit-and 0x3f (bit-shift-right w (- 10 offset)))
                   val (op-vals idx)]
               (if (= val :literal-20)
                 {:val :lit :literal (- idx 0x20)}
                 {:val val})))

(defn instruction
  "Returns a map of the instruction defined by w with :op :a :b"
  [w] (let [o (op w)
            a (if (= :unk o)
                {:val :lit :literal w}
                (op-val w  (if (= :jsr o) 0 6)))
            b (if (or (= :unk o) (= :jsr o)) nil (op-val w 0))]
        {:op o, :a a, :b b, :w w}))

(defn instruction-size
  "Returns the number of words this instruction consumes (1-3)."
  [i] (reduce #(+ %1 (if %2 1 0)) 1
              [(op-val-reads-next (:a i))
               (op-val-reads-next (:b i))]))

(defn instr-needs-next
  "Returns true if instruction i has a val that reads next word."
  [i] (or (op-val-needs-next (:a i))
          (op-val-needs-next (:b i))))

(defn instr-read-next
  "Returns new instruction with w read into i (a before b)."
  [i w] (if (op-val-needs-next (:a i))
          (assoc i :a (assoc (:a i) :nxt w))
          (if (op-val-needs-next (:b i))
            (assoc i :b (assoc (:b i) :nxt w))
            i)))

(defn instr-cycle-count
  "Returns the number of cycles an instruction will take.
Note: Assumes branches succeed."
  [i] (let [{cost :cost} ((:op i) op-props)]
        (reduce + cost  (map #(if (op-val-needs-next (% i))
                                1
                                0)
                             [:a :b]))))
          
(defn op-val-key-to-str
  ""
  [vk] (last (re-find #"^:(.)" (str vk))))

(defn op-val-to-str
  "Returns a string representation of op-val v."
  [v] (let [val (:val v)]
        (cond
         (= :ptr-nxt val) (str "[0x" (Integer/toHexString (:nxt v)) "]")
         (= :nxt val) (str "0x" (Integer/toHexString (:nxt v)))
         (= :lit val) (str "0x" (Integer/toHexString (:literal v)))
         (re-find #":.-ptr-nxt" (str val)) (str "[0x"
                                                (if (:nxt v)
                                                  (Integer/toHexString (:nxt v))
                                                  "????")
                                                 "+"
                                                 (op-val-key-to-str val)
                                                 "]")
         (re-find #":.-ptr" (str val)) (str "["
                                            (op-val-key-to-str val)
                                            "]")

         :default (name val))))


(defn instr-to-str
  "Returns a string representation of instruction i (and comment mem if c)."
  ([i c] (str (name (:op i))
               " "
               (op-val-to-str (:a i))
               (let [b (:b i)]
                 (if b (str ", " (op-val-to-str b))))
               (if c (str "  ; "
                          (reduce #(str % (if %2 (str "0x" (Integer/toHexString %2) " ")))
                                  ""
                                  [(:w i)
                                   (:nxt (:a i))
                                   (:nxt (:b i))])))))
  ([i] (instr-to-str i true)))

(defn disassemble
  "Returns decoded instructions for each word in ws."
  [ws] (reduce #(let [last-i (last %)]
                  (if (instr-needs-next last-i)
                    (conj (vec (drop-last %))
                          (instr-read-next last-i %2))
                    (conj % (instruction %2))))
               []
               ws))

(def disassemble-memoized (memoize disassemble))

(defn disassemble-file
  "Slurp a file and diassemble it."
  [file-name] (disassemble (file-to-words file-name)))