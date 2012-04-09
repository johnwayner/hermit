(ns hermit.cpu
  (:use (hermit core disassembler)))

(def init-regs {
                :a 0,
                :b 0,
                :c 0,
                :x 0,
                :y 0,
                :z 0,
                :i 0,
                :j 0,
                :o 0,
                :sp 0,
                :pc 0})
(def init-mem {})

(def init-machine {:cycle 0
                   :regs init-regs
                   :mem init-mem})

(defn load-data
  "Returns new machine with the word-seq ws starting at memory location l."
  [m l ws] (assoc m :mem (reduce merge (:mem m) (map-indexed #(hash-map (+ l %1) %2) ws))))

(defn load-data-file
  "Loads data from file with file-name into memory m at location l."
  [m l file-name] (load-data m l (file-to-words file-name)))

(defn mem-val
  "Returns the value of memory location l in machine m."
  ([m l] (if-let [v (find (:mem m) l)]
           (val v)
           0))
  ([m l c] (map (partial mem-val m)
                (range l (+ l c)))))


(defn mem-set
  "Returns new memory for machine with location l set to v."
  [m l v] (assoc (:mem m) l v))

(defn machine-with-mem-set
  [m l v] (merge m {:mem (mem-set m l v)}))

(defn reg-val
  "Retruns the value of register-keyword rk (:a, :b, etc) in machine m."
  [m rk] (rk (:regs m)))

(defn reg-set
  "Returns new regs for machine with reg rk set to v."
  [m rk v] (assoc (:regs m) rk v))

(defn machine-with-reg-set
  [m rk v] (merge m {:regs (reg-set m rk v)}))

(defn machine-with-reg-delta
  [m rk d] (merge m {:regs (reg-set m rk (+ d (reg-val m rk)))}))

(defn reg-from-regop-kw
  "There must be a better way... :("
  [kw] (keyword (str (first (name kw)))))

;;stack stuff
(defn machine-with-stack-push
  [m v] (let [m1 (machine-with-reg-delta m :sp -1)]
          (machine-with-mem-set m1 (reg-val m1 :sp) v)))

(defn machine-with-stack-pop
  [m op-val] (let [m1 (set-lval-op-val m op-val (mem-val m (reg-val m :sp)))]
               (machine-with-reg-delta m1 :sp 1)))

(defn get-op-val-value
  [m op-val]
  (let [val (:val op-val)]
    (case val
      :ptr-nxt (mem-val m (:nxt op-val))
      :nxt (:nxt op-val)
      :lit (:literal op-val)
      (:a-ptr-nxt
       :b-ptr-nxt
       :c-ptr-nxt
       :x-ptr-nxt
       :y-ptr-nxt
       :z-ptr-nxt
       :i-ptr-nxt
       :j-ptr-nxt) (mem-val m (+ (reg-val m (reg-from-regop-kw val))
                                 (:nxt op-val))) 
       (:a-ptr
        :b-ptr
        :c-ptr
        :x-ptr
        :y-ptr
        :z-ptr
        :i-ptr
        :j-ptr) (mem-val m (reg-val m (reg-from-regop-kw val)))
        (:pop
         :peek) (mem-val m (reg-val m :sp))
         :push (mem-val m (- (reg-val m :sp) 1))
         (reg-val m val))))

(defn set-lval-op-val
  [m op-val new]
  (let [val (:val op-val)]
    (case val
      :ptr-nxt (machine-with-mem-set m (:nxt op-val) new)
      (:nxt :lit) m ;fail silently as per spec
      (:a-ptr-nxt
       :b-ptr-nxt
       :c-ptr-nxt
       :x-ptr-nxt
       :y-ptr-nxt
       :z-ptr-nxt
       :i-ptr-nxt
       :j-ptr-nxt) (machine-with-mem-set m (+ (reg-val m (reg-from-regop-kw val))
                                              (:nxt op-val)) new) 
       (:a-ptr
        :b-ptr
        :c-ptr
        :x-ptr
        :y-ptr
        :z-ptr
        :i-ptr
        :j-ptr) (machine-with-mem-set m (reg-val m (reg-from-regop-kw val)) new)
        :pop (machine-with-stack-pop m op-val)
        :peek (machine-with-mem-set m (reg-val m :sp) new)
         :push (machine-with-stack-push m new)
         (machine-with-reg-set m val new))))

(defn next-instr
  "Returns the next instruction to be executed."
  [m] (first (disassemble (mem-val m (reg-val m :pc) 3))))

(defn apply-func-to-ops
  [m i f]  (let [[op-res o-val] (f (get-op-val-value m (:a i))
                                   (get-op-val-value m (:b i)))
                 m1 (set-lval-op-val m (:a i) op-res)]
             (if o-val
               (machine-with-reg-set m1 :o o-val)
               m1)))

(defn apply-func-to-branch-ops
  [m i f]  (machine-with-reg-delta m :pc (if (f (get-op-val-value m (:a i))
                                                (get-op-val-value m (:b i)))
                                           0
                                           (instruction-size (next-instr m)))))

(defn non-overflow-op
  [f] (fn [a b] [(f a b) nil]))

(def ops-map {:set (fn [a b] [b nil])
              :add (fn [a b] [(+ a b) (if (> (+ a b) 0xFFFF) 1 0)])
              :sub (fn [a b] [(- a b) (if (< (- a b) 0) 1 0)])
              :mul (fn [a b] (let [p (* a b)] [(bit-and 0xFFFF p)
                                                (bit-and 0xFFFF (bit-shift-right p 16))]))  
              :div (fn [a b] (let [d (/ a b)] [(bit-and 0xFFFF d)
                                                (if (= 0 b)
                                                  0
                                                  (bit-and 0xFFFF (/ (bit-shift-left a 16) b)))]))
              :mod (non-overflow-op mod)
              :shl (fn [a b] [(bit-shift-left a b) (bit-and 0xFFFF
                                                           (bit-shift-right (bit-shift-left
                                                                             a
                                                                             16)
                                                                            b))])
              :shr (fn [a b] [(bit-shift-right a b) (bit-and 0xFFFF
                                                            (bit-shift-right
                                                             (bit-shift-left a 16) b))])
              :and (non-overflow-op bit-and)
              :bor (non-overflow-op bit-or)
              :xor (non-overflow-op bit-xor)})

(def branch-ops-map {:ife =
                     :ifn (comp not =)
                     :ifg >
                     :ifb #(> (bit-and %1 %2) 0)})


(defn step
  "Steps a machine one instruction."
  [m] (let [i (next-instr m)
            op (:op i)
            post-pc-inc-m (machine-with-reg-delta m :pc (instruction-size i))
            post-op-m (cond
                       (op ops-map) (apply-func-to-ops post-pc-inc-m i (op ops-map))
                       (op branch-ops-map) (apply-func-to-branch-ops post-pc-inc-m i (op branch-ops-map))
                       (= :jsr op) (let [m1 (machine-with-stack-push
                                              post-pc-inc-m
                                              (reg-val post-pc-inc-m :pc))]
                                     (machine-with-reg-set m1 :pc
                                       (get-op-val-value m1 (:a i))))
                       :default (do (println (str "INVALID OPCODE: "
                                                  op
                                                  " PC:0x"
                                                  (Integer/toHexString (reg-val m :pc))
                                                  " <0x"
                                                  (Integer/toHexString (:w i))
                                                  ">"))
                                    m)
                       )]
        (case (:val (:b i))
          ;; fix sp when stack used as val
          :pop (machine-with-reg-delta post-op-m :sp 1)
          :push (machine-with-reg-delta post-op-m :sp -1)
          post-op-m)))