(ns hermit.ui
  (:use (hermit core disassembler cpu)
        (seesaw core font graphics chooser dev))
  (:import [java.util.concurrent Executors])
  (:gen-class))

(def video-ram-loc 0x8000)
(def video-display-width 32)
(def video-display-height 16)

(def cpu (atom init-machine))
(def run-future nil)

(defn printable-char?
  [c]
  (if (and (> (int c) 32)
	   (< (int c) 127))
    (char c)))

(defn display-video
  [f m]
  (config!
   (select f [:#txtvideo]) :text
   (str (apply str
               (interleave (map #(reduce (fn [a b] (if (printable-char? (bit-and 0xFF b))
                                                    (str a (char  (bit-and 0xFF b))) 
                                                    (str a " ")))
                                         ""
                                         %) 
                                (partition video-display-width
                                           (mem-val m
                                                    video-ram-loc
                                                    (* video-display-height
                                                       video-display-width))))
                           (repeat "\n")))
        "[Click here to send keyboard input to cpu.]")))

(defn make-reg-panel
  [title id] (horizontal-panel :items [(label title) (label :text "0x0000"
                                                            :id id
                                                            :font "MONOSPACED-PLAIN-14")]))

(defn get-mem-display
  [value] (String/format "0x%04X" (to-array [(bit-and 0xFFFF value)])))

(defn get-reg-display
  [m rk] (get-mem-display (reg-val m rk)))

(defn display-reg
  [f m rk sel] (config! (select f [sel]) :text (get-reg-display m rk)))

(defn display-regs
  [f m] (doall (map (partial display-reg f m)
                    [:a :b :c :x :y :z :i :j :pc :sp :o]
                    [:#reg-a :#reg-b :#reg-c
                     :#reg-x :#reg-y :#reg-z
                     :#reg-i :#reg-j :#reg-pc
                     :#reg-sp :#reg-o])))



(defn display-mem
  [f m] (config! (select f [:#mem])
                 :text (reduce #(str %1
                                     (if (= %2 (reg-val m :pc))
                                       "=>"
                                       "  ")
                                     " "
                                     (get-mem-display %2) ": "
                                     (get-mem-display (mem-val m %2))
                                     "  '"
                                     (let [c (char (bit-and 0xFF (mem-val m %2)))]
                                       (if (printable-char? c)
                                         c
                                         " "))
                                     "'  "
                                     (instr-to-str (first (disassemble
                                                           (mem-val m %2 3)))
                                                   false)
                                     "\n")
                               ""
                               (sort (map #(if (< % 0) (+ 0xFFFF %) %) (keys (:mem m)))))))
(defn update-display
  [f m] (do
          (display-regs f m)
;          (display-mem f m)
          (display-video f m)))

(defn setup-ui
  "Creates the UI."
  [] (do
       (native!)
       (let [f (frame :title "Hermit DCPU-16 Emulator"
                      :width 1000 :height 1000
                      :on-close :dispose
                      :visible? true)
             load-button (button :text "Load" :id :load-button)
             run-button (button :text "Run")
             step-button (button :text "Step")
             reset-button (button :text "Reset")
             auto-refresh-button (button :text "Toggle Auto Refresh")
             refresh-button (button :text "Refresh")
             memory-text (scrollable (text :multi-line? true
                                           :editable? false
                                           :font "MONOSPACED-PLAIN-14"
                                           :id :mem))
             video-output (canvas :size [250 :by 250]
                                  :id :video)
             video-text (text :editable? false
                              :multi-line? true
                              :font "MONOSPACED-PLAIN-14"
                              :id :txtvideo)
             regs-first-col (vertical-panel :items [(make-reg-panel "A:" :reg-a)
                                                    (make-reg-panel "B:" :reg-b)
                                                    (make-reg-panel "C:" :reg-c)
                                                    (make-reg-panel "X:" :reg-x)
                                                    (make-reg-panel "Y:" :reg-y)
                                                    (make-reg-panel "Z:" :reg-z)])
             regs-second-col (vertical-panel :items [
                                                     (make-reg-panel " I:" :reg-i)
                                                     (make-reg-panel " J:" :reg-j)
                                                     (make-reg-panel "PC:" :reg-pc)
                                                     (make-reg-panel "SP:" :reg-sp)
                                                     (make-reg-panel " O:" :reg-o)])
             panel (border-panel :north (horizontal-panel :items
                                                          [load-button run-button
                                                           step-button reset-button
                                                           auto-refresh-button
                                                           refresh-button])
                                 :west video-text
                                 :center memory-text
                                 :south (horizontal-panel :items
                                                          [regs-first-col
                                                           regs-second-col])
                                 :vgap 5 :hgap 5 :border 5)]
         (config! f :content panel :size [750 :by 500])
         (listen load-button :action (fn [x] (if-let [file (choose-file f)]
                                              (swap! cpu (fn [m] (load-data-file m 0 file))))))
         (listen step-button :action (fn [x] (swap! cpu step)))
         (listen reset-button :action (fn [x] (swap! cpu (fn [_] init-machine))))
         (listen run-button :action (fn [x] (if run-future
                                             (do (future-cancel run-future)
                                                 (def run-future nil)
                                                 (config! run-button :text "Run"))
                                             (do (def run-future
                                                   (future (loop [] 
                                                                 (swap! cpu step)
                                                                 (recur))))
                                                 (config! run-button :text "Stop")))))
         (listen refresh-button :action (fn [x] (do (update-display f @cpu)
                                                   (display-mem f @cpu))))
         (add-watch cpu :disp (fn [_ _ _ m] (update-display f m)))
         (listen (select f [:#txtvideo])
                 :key-typed (fn [e] (swap! cpu #(machine-with-key-input
                                                 %
                                                 (int (.getKeyChar e))))))
         (seesaw.dev/debug!) 
         (-> f show!)
         f)))

(defn -main
  [] (setup-ui))

