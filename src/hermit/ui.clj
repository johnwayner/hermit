(ns hermit.ui
  (:use (hermit core disassembler cpu)
        (seesaw core font graphics chooser dev))
  (:import [java.util.concurrent Executors])
  (:gen-class))

(def cpu (atom init-machine))
(def run-future nil)

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

(defn printable-char?
  [c]
  (if (and (> (int c) 32)
	   (< (int c) 127))
    (char c)))

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
                                     (let [c (char (mem-val m %2))]
                                       (if (printable-char? c)
                                         c
                                         " "))
                                     "'  "
                                     (instr-to-str (first (disassemble
                                                           (mem-val m %2 3)))
                                                   false)
                                     "\n")
                               ""
                               (sort (keys (:mem m))))))
(defn update-display
  [f m] (do
          (display-regs f m)
          (display-mem f m)))

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
             memory-text (scrollable (text :multi-line? true
                                           :font "MONOSPACED-PLAIN-14"
                                           :id :mem))
             video-output (canvas :size [250 :by 250]
                                  :id :video)
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
                                                           step-button reset-button])
                                 :west video-output
                                 :center memory-text
                                 :south (horizontal-panel :items
                                                          [regs-first-col
                                                           regs-second-col])
                                 :vgap 5 :hgap 5 :border 5)]
         (config! f :content panel :size [750 :by 500])
         (listen load-button :action (fn [x] (if-let [file (choose-file f)]
                                              (do
                                                (swap! cpu (fn [m] (load-data-file m 0 file)))
                                                (update-display f @cpu)))))
         (listen step-button :action (fn [x] (do (swap! cpu step)
                                                (update-display f @cpu))))
         (listen reset-button :action (fn [x] (do (swap! cpu (fn [_] init-machine))
                                                 (update-display f @cpu))))
         (listen run-button :action (fn [x] (if run-future
                                             (do (future-cancel run-future)
                                                 (def run-future nil)
                                                 (config! run-button :text "Run"))
                                             (do (def run-future
                                                   (future (loop [] (Thread/sleep 10)
                                                                 (swap! cpu step)
                                                                 (update-display f @cpu)
                                                                 (recur))))
                                                 (config! run-button :text "Stop")))))
         (seesaw.dev/debug!) 
         (-> f show!)
         f)))

(defn -main
  [] (setup-ui))

