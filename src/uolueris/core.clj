(ns uolueris.core
  (:gen-class))

;; not till 1<<8 cos' there is not interesting char after 122 (\z)
(def byte-masks
  (map #(bit-shift-left 1 %) (range 7)))

(defn toggle-bit-mask [char-val]
  (fn [mask]
    (bit-and 255
             (if (bit-test char-val mask)
               (bit-clear char-val mask)
               (bit-set char-val mask)))))

(defn bit-squatting [c]
  (let* [char-val (int c)
        toggler (toggle-bit-mask char-val)]
    (map
     toggler
     byte-masks)))

(def allowed-reg #"[\.\-A-Za-z0-9]")
(defn uniq-bit-squatting-char [c]
  (filter #(and
            (not= c %)
            (re-matches allowed-reg (str %)))
          (dedupe
           (map char
                (bit-squatting c)))))

(defn replace-by-squat [s n]
  (fn [squat]
    (let [sb (StringBuilder. s)
          squat-str (str squat)]
      (str (.replace sb n (inc n) squat-str)))))

(defn single-bit-squatting [name]
  (flatten (map
            (fn [idx]
              (map (replace-by-squat name idx)
                   (uniq-bit-squatting-char (get name idx)))) ;; TODO build a static bit squatting dict
            (range (count name)))))

(defn -main
  [& args]
  (single-bit-squatting (first args)))
