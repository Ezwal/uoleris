(ns uolueris.core
  (:import [java.net IDN])
  (:gen-class))

;; Punycode EXAMPLE : korean unicode url : 실례.테스트 παράδειγμα.δοκιμή правительство.рф

;; not till 1<<8 cos' there is no char used by puny-code charset (after 122 = \z)
(def byte-masks
  (map #(bit-shift-left 1 %) (range 7)))

(defn toggle-bit-mask [^Integer char-val]
  (fn [mask]
    (bit-and 0xff
             ((if (bit-test char-val mask)
               bit-clear
               bit-set) char-val mask))))

(defn bit-squatting [^Character c]
  (let* [char-val (int c)
        toggler (toggle-bit-mask char-val)]
    (map
     toggler
     byte-masks)))

(def allowed-reg #"[.\-A-Za-z0-9]")
(defn uniq-squatting-char [^Character c]
  (filter #(and
            (not= c %)
            (re-matches allowed-reg (str %)))
          (dedupe
           (map char
                (bit-squatting c)))))

(def mem-uniq-squatting-char (memoize uniq-squatting-char))

(defn replace-id-by-squat [^String s ^Integer n]
  (fn [squat]
    (let [sb (StringBuilder. s)
          squat-str (str squat)]
      (str (.replace sb n (inc n) squat-str)))))

(defn single-bit-squatting [^String name]
  (flatten (map
            (fn [idx]
              (map (replace-id-by-squat name idx)
                   (mem-uniq-squatting-char (get name idx)))) ;; TODO build a static bit squatting dict
            (range (count name)))))

(defn -main
  [& args]
  (single-bit-squatting (IDN/toASCII (first args))))
