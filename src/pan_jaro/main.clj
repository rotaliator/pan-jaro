(ns pan-jaro.main
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.core.reducers :as r]
            [clj-fuzzy.jaro-winkler :refer [jaro-winkler]])
  (:import (org.apache.commons.text.similarity JaroWinklerDistance)))

(set! *warn-on-reflection* true)

(def jw-distance ^JaroWinklerDistance (JaroWinklerDistance.))


(def istotne-słowa (as-> "pan-tadeusz1.txt" $
                     (slurp $)                                     ;; odczyt z pliku
                     (str/split $ #"[^A-Za-zżółćęśąźńŻÓŁĆĘŚĄŹŃ]+") ;; dzielę na wyrazy
                     (filter #(> (count %) 2) $)))                 ;; tylko dłuższe niż dwa znaki

(def wybrane-słowa (repeatedly 30 #(rand-nth istotne-słowa)))

(defn zapisz-słowa [lista-słów plik]
  (doseq [słowo lista-słów]
    (spit plik słowo :append true)
    (spit plik "\n" :append true)))

(defn odczytaj-słowa [plik]
  (-> plik slurp str/split-lines))

(defn znajdź-podobne-słowa [słowo słownik minimalne-dopasowanie]
  (->> słownik
       (pmap (fn [s] [s (jaro-winkler słowo s)]))
       (filter #(> (second %) minimalne-dopasowanie))
       (sort-by second)
       (distinct)
       (reverse)))

(defn znajdź-podobne-słowa-java [słowo słownik minimalne-dopasowanie]
  (->> słownik
       (map (fn [s] [s (.apply ^JaroWinklerDistance jw-distance ^String słowo ^String s)]))
       (filter #(> (second %) minimalne-dopasowanie))
       (sort-by second)
       (distinct)
       (reverse)))

(defn znajdź-podobne-słowa-java-reducers
  "JaroWinkler java + reducers"
  [słowo słownik minimalne-dopasowanie]
  (->> słownik
       (r/map (fn [s] [s (.apply ^JaroWinklerDistance jw-distance ^String słowo ^String s)]))
       (r/filter #(> (second %) minimalne-dopasowanie))
       (into [])
       (sort-by second)
       (distinct)
       (reverse)))

(defn znajdź-podobne-słowa-xf
  "To samo co powyżej tylko z użyciem transducera"
  [słowo słownik minimalne-dopasowanie]
  (let [xf          (comp
                     (map (fn [s] [s (jaro-winkler słowo s)]))
                     (filter #(> (second %) minimalne-dopasowanie))
                     (distinct))
        dopasowania (into [] xf słownik)]
    (->> dopasowania
        (sort-by second)
        (reverse))))

(defn zapisz-dopasowania-słów [słowa słownik minimalne-dopasowanie plik]
  (doseq [słowo słowa]
    (let [dopasowania (znajdź-podobne-słowa-java-reducers słowo słownik minimalne-dopasowanie)]
      (spit plik (str słowo ":\n") :append true)
      (spit plik (with-out-str (pprint dopasowania)) :append true))))

(defn zapisz-dopasowania-słów-reducers
  "Wersja z użyciem reducera w głównej pętli"
  [słowa słownik minimalne-dopasowanie plik]
  (let [znajdź-dopasowania
        (fn [słownik minimalne-dopasowanie słowo]
          (znajdź-podobne-słowa-java-reducers słowo słownik minimalne-dopasowanie))
        zapisz-dopasowania!
        (fn [plik słowo dopasowania]
          (spit plik (str słowo ":\n") :append true)
          (spit plik (with-out-str (pprint dopasowania)) :append true)
                             )
        dopasowania (->> słowa
                         (r/map (partial znajdź-dopasowania słownik minimalne-dopasowanie))
                         (into []))]
    (mapv #(zapisz-dopasowania! plik %1 %2) słowa dopasowania)
    nil))

(comment

  (zapisz-słowa wybrane-słowa "corrections.txt")
  ;; po zapisie ręcznie modyfikuję słowa
  (def zmodyfikowane-słowa (odczytaj-słowa "corrections.txt"))

  (znajdź-podobne-słowa "mrówki" istotne-słowa 0.85)
  ;; => (["mrówek" 0.9333333333333333]
  ;;     ["mówi" 0.9]
  ;;     ["Mrówki" 0.888888888888889]
  ;;     ["mruki" 0.8577777777777779]
  ;;     ["mów" 0.8500000000000001])
  (time (znajdź-podobne-słowa "mrówki" istotne-słowa 0.85))
  (time (znajdź-podobne-słowa-xf "mrówki" istotne-słowa 0.85))
  (time (znajdź-podobne-słowa-java "mrówki" istotne-słowa 0.85))
  (time (znajdź-podobne-słowa-java-reducers "mrówki" istotne-słowa 0.85))

  (time (zapisz-dopasowania-słów zmodyfikowane-słowa istotne-słowa 0.85 "dopasowania.txt"))


  (def wybrane-słowa (repeatedly 2000 #(rand-nth istotne-słowa)))
  (count wybrane-słowa)

  (time (zapisz-dopasowania-słów wybrane-słowa istotne-słowa 0.85 "dopasowania-jav-red-wew-2000.txt"))


  )
