(ns pan-jaro.main
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clj-fuzzy.jaro-winkler :refer [jaro-winkler]])
  (:import (org.apache.commons.text.similarity JaroWinklerDistance)))

(def jw-distance (JaroWinklerDistance.))


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
       (pmap (fn [s] [s (.apply jw-distance słowo s)]))
       (filter #(> (second %) minimalne-dopasowanie))
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
    (let [dopasowania (znajdź-podobne-słowa-java słowo słownik minimalne-dopasowanie)]
      (spit plik (str słowo ":\n") :append true)
      (spit plik (with-out-str (pprint dopasowania)) :append true))))

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

  (time (zapisz-dopasowania-słów zmodyfikowane-słowa istotne-słowa 0.85 "dopasowania.txt"))

  )
