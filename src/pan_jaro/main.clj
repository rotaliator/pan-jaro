(ns pan-jaro.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-fuzzy.jaro-winkler :refer [jaro jaro-winkler]]))

(def istotne-słowa (as-> "pan-tadeusz1.txt" $
                     (slurp $)                                     ;; odczyt z pliku
                     (str/split $ #"[^A-Za-zżółćęśąźńŻÓŁĆĘŚĄŹŃ]+") ;; dzielę na wyrazy
                     (filter #(> (count %) 2) $)))                 ;; tylko dłuższe niż dwa znaki

(def wybrane-słowa (repeatedly 30 #(rand-nth istotne-słowa)))

(comment
  (jaro "MARTHA" "MARHTA")
  (jaro "DIXON" "DICKSONX")
  (jaro "JELLYFISH" "SMELLYFISH")

  (jaro-winkler "MARTHA" "MARHTA")
  (jaro-winkler "DIXON" "DICKSONX")
  (jaro-winkler "JELLYFISH" "SMELLYFISH")
  )
