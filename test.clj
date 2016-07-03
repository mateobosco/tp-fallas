
; -----Defino variables-----

(def enfermedades ["Anemia" "Infeccion" "Tumores" "Leucemia" "Parasito" "Alergia" "Asma" "Virus" "Inflamacion" "Insuficiencia Respiratoria" "Enfermedad Medula Osea" "Patologia de Higado" "Deficit de Vitamina" "Hemorragia"  "Diabetes"  "Bien"])

(def tabla [
              [0 1 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 1 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 1 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 1]
              [0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0]
              [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 1 0 0 0 0 1 0 1 0 1 0 1 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                      ])

; ------Defino funciones------

(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [f]
  (-> (slurp f)
    (str/split #"\n")))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn get-first-value [line] 
	(parse-int (get (str/split line #",") 1))
)

(defn get-second-value [line] 
  (parse-int (get (str/split line #",") 2))
)

(defn apply-range [min-v max-v value] 
	( cond  
		(< value min-v) -1
		(> value max-v) 1
		:else 0)
)

(defn reference-max [value]
  ( if (= value 1) 1 0
  )
)

(defn reference-min [value]
  ( if (= value -1) 1 0
  )
)

(defn create-vector [v1 v2]
  (vector v1 v2)
  )

(defn abs [n]
  (max n (- n)))

(defn abs_dif [a b]
  (abs (- a b)))

(defn distance_vector [v1 v2]
  (map abs_dif v1 v2))

(defn distance [v1 v2]
  (apply + (distance_vector v1 v2)))

(defn distances [tabla v]
  (map (fn [c]
         (distance c v)) tabla))

(defn min_distance [tabla v]
  (apply min (distances tabla v)))

(defn index_min_distance [tabla v]
  (.indexOf (distances tabla v) (min_distance tabla v)))


; ------Ejecucion--------
(def valores (map get-first-value (read-file "input.csv")))
(def minimos (map get-first-value (read-file "referencias.csv")))
(def maximos (map get-second-value (read-file "referencias.csv")))

(def result (map apply-range minimos maximos valores))

(def ref-maximos (map reference-max result))
(def ref-minimos (map reference-min result))

(def vect (mapcat create-vector ref-maximos ref-minimos))

(println vect)
(println (enfermedades (index_min_distance tabla vect)))
