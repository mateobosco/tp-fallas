;; ===== Defino funciones =====
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

(defn String->Number [str]
  (let [n (read-string str)]
    (if (number? n) n nil)))

(defn ask [preguntas]
  (doall (map (fn [par]
                (println " *"(par 0))
                (doall (map (fn [opt]
                              (println "  " (.indexOf (par 1) opt) "." opt)) (par 1)))
                (String->Number (read-line))) preguntas)))

;; =========== Datos =========
(def reglas [
             [ 2, 2, 2, 1, 1, 0, 1, 0, 0, 2, 1 ],
             [ 3, 1, 2, 0, 0, 1, 1, 2, 1, 2, 2 ],
             [ 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1 ],
             [ 0, 1, 2, 1, 0, 3, 0, 3, 1, 1, 1 ],
             [ 3, 3, 2, 2, 0, 2, 1, 0, 2, 9, 2 ],
             [ 2, 2, 1, 2, 0, 2, 1, 1, 1, 1, 1 ],
             [ 0, 4, 0, 2, 0, 0, 1, 0, 1, 0, 0 ],
             [ 0, 0, 0, 3, 0, 0, 1, 0, 0, 0, 0 ] ] )

(def nombres_tabla [ "Zapatos" "Campera" "Remera" "Zapatillas" "Perfume" "Accesorios" "Bombones" "Libro"])
(def preguntas [
                  [ "Cual es la ocacion del regalo?",
                   [ "Otra", "D`ia de los enamorados", "Aniversario", "Cumplea~nos" ] ],
                  [ "Cual es el estado de la relaci`on?",
                   [ "Peleado", "Complicado", "Relaci`on Abierta", "Estable", "Comprometidi", "Casado"] ],
                  [ "Cual es la largo de la relaci`on?",
                   [ "Corta", "Mediana", "Larga"] ],
                  [ "Cual es su forma de ser?",
                   [ "Extrovertida", "Sociable", "Poco Sociable", "Introvertida"] ],
                  [ "Como es la formalidad de su trabajo?",
                   [ "Informal", "Formal"] ],
                  [ "Como es su trabajo?",
                   [ "No es activo", "Poco Activo", "Activo", "Muy Activo"] ],
                  [ "Cual es su edad?",
                   [ "Adulta", "Joven"] ],
                  [ "Que tan activa es ella?",
                   [ "Sedentaria", "Poco Activa", "Activa", "Deportista"] ],
                  [ "Cual es su altura?",
                   [ "Baja", "Normal", "Alta"] ],
                  [ "Que tanto sabes acerca de su talle?",
                   [ "Desconocido", "Aproximado", "Conocido"] ],
                  [ "Cual es el costo del regalo esperado?",
                   [ "Bajo", "Medio", "Alto"] ] ] )

;; ===========================

; (println "El regalo es:" (nombres_tabla (index_min_distance reglas (ask preguntas))))




(require '[clojure.string :as str])
(println(str/split  "Clojure is a basura!" #" "))




(use 'clojure.java.io)

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

(def valores (map get-first-value (read-file "input.csv")))


(def minimos (map get-first-value (read-file "referencias.csv")))

(def maximos (map get-second-value (read-file "referencias.csv")))


(defn apply-range [min-v max-v value] 
	( cond  
		(< value min-v) -1
		(> value max-v) 1
		:else 0)
)

(def result (map apply-range minimos maximos valores))

(println result)

(defn reference-max [value]
  ( if (= value 1) 1 0
  )
)

(defn reference-min [value]
  ( if (= value -1) 1 0
  )
)

(def ref-maximos (map reference-max result))

(def ref-minimos (map reference-min result))


(defn create-vector [v1 v2]
  (vector v1 v2)
  )

(println ref-maximos)
(println ref-minimos)


(println (mapcat create-vector ref-maximos ref-minimos))

