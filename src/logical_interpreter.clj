(ns logical-interpreter)

(use '[clojure.string :only (split trim includes?)])

(defrecord t-elem [name args])
(defrecord t-db [facts rules])


(defn split-db 
  [database]
  (split (trim database) #"[\.|\t|\n|\r]+")
)


(defn split-elem
    "Divide el elemento y entrega un vector con cada parte"
    [elem]
    (split elem #"[\(|\)|\.|\s]+")
)

(defn split-rule
    "Divide el elemento y entrega un vector con cada parte"
    [rule]
    (split rule #"[:-]+")
)


(defn set-vars-order
  ""
  [vars args]
   (into [] (for [v args] (.indexOf vars v)))
)



(defn make-vector-args
    "Genera un vector de elementos (t-elem) que contiene nombre de la fact contenída y número de la variable que se debería cargar"
  [vars args]
  (into [] (for [fact args]
     (->t-elem (first fact) (set-vars-order vars (split (trim(first (rest fact)))  #"[,| ]+")) ) 
  ))
)


(defn gen-rule
    "Se encarga de devolver un elemento (t-elem) que contiene una regla, los argumentos (:args) es un vector con los facts contenidos"
    [str-rule]
    (let [vec-rule  (split-rule str-rule)
          vec-title (split (trim (first vec-rule)) #"[\(\)]+")
          vec-vars  (split (trim (first (rest vec-title))) #"[,\s]+")
          vec-facts (split (trim (first (rest vec-rule))) #"(\),)( )*")
          vec-args  (for [fact vec-facts]  (split fact #"[\)|\(|]+") )
         ]
       
        (if (= (count vec-rule) 2) 
            (->t-elem (first vec-title) (make-vector-args vec-vars vec-args))
        )
    )
)

(defn gen-fact 
    "A partir de un string, genera un elemento (t-elem) de tipo Fact"
    [str-fact]
    (let [vec-fact  (split (trim str-fact) #"[\(\)]+")
          vec-values (split (trim (first (rest vec-fact))) #"[,\s]+")
         ]
        (if (> (count vec-fact) 1)
             (->t-elem (first vec-fact)  (conj [] (vec vec-values)))
        )
    )
)


(defn push-new 
    "Agrega un nuevo elemento al vector"
  [vec value]
  (if (some #{value} vec)
       vector
       (conj vec value)
  )
)


(defn add-new-elem
    "Agrega un nuevo elemento al vector y swapea al atom del vector"
  [elem atom-vector]
   (swap! atom-vector push-new elem)
)


(defn is-rule? 
    "Define si el elemento es una regla "
    [elem]
   (includes? elem ":-")
)


(defn is-fact? 
    "Define si tiene el caracter de fact"
    [elem]
    (let [splitted (split-elem elem)]
      (and (includes? elem "(") (includes? elem ")") (> (count splitted) 1)) 
    )
  )

(defn get-pos-fact-in-vector
  "devuelve la posición en el vector de facts en la que está la que se pasa por parámetro, sino -1"
  [fact atom-facts]
  (.indexOf (map (fn [x] (:name @x)) @atom-facts) (:name fact))
)

(defn exist-fact?
  "Determina si la fact existe en el vector de facts"
  [fact atom-facts]
    (>= (get-pos-fact-in-vector fact atom-facts) 0)
)


(defn exist-rule?
  "Determina si la rule pasada por parámetro existe en el vector de rules"
  [rule atom-rules]
   (>= (.indexOf (map (fn [x] (:name @x)) @atom-rules) (:name rule)) 0)            
)



(defn add-arg-to-fact
  "Encola un nuevo valor a una fact ya existente"
  [fact atom-facts]
  (let [pos-fact-in-facts (get-pos-fact-in-vector fact atom-facts)
        args (:args fact)
        fact-to-update (nth @atom-facts pos-fact-in-facts)
        vector-args (:args @fact-to-update)
        new-fact (->t-elem (:name fact) (into (:args fact) vector-args)) 
       ]
      (reset! fact-to-update new-fact)
  )
)


(defn add-fact 
  "Determina si la fact existe o no en el vector de fats, si no existe genera el nuevo fact y lo encola, 
  sino agrega sus valores al fact existente"
  [fact atom-facts]
  (if (exist-fact? fact atom-facts)
      (add-arg-to-fact fact atom-facts)
      (add-new-elem (atom fact) atom-facts)
  )
)



(defn charge-rule-or-fact 
    "Determina y carga una regla o un fact, sino devuelve nil"
    [elem database]
    (cond
        (is-rule? elem) (add-new-elem (atom (gen-rule elem)) (:rules @database))
        (is-fact? elem) (add-fact (gen-fact elem) (:facts @database))
    )
)


(defn charge-db 
  [str-database atom-database]
  (let [splited-db (split-db str-database)]
      (for [elem splited-db] (charge-rule-or-fact elem atom-database))  
  )
)


(defn get-args-of
    "Devuelve la posición en el vector de variables, de la variable pasada"
    [name atom-facts]
    (:args @(get @atom-facts (.indexOf (map (fn [x] (:name @x)) @atom-facts) name)))
)



(defn evaluate-fact
  [fact atom-facts]
  (let [vector-args (get-args-of (:name fact) atom-facts)
        arg-fact  (first (:args fact))
       ]
     (if (true? (some true? (for [arg vector-args] (= arg arg-fact)))) (boolean true) (boolean false)) 
  )
)



(defn set-args-in-order
    "reemplaza el valor del argumento del fact por la posición según el orden de las variables "
    [vector-args vector-positions]
    (remove nil? (for [pos vector-positions] (get vector-args pos)))
)


(defn gen-evaluate-elem
  ""
  [name args-vector]
  (->t-elem name  (conj [] (vec args-vector)))  
)

(defn search-arg-in-facts
    ""
    [name arg-fact vector-args]
    (some true? (for [arg vector-args] (= arg arg-fact)))
)



(defn evaluate-rule
  ""
  [rule atom-database]
  (let [atom-rules (:rules @atom-database)
        atom-facts (:facts @atom-database)
        vector-atom-facts (get-args-of (:name rule) atom-rules)
        vector-pos-args  (first (:args rule))
        vector-facts (for [fact vector-atom-facts] 
              (gen-evaluate-elem (:name fact) (set-args-in-order  vector-pos-args (:args fact))))
       ]    
     (every? true? 
 
        (for [fact vector-atom-facts] 
          (evaluate-fact (gen-evaluate-elem (:name fact) 
                         (set-args-in-order  vector-pos-args (:args fact))) atom-facts )
        )
     )
  )
)



(defn validate-query 
  ""
  [query]
  (if (is-fact? query)
      (gen-fact query)
  )
)






(defn evaluate-query
  ""
  [str-database query]
  (let [atom-rules (atom [] )
        atom-facts (atom [] )
        atom-database (atom (->t-db atom-facts atom-rules))
        charged-db (charge-db str-database atom-database)
        format-query (validate-query query)
        ]
      (if (not (nil? format-query))
          (if (not (some nil? charged-db))
            (cond 
              (exist-rule? format-query (:rules @atom-database)) (evaluate-rule format-query atom-database)
              (exist-fact? format-query (:facts @atom-database)) (evaluate-fact format-query (:facts @atom-database))
            
            )
          )
      )
  )
)







