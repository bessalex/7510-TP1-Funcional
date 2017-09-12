(ns logical-interpreter)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  nil)





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


(defn get-pos-var-into-args
    "Devuelve la posición en el vector de variables, de la variable pasada"
    [act-var vec-args]
    [(.indexOf vec-args act-var)]
)

(defn set-vars-order
    "reemplaza el valor del argumento del fact por la posición según el orden de las variables "
    [vars args]
    (if (= (count vars) 1)
        (get-pos-var-into-args (first vars) args)
        (into (get-pos-var-into-args (first vars) args) (set-vars-order (rest vars) args))
    )
)


(defn make-vector-args
  [vars args]
;  (let vec-args )
  (into [] (for [fact args]
  ;(prn (split (first (rest fact))  #","))
     (->t-elem (first fact) (set-vars-order vars (split (trim(first (rest fact)))  #"[,| ]+")) ) 
   ; (prn (first fact) (gen-args vars (into [] (split-elem (first (rest fact))) ))) 
  ))
)


(defn get-fact-from-db 
  [name db]
  (get db (.indexOf (map (fn [x] (:name x)) db) name))
)

(defn get-args-in-order 
  [order-args args]
  (for [order order-args] ((fn [x y] (nth y x)) order args))
)


(defn gen-rule
    "A partir de un string, genera un elemento de tipo rule "
    [str-rule]
    (let [vec-rule  (split-rule str-rule)
          vec-title (split (trim (first vec-rule)) #"[\(\)]+")
          vec-vars  (split (trim (first (rest vec-title))) #"[,\s]+")
          vec-facts (split (trim (first (rest vec-rule))) #"(\),)( )*")
          vec-args  (for [fact vec-facts]  (split fact #"[\)|\(|]+") )
         ]
       
   ;     (prn vec-rule)
    ;    (prn vec-title)
   ;     (prn vec-facts)
   ;     (prn vec-rule)
   ;     (prn vec-args)
        (if (= (count vec-rule) 2) 
            (->t-elem (first vec-title) (make-vector-args vec-vars vec-args))
        )
    )
)

; Generate elements 
(defn gen-fact 
    "A partir de un string, genera un elemento de tipo Fact"
    [str-fact]
    (let [vec-fact  (split (trim str-fact) #"[\(\)]+")
          vec-values (split (trim (first (rest vec-fact))) #"[,\s]+")
          ;vec-fact (split-elem str-fact)
          ;vec-values (split (first (rest vec-fact)) #"[,]+")
         ]
        (if (> (count vec-fact) 1)
             (->t-elem (first vec-fact)  (conj [] (vec vec-values)))
        )
    )
)


(defn push-new [vec value]
  (if (some #{value} vec)
       vector
       (conj vec value)
  )
)


(defn add-new-elem
  [elem atom-vector]
   (swap! atom-vector push-new elem)
)


(defn is-rule? 
    "Define si tiene el caracter de regla"
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
  "Determina si la fact ya está en la lista de facts"
  [fact atom-facts]
  (.indexOf (map (fn [x] (:name @x)) @atom-facts) (:name fact))
)

(defn exist-fact?
  "Determina si la fact ya está en la lista de facts"
  [fact atom-facts]
    (>= (.indexOf (map (fn [x] (:name @x)) @atom-facts) (:name fact)) 0)
)


(defn exist-rule?
  "Determina si la fact ya está en la lista de facts"
  [rule atom-rules]
   (>= (.indexOf (map (fn [x] (:name @x)) @atom-rules) (:name rule)) 0)
;  (prn  (.indexOf (map (fn [x] (:name @x)) @atom-rules) (:name rule)))
 ;(prn  (map (fn [x] (:name @x)) @atom-rules))
             
)



(defn add-arg-to-fact
  ""
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
  ""
  [fact atom-facts]
  (if (exist-fact? fact atom-facts)
      (add-arg-to-fact fact atom-facts)
      (add-new-elem (atom fact) atom-facts)
  )
)



(defn charge-rule-or-fact 
    ""
    [elem database]
    (cond
        (is-rule? elem) (add-new-elem (atom (gen-rule elem)) (:rules @database))
        (is-fact? elem) (add-fact (gen-fact elem) (:facts @database))
    ;    :else (add-fact (gen-fact elem) (:facts @database))
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
  ""
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
;  (let [vector-args (get-args-of (:name fact) atom-facts)
;        arg-fact  (first (:args fact))
 ;      ]
     
;  )



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
     
  ;   (prn atom-rules)
  ;   (prn atom-facts)
 ;    (prn vector-facts)
  ;   (prn  vector-pos-args )
     
     (every? true? 
 
        (for [fact vector-atom-facts] 
          (evaluate-fact (gen-evaluate-elem (:name fact) 
                         (set-args-in-order  vector-pos-args (:args fact))) atom-facts )
        )
     )
  )
)



;; Testeo de carga base
;(def atom-rules (atom [] ))
;(def atom-facts (atom [] ))
;(def atom-database (atom (->t-db atom-facts atom-rules)))


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
;     (prn  charged-db)
      (if (not (nil? format-query))
          (if (not (some nil? charged-db))
 ;   (if (not-empty charged-db)
            (cond 
              (exist-rule? format-query (:rules @atom-database)) (evaluate-rule format-query atom-database)
              (exist-fact? format-query (:facts @atom-database)) (evaluate-fact format-query (:facts @atom-database))
            
            )
          )
      )
  )
)



(prn "Test archivo 1 *******  nivel basicol ")

(def incomplete-database "
	varon(juan).
	varon
")


(prn "Test 1 - debe dar nil ")
(prn (evaluate-query incomplete-database "varon(juan)"))         

(prn "Test 2 - debe dar nil ")
(prn (evaluate-query incomplete-database "varon(maria)"))  

(prn "Test 3 - debe dar nil ")
(prn (evaluate-query incomplete-database "mujer(cecilia)"))  

(prn "Test 4 - debe dar nil ")
(prn (evaluate-query incomplete-database "padre(juan, pepe)")) 

(prn "Test 5 - debe dar nil ")
(prn (evaluate-query incomplete-database "padre(mario, pepe)")) 

(prn "Test 6 - debe dar nil ")
(prn (evaluate-query incomplete-database "hijo(pepe, juan)")) 

(prn "Test 7 - debe dar nil ")
(prn (evaluate-query incomplete-database "hija(maria, roberto)")) 



(prn "Test archivo 2 *******  Test mayor nivel ")

(def number-database "
	add(zero, zero, zero).
	add(zero, one, one).
	add(zero, two, two).
	add(one, zero, one).
	add(one, one, two).
	add(one, two, zero).
	add(two, zero, two).
	add(two, one, zero).
	add(two, two, one).
	subtract(X, Y, Z) :- add(Y, Z, X).
")

(prn "Test 8 - add(one, one, two) should be true")
(prn (evaluate-query number-database "add(one, one, two)"))

(prn "Test 9 - add(two, one, one) should be false")
(prn (evaluate-query number-database "add(two, one, one)"))
  
(prn "Test 10 - subtract(one, one, two) should be false")
(prn (evaluate-query number-database "subtract(one, one, two)"))
           
(prn "Test 11 - subtract(one, one, two) should be false")
(prn (evaluate-query number-database "subtract(two, one, one)"))



(prn "Test archivo 3 *******  nivel Advance ************* ")

(def parent-database "
	varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")



(prn "Test 12 - varon(juan) should be true" )
(prn (evaluate-query parent-database "varon(juan)"))

(prn "Test 13 - varon(maria) should be false" )
(prn (evaluate-query parent-database "varon(maria)"))

(prn "Test 14 - mujer(cecilia) should be true")
(prn (evaluate-query parent-database "mujer(cecilia)"))
  
(prn "Test 15 - padre(juan, pepe) should be true")
(prn (evaluate-query parent-database "padre(juan, pepe)"))

(prn "Test 16 - padre(mario, pepe) should be false")
(prn (evaluate-query parent-database "padre(mario, pepe)"))

(prn "Test 17 - hijo(pepe, juan) should be true")
(prn (evaluate-query parent-database "hijo(pepe, juan)"))

(prn "Test 18 - hija(maria, roberto) should be false")
(prn (evaluate-query parent-database "hija(maria, roberto)"))

(prn "Test 19 - varon should be nil")
(prn (evaluate-query parent-database "varon"))
 
(prn "Test 20 - maria should be nil")
(prn (evaluate-query parent-database "maria"))

(prn "Test 21 - empty should be nil")
(prn (evaluate-query parent-database ""))






;(charge-db "varon(juan). varon(roberto). mujer(pepa). padre(juan,roberto). madre(pepa,roberto). hijo(X,Y):-varon(X),padre(Y,X)." atom-database)


;(prn @atom-database)


;(is-fact? "varon(carlos).")
;(is-fact? "varon(")
;(is-fact? "varon().")
;(is-fact? "(carlos).")


;(get-args-of "varon" (:facts @atom-database))
;(get-args-of "hijo" (:rules @atom-database))


;(evaluate-fact (gen-fact "varon(juana)") (:facts @atom-database)) 


;(set-args-in-order ["juan" "jorge"] [-1 0])
;(gen-evaluate-elem "hijo" (set-args-in-order ["juan" "jorge"] [-1 0]))


;(:name (gen-rule "hijo(X,Y):-varon(X),padre(Y,X)."))

;(evaluate-rule (gen-fact "hijo(roberto,juan).") atom-database) 



;(prn "-- Carga padre(juan,pepe)" )
;(charge-rule-or-fact "padre(juan,pepe)" atom-database)
;(prn "-- Carga varon(juan)" )
;(charge-rule-or-fact "varon(juan)" atom-database)
;(prn @atom-database)
;(prn "-- Carga varon(roberto)" )
;(charge-rule-or-fact "varon(roberto)" atom-database)
;(prn "-- Carga padre(juan,pepa)" )
;(charge-rule-or-fact "padre(juan,pepa)" atom-database)


;(type (:rules @atom-database))

;(charge-rule-or-fact "varon(juan)" atom-database) 
;(charge-rule-or-fact "padre(juan,pepe)" atom-database) 
;(charge-rule-or-fact "hijo(X,Y):-varom(X),padre(Y,X)." atom-database) 

;(str " -------- ATOM DATABASE --------- ")
;(first @(:facts @atom-database))

;(def pos-fact-in-facts (get-pos-fact-in-vector fact atom-facts))
;(def args (:args fact))
;(def vector-args (:args (nth @atom-facts pos-fact-in-facts)))



;(str " ---------- GEN FACT ---------- ")
;(conj (:args @(gen-fact "padre(juan,pepe)")) 

;(str " ---------- GET_POS_FACT_IN_VECTOR ----------")
;(get-pos-fact-in-vector @(gen-fact "padre(juan,pepe)") (:facts @atom-database))

;(str " ---------- add-arg-to-fact ----------")
;(add-arg-to-fact @(gen-fact "padre(roberto,carlos)") (:facts @atom-database))
;(add-arg-to-fact @(gen-fact "padre(juan,pepe)") (:facts @atom-database)) 

; TEST bd 
;(def db [{:name "varon" :args [["juan"]["pepe"]["marce"]["alex"]]} {:name "padre" :args [["marce" "alex"]["juan" "pepe"]]}])

;(str "gen-args")
;(gen-args  ["X" "Y"]["X" "Y"])

;(str "gen-args")
;(set-vars-order  ["X" "Y"]["X" "Y"])
;(= ["alex" "marce"] (into [] (get-args-in-order [0 1] (first (:args (get-fact-from-db "padre" db ))))))


;(def str-rule "hijo(X,Y):-varon(X),padre(Y,X),varon(Y).")

;(def vec-rule  (split-rule str-rule))
;(split-rule str-rule)

;(def vec-title (split-elem (first vec-rule)))
;(split-elem (first vec-rule))

;(def vec-vars  (split (first (rest vec-title)) #"[,]+"))
;(split (first (rest vec-title)) #"[,]+")

;(def vec-args  (map split-elem (split (first (rest vec-rule)) #"\)\,")))
;(map split-elem (split (first (rest vec-rule)) #"\)\,"))


;(for [fact vec-args] (gen-args (split-elem (rest fact)) vec-vars))
;(for [fact vec-args] (split-elem (rest fact)))



;(make-vector-args vec-vars vec-args)
;(gen-rule str-rule)

;(gen-fact "padre(juan,pepe).")

;(map split-elem (split (first (rest vec-rule)) #"\)\,"))




;(into [] (for [z pos] ((fn [x y] (nth y x)) z ["juan" "pepe"])))

;(get-pos-var-into-vector "X" ["X" "Y"])



