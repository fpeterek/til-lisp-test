;;;; Bavíme se o třídě o třech studentech
(def indivs '('Tom 'Karel 'Petr))

;;;; Primitivní substituční metoda
(defn sub [where what with]
  (map 
       (fn [construction]
           (cond 
             (= construction what) with
             (list? construction) (sub construction what with)
             (not (= construction what)) construction)) 
       where))

;;;; Trivializaci řešíme pomocí substituční metody
;;;; Do trivializace 'what musíme za what dosadit vlastní hodnotu místo what
(defn tr [what] (sub ''what 'what what))

;;;; Píšou první test z matematiky
(defn compute-one [who what]
    (cond 
      (= who 'Tom) (= what '(= '(+ x 10) 15))
      (= who 'Karel) (= what '(= '(- x 20) 13))
      true nil))

;;;; Píšou druhý test z matematiky
(defn compute-two [who what]
    (cond 
      (= who 'Tom) (= what '(= '(* x 3) 15))
      (= who 'Karel) (= what '(= '(/ x 3) 13))
      true nil))

;;;; Nikdo nic nepočítá
(defn no-computations [who what] nil)

;;;; Ve světě číslo 1 se píšou dva testy ve dvou časových intervalech
;;;; Jinak se nic nepočítá
(defn computes [world]
  (if (= world 1)
    (fn [tm]
      (cond 
        (and (> tm 150) (< tm 200)) compute-one
        (and (> tm 250) (< tm 300)) compute-two
        true no-computations))
    no-computations))

;;;; Tom je po určitou dobu nejlepším studentem ve třídě
;;;; Jindy nejlepší student neexistuje
(defn nejlepsi-student [world]
  (if (= world 1)
    (fn [tm]
        (cond 
          (and (> tm 100) (< tm 400)) 'Tom
          true nil))
    (fn [tm] nil)))

;;;; Provedení
;;;; V Common Lispu lze využít ^1 jako název funkce, v Clojure ne
;;;; V Common Lispu ale nelze přímo zavolat funkci vracenou jinou funkcí, v Clojure ano -- ((nejlepsi-student 1) 150)
(defn *1 [con] (eval con))

;;;; Dvojí provedení
(defn *2 [con] (*1 (*1 con)))

(def substituce (sub  ;; V proměnné bude uložena konstrukce s již provedenou substitucí
  '(((computes 1) 170) y '(= '(+ x 10) 15))     ; Kde nahrazujeme
  'y                                            ; Co  nahrazujeme
  (tr ((nejlepsi-student 1) 170))))             ; Čím nahrazujeme

;;;; Ukázka, že v Lispu lze TIL konstrukce modelovat, pracovat s nimi (viz např. substituční metoda),
;;;; ale také je evaluovat a získat tak jejich výsledky
(defn check []
    (printf
        "Je rovnice x + 10 = 15 počítána Tomem, který je nejlepší student? %b%n" 
        (*2 'substituce))) ;;; Proměnnou trivializujeme pouze za účelem vyzkoušení dvojitého provedení

(defn -main [] (check))

