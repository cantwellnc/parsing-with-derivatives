(ns parser)

;; this is a clojure port of https://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
;; any mistakes are my own!

(def regex-NULL false)

(def regex-BLANK true)

;; our re syntax will be defined using lists, where
;; the first item is the type of combinator we are using
;; and the others are the args to that combinator. 

;; (:alt a b) => a choice of matching a or b. Typically denoted a | b or a ∪ b
;; Note: In the OG paper, the defintions of the derivative/the regex-empty fn actually work
;; for ANY boolean connective f(P, Q), not just alternation (P ∪ Q). He gives many examples like
;; complement with respect to a universal set, set difference, xor, intersection, etc.
(defn regex-alt? [re]
  (and (seq? re) (= (first re) :alt)))

;; (:seq a b) => match a, and then b, aka concatenation. Typically denoted ab 
(defn regex-seq? [re]
  (and (seq? re) (= (first re) :seq)))

;; (:rep a) => match a 0 or more times, aka the kleene start. Typically denoted a*
(defn regex-rep? [re]
  (and (seq? re) (= (first re) :rep)))

(defn regex-null? [re]
  (= re regex-NULL))

(defn regex-empty? [re]
  (= re regex-BLANK))

;; matches literal characters or symbols, ex: "a"
(defn regex-atom? [re]
  (or (char? re) (symbol? re)))

;; deconstructors 
;; used for accessing the parts of seq, alt, rep

(defn match-alt
  "if re is of the form (:alt a b), return (f a b), else"
  [re f]
  (and (regex-alt? re)
       (let [[_ choice-1 choice-2] re]
         (f choice-1 choice-2))))


(defn match-seq
  "if re is of the form (:seq a b), return (f a b), else false"
  [re f]
  (and  (regex-seq? re)
        (let [[_ first-item second-item] re]
          (f first-item second-item))))

(defn match-rep
  "if re is of the form (:rep a), return (f a), else false"
  [re f]
  (and  (regex-rep? re)
        (let [[_ to-repeat] re]
          (f to-repeat))))


;; regex constructors

(defn seq [pat1 pat2]
  (cond
    (regex-null? pat1) regex-NULL
    (regex-null? pat2) regex-NULL
    (regex-empty? pat1) pat2
    (regex-empty? pat2) pat1
    :else (list :seq pat1 pat2)))

(defn alt
  [pat1 pat2]
  (cond
    (regex-null? pat1) pat2
    (regex-null? pat2) pat1
    :else (list :alt pat1 pat2)))

(defn rep [pat]
  (cond
    (regex-null? pat) regex-BLANK
    (regex-empty? pat) regex-BLANK
    :else (list :rep pat)))

;; Matching

;; we have to create a macro 
;; to let use refer to the result of a cond test, since we
;; are recursively testing seq/alt expressions
(defmacro cond-let
  "Meant as companion to if-let and when-let. 
   Each clause is a pair [test expr]. 
   Recursively builds a tower of if-lets so that we can always 
   refer to the result of the test in expr if we need to.
   "
  [& clauses]
  (when-let [[test-binding result-expr & rest-clauses] clauses]
    (list 'if-let test-binding
          result-expr
          (cons 'cond-let rest-clauses))))

(comment
  (cond-let
   [a (> (+ 1 2) 0)] :nice
   [b (when true (println "bad"))] :not-nice)) ;; => :nice



(defn regex-empty
  "does `re` match the empty string?"
  [re]
  (cond-let
   [_ (regex-empty? re)] true
   [_ (regex-null? re)] false
   [_ (regex-atom? re)] false
   [result (match-seq re (fn [pat1 pat2] (seq (regex-empty pat1) (regex-empty pat2))))] result
   [result (match-alt re (fn [pat1 pat2] (alt (regex-empty pat1) (regex-empty pat2))))] result
   [_ (regex-rep? re)] true
   [_ :else] false))


(comment
  (regex-empty regex-NULL) ;; => false
  (regex-empty regex-BLANK) ;; => true
  (regex-empty (alt "a" "b")) ;; => false
  (regex-empty (alt regex-BLANK "b")) ;; => true
  (regex-empty (seq regex-BLANK "b")) ;; => false
  (regex-empty (seq regex-BLANK regex-BLANK)) ;; => true 
  (regex-empty (rep "a")) ;; => true 
  (regex-empty (seq regex-BLANK (alt regex-BLANK "a")))) ;; => true 


;; is there a way to express the complement of a regex using this? 

;;
(defn derivative
  "takes the derivative of re w.r.t. c (a chararacter or symbol) returns a regex."
  [re c]
  (println "DERIVATIVE")
  (println "pattern: " re)
  (println "char: " c)
  (cond-let
   [_ (regex-empty? re)] regex-NULL
   [_ (regex-null? re)] regex-NULL
   [_ (= re c)] regex-BLANK ;; re is exactly c, so the derivative is the empty regex
   [_ (regex-atom? re)] regex-NULL ;; it's some atom that is not c, so no match
   [result (match-seq re (fn ;; "product rule"
                           [pat1 pat2]
                           (alt
                            (seq (regex-empty pat1) (derivative pat2 c))
                            (seq (derivative pat1 c) pat2))))] result
   [result (match-alt re (fn [pat1 pat2] ;; "sum rule"
                           (alt
                            (derivative pat1 c)
                            (derivative pat2 c))))] result
   [result (match-rep re (fn [pat] ;; rep rule
                           (seq (derivative pat c) (rep pat))))] result
   [_ :else] regex-NULL))


(defn regex-match
  "checks if `pattern` matches `data` using the derivative 
   with respect to the data as a matching algorithm."
  [pattern data]
  (println "PATTERN: " pattern)
  (println "DATA: " data)

  (if (empty? data)
    (regex-empty? (regex-empty pattern))
    (regex-match (derivative pattern (first data)) (rest data))))


(comment
  ;; the regex "dog" should match the string "dog"
  ;; we have to represent everything as lists of characters with the current 
  ;; impl which is a but unwieldy, but fixable.
  (regex-match `(:seq (:seq "d" "o") "g") `("d" "o" "g")) ;; => true

  ;; blog post example: do(g|t) should match "dog" and "dot"
  (and (regex-match `(:seq (:seq "d" "o") (:alt "g" "t")) `("d" "o" "g"))
       (regex-match `(:seq (:seq "d" "o") (:alt "g" "t")) `("d" "o" "t"))) ;; => true
  
  ;; but not "doll"
  (regex-match `(:seq (:seq "d" "o") (:alt "g" "t")) `("d" "o" "l" "l"));; => false
  ) 

  
















