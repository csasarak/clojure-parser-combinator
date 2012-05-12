;; Author: Christopher Sasarak
;; This declares that these functions are in the parser namespace
(ns parser "Basic functions for monadic parsing.") 

;; For the purposes of these parsers, the results will be a vector of 2-vectors
;; One possible alternative is a Clojure facility such as defrecord which provides
;; some documentation of the expected values for a type and creates a Java class for
;; those items under the hood. e.g. (defrecord parseItem [consumed unconsumed])
;; See page 189 of "The Joy of Clojure" by Michael Fogus and Chris Houser


;; FUNCTIONS FOR MANIPULATING PARSERS
(defn result [v]
  "Result returns a parser which consumes none of its input and returns
   v as the result"
  (fn [inp] [[v inp]])) ;; fn creates a function that isn't bound to a name, defn
                        ;; is a combination of the def and fn macros for named functions

(defn zero []
  "This parser always fails, regardless of its input"
  (fn [inp] []))

(defn item []
  "Consumes the first character if the input string is non-empty"
  (fn [inp]
    (if (seq inp)
      (let [[hd & rst] inp] ;; This destructures inp into its head and tail
        [[hd rst]])
      [])))

(defn >>= [pars f]
  (fn [inp] (first (concat (for [[v inp'] (pars inp)]
                             ((f v) inp'))))))

(defn sat [pred]
  "Takes a predicate and returns a parser that consumes a character if
   the predicate is satisfied, otherwise nothing"
  (let [sat' (fn [x]
               (if (pred x)
                 (result x)
                 (zero))
               )
        ]
    (>>= (item) sat')))

;; This function has a slight advantage on the haskell implementation because it
;; can operate on an arbitrary number of parsers

(defn choice [& parsers] ;; '&' specifies that arguments should be in the 'parsers' list
  "Returns a parser which applies the supplied parsers as arguments to
   the same inp string and returns a vector of their results"
  (fn [inp]
    (map #(% inp) parsers ;; #(s-exp) is shorthand for a lambda, % refers to its argument
         ;; for multiple arguments use %1, %2 .. %n for multiple arguments
         )))

(defn choice-succ [& parsers]
  "Assuming some of the parsers will fail on the given input, return a parser which
   applies them all to the same input and only return the successful output"
  (fn [inp]
    (filter #(seq %) ((apply choice parsers) inp))))

(defn parse-char [x]
  "Takes a single character and returns the parser that consumes only
   that character"
  (sat #(= x %)
       ))

;; FUNCTIONS THAT MAKE PARSERS FOR DIFFERENT STRINGS

;; char is already a clojure function, for this function and all others 'p-'
;; stands for 'parse'
(defn p-char [x]
  "Takes a single character and returns the parser that consumes only
   that character"
  (sat #(= x %)))
