;; Author: Christopher Sasarak
;; This declares that these functions are in the parser namespace
(ns parser "Basic functions for monadic parsing.") 
 
;; For the purposes of these parsers, the results will be a vector of 2-vectors
;; One possible alternative is a Clojure facility such as defrecord which provides
;; some documentation of the expected values for a type and creates a Java class for
;; those items under the hood. e.g. (defrecord parseItem [consumed unconsumed])
;; See page 189 of "The Joy of Clojure" by Michael Fogus and Chris Houser


;; FUNCTIONS FOR MANIPULATING PARSERS
(defn success [v]
  "Success returns a parser which consumes none of its input and returns
   v as the result"
  (fn [inp] [[v inp]])) ;; fn creates a function that isn't bound to a name, defn
                        ;; is a combination of the def and fn macros for named functions

(defn zero [inp]
  "This parser always fails, regardless of its input"
  [])

(defn item [inp]
  "Consumes the first character if the input string is non-empty"
  (if (seq inp)
    (let [[hd & rst] inp] ;; This destructures inp into its head and tail
      [[hd rst]])
    []))

(defn sat [pred]
  "Takes a predicate and returns a parser that consumes a character if
   the predicate is satisfied, otherwise nothing"
  (letfn [(sat' [inp]
                (if (pred inp)
                  (success inp)
                  zero))]
         (>>= item sat')))

;; This function has a slight advantage on the haskell implementation because it
;; can operate on an arbitrary number of parsers

(defn choice [& parsers] ;; '&' specifies that arguments should be in the 'parsers' list
  "Returns a parser which applies the supplied parsers as arguments to
   the same inp string and returns a vector of their results"
  (fn [inp]
    (map #(% inp) parsers);; #(s-exp) is shorthand for a lambda, % refers to its argument
         ;; for multiple arguments use %1, %2 .. %n for multiple arguments
    ))

(defn choice-succ [& parsers]
  "Assuming some of the parsers will fail on the given input, return a parser which
   applies them all to the same input and only return the successful output"
  (fn [inp]
    (filter #(seq %) ((apply choice parsers) inp))))

(defn ex-or [& parsers]
  "Like the choice function, but only returns the first successful parse"
  (fn [inp]
    (first ((apply choice-succ parsers) inp))
    ))

(declare many)
(defn many1 [p]
  (>>= p (fn [inp1]
           (>>= (many p) (fn [inp2]
                           (let [rslt (filter #(not= nil %) (flatten [inp1 inp2])) ]
                             (success rslt))
                           )))))

(defn many [p]
  (ex-or (many1 p) (success [])))


;; FUNCTIONS FOR DEALING WITH MONADS
(defn >>= [pars f]
  "A bind function for parsers"
  (fn [inp] (first (concat (for [[v inp'] (pars inp)]
                             ((f v) inp'))))))

(defn >> [p1 p2]
  "Then is like bind but throws away the result of the first monad"
  (>>= p1 (fn [inp]
            p2)))

(defn bind2 [p1 p2]
  "Bind two parsers together in sequence"
  (>>= p1 (fn [inp1]
            (>>= p2 (fn [inp2]
                      (success (flatten [inp1 inp2])))))))

;; This function can be used to emulate Haskell's do notation for parsers.  It
;; uses Clojure's loop-recur functionality which are macros that let you do tail
;; recursion. The JVM does not allow for this to be done automatically.  The
;; first loop call binds initial values to variables. The call to recur is
;; supplied with new values for these variables and jumps up to the most recent
;; recursion target it can find, in this case loop. The function itself can also
;; be used as a target, i.e. use recur without loop.
(defn bind-many [p1 p2 & parsers] 
  "Bind a list of many parsers together in sequence"
  (loop [[p3 & ps] parsers acc (bind2 p1 p2)]
    (if (not p3) acc
        (recur ps (bind2 acc p3))))
  )

;; FUNCTIONS THAT MAKE PARSERS FOR DIFFERENT STRINGS

;; char is already a clojure function, for this function and all others 'p-'
;; stands for 'parse'
(defn p-char [x]
  "Takes a single character and returns the parser that consumes only
   that character"
  (sat #(= x %)))

;; This function shows java interop with Character/isDigit. In order
;; to use a java function as a first class object though, it must be
;; wrapped in a Clojure function, java.lang.* is automatically
;; imported into any clojure program
(def digit 
     "Return the parser that parses a character if it is a digit."
     (sat #(Character/isDigit %)))

(def lower
     "The parser that parses a lowercase character"
     (sat #(Character/isLowerCase %)))

(def upper
     "The parser that parses a lowercase character"
     (sat #(Character/isUpperCase %)))

(def whitespaceChar
     "The parser which matches a whitespace character"
     (sat #(Character/isWhitespace %)))

(def letter
     "The parser which matches any single upper or lower-case letter of the
      alphabet"
     (ex-or lower upper))

(def alphanum
     "The parser which matches any upper or lower-case letter or a number"
     (ex-or letter digit))



