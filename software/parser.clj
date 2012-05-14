;; Author: Christopher Sasarak
;; This declares that these functions are in the parser namespace
(ns parser "Basic functions for monadic parsing.") 

;; For the purposes of these parsers, the results will be a vector of 2-vectors
;; One possible alternative is a Clojure facility such as defrecord which provides
;; some documentation of the expected values for a type and creates a Java class for
;; those items under the hood. e.g. (defrecord parseItem [consumed unconsumed])
;; See page 189 of "The Joy of Clojure" by Michael Fogus and Chris Houser

(defn >>= [pars f]
  "A bind function for parsers"
  (fn [inp] (first (concat (for [[v inp'] (pars inp)]
                             ((f v) inp'))))))

(defn success [v]
  "Success returns a parser which consumes none of its input and returns
   v as the result"
  (fn [inp] [[v inp]])) ;; fn creates a function that isn't bound to a name, defn
                        ;; is a combination of the def and fn macros for named functions

(defn >> [p1 p2]
  "Then is like bind but throws away the result of the first monad"
  (>>= p1 (fn [inp]
            p2)))

(defn bind2 [p1 p2]
  "Bind two parsers together in sequence"
  (>>= p1 (fn [inp1]
            (>>= p2 (fn [inp2]
                      (success (concat [inp1 inp2])))))))

(defn throwaway [p1]
  "Run p1 and throw away whatever it returns"
  (>> p1 (success [])))

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

;; FUNCTIONS FOR MANIPULATING PARSERS

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

(defn parse' [p1 inp]
  "Run a parser with the given input"
  (p1 inp))

(defn parse [p1 inp]
  "Run a parser with the given input and return it as a 2-vector of Strings, or
   nil if the text couldn't be parsed successfully"
  (let [[consumed left] (first (parse' p1 inp))]
    (if (seq consumed)
      [(apply str (flatten consumed)) (apply str left)]
      nil)
    ))

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

(defn x-or [& parsers]
  "Like the choice function, but only returns the first successful parse"
  (fn [inp]
    (first ((apply choice-succ parsers) inp))
    ))

;; Because many1 and many are mutually recursive, using a declare directive tells
;; clojure not to worry about it when it tries to interpret many and many1
(declare many)
(defn many1 [p]
  (>>= p (fn [inp1]
           (>>= (many p) (fn [inp2]
                           (let [rslt (filter #(not= nil %) (flatten [inp1 inp2])) ]
                             (success rslt))
                           )))))

(defn many [p]
  (x-or (many1 p) (success [])))

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
     (x-or lower upper))

(def alphanum
     "The parser which matches any upper or lower-case letter or a number"
     (x-or letter digit))

(defn string [str]
  "Return the parser that matches a specific string"
  (fn [inp]
    (let [parsers (map #(p-char %) (seq str))]
      ((apply bind-many parsers) inp))))

(def word
     "A sequence of at least one letter"
     (many1 letter))

;; BASIC PARSER FOR S-EXPRESSIONS

(defn isValidStringChar [c]
  (not= c \"))
;; A '\' followed by a character is the character represented by that character
;; e.g. \c == 'c'

(def string-p
     "A string starts with a single \", followed by any number of characters
     or whitespace and end with a \"."
     (fn [inp]
       ((bind-many (p-char \")
                   (many (sat isValidStringChar))
                   (p-char \")) inp))
     )

(declare sexp-p) 
(def sexp-list-p (bind-many (many whitespaceChar)
                            (x-or sexp-p word string-p)
                         ))

(defn outermost-sexps-p [inp]
     "This is for the outermost s-expressions"
     ((many (bind-many (many whitespaceChar)
                       sexp-p
                       (many whitespaceChar))) inp ))

(defn sexp-p [inp]
  "Parser for an non-toplevel s-expression"
  ((bind-many (many whitespaceChar)
              (x-or (p-char \( ) (string "'("))
              (many whitespaceChar)
              (many sexp-list-p)
              (many whitespaceChar)
              (p-char \) )) inp)
  )


(defn parse-s-expressions [str]
  "This function will parse s-expressions, returning a vector of parsed text
   and unparsed text."
  (parse outermost-sexps-p str))
