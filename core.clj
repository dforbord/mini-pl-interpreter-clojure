;; Mini-PL Grammar (EBNF)
;; <program>        ::= { <statement> }
;; <statement>      ::= <assignment> | <incrementation> | <conditional> | <for-loop> | <display>
;; <assignment>     ::= ("=" <identifier> <expression>)
;; <incrementation> ::= ("inc" <identifier>)
;; <conditional>    ::= ("if" <bool-expr> <block> <block>)
;; <for-loop>       ::= ("for" [<identifier> <number> <number> <number>] <block>)
;; <display>        ::= ("display" <expression>)
;; <block>          ::= ( { <statement> } )
;; <expression>     ::= <number> | <identifier> | ("+"|"-"|"*"|"/" <expression> <expression>) | <incrementation>
;; <bool-expr>      ::= ("<"|">"|"="|"!=" <expression> <expression>)

(ns mini-pl-final.core
  (:gen-class))


(defn eval-expr [expr env]
  (cond
    (number? expr) expr
    (symbol? expr) (get env expr 0)
    (list? expr)
    (let [op (first expr)]
      (case op
        + (+ (eval-expr (second expr) env) (eval-expr (nth expr 2) env))
        - (- (eval-expr (second expr) env) (eval-expr (nth expr 2) env))
        * (* (eval-expr (second expr) env) (eval-expr (nth expr 2) env))
        / (let [right (eval-expr (nth expr 2) env)]
            (if (zero? right)
              (throw (Exception. "Division by zero"))
              (quot (eval-expr (second expr) env) right)))
        inc (inc (eval-expr (second expr) env))
        (throw (Exception. (str "Invalid expression operator: " op)))))
    :else (throw (Exception. (str "Invalid expression: " expr)))))

(defn eval-bool [expr env]
  (let [op (first expr)
        left (eval-expr (second expr) env)
        right (eval-expr (nth expr 2) env)]
    (case op
      < (< left right)
      > (> left right)
      = (= left right)
      '!= (not= left right)
      (throw (Exception. (str "Invalid boolean operator: " op))))))

(defn interpret
  ([program] (interpret program {}))
  ([program env]
   (loop [stmts program
          env env]
     (if (empty? stmts)
       env
       (let [stmt (first stmts)
             op (first stmt)]
         (case op
           = (recur (rest stmts)
                    (assoc env (second stmt) (eval-expr (nth stmt 2) env)))
           inc (recur (rest stmts)
                      (update env (second stmt) (fnil inc 0)))
           display (do
                     (println (eval-expr (second stmt) env))
                     (recur (rest stmts) env))
           if (if (eval-bool (second stmt) env)
                (recur (concat (nth stmt 2) (rest stmts)) env)
                (recur (concat (nth stmt 3) (rest stmts)) env))
           for (let [[i start end step] (second stmt)
                     block (nth stmt 2)
                     vals (range start (+ end 1) step)]
                 (let [final-env
                       (reduce
                         (fn [e v]
                           (interpret block (assoc e i v)))
                         env
                         vals)]
                   (recur (rest stmts) final-env)))
           (throw (Exception. (str "Invalid operator: " op)))))))))

(def program1
  '(
    (= x 3)
    (= y (inc x))
    (if (> y 3)
      (
       (= z (+ y 2))
       (display z)
       )
      (
       (= z 0)
       )
      )
    ))

(def program2
  '(
    (= sum 0)
    (for [i 1 5 1]
      (
       (= sum (+ sum i))
       (display sum)
       )
      )
    (display sum)
    ))

(defn -main [& args]
  (try
    (println "Running Program 1:")
    (interpret program1)
    (println "\nRunning Program 2:")
    (interpret program2)
    (catch Exception e
      (println "Runtime error:" (.getMessage e)))))
