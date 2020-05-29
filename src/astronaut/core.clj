(ns astronaut.core
  (:require [cli-matic.core :refer [run-cmd]])
  (:require [clojure.java.jdbc :refer :all])
  (:require [clojure.pprint :as p])
  (:require [clojure.java.jdbc :as j])
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

;;; Declaring constants at the top is very idiomatic. Good form.

(def home-directory (System/getProperty "user.home"))
(def astro-directory (str home-directory "/.astronaut/"))
(def cards-db-location (str astro-directory "cards.db"))


;; This works, but you might want to try wrapping it with
;; HugSQL or HoneySQL. Those libraries do a great job of 
;; abstractingaway all the manual work you would otherwise 
;; have to do with strings.

(def INITIALIZE-DB (str "create table cards"
                        "(id TEXT PRIMARY KEY,"
                        "card_id INTEGER,"
                        "front TEXT,"
                        "back TEXT,"
                        "attempt INTEGER,"
                        "confidence INTEGER,"
                        "review INTEGER,"
                        "next_attempt INTEGER,"
                        "next_review INTEGER);"))

(defn review-query
  [ts]
  (str "select * from (select distinct * from (select * from cards order by next_review desc) group by card_id) where next_review <= " ts ";"))

;; This is purely my personal preference, but I try
;; not to include libraries when they are really only
;; going to be used once.
;;
;; Other Clojurians often write something like this as:
;
; (def current-time (System/currentTimeMillis))

(defn current-time [] (c/to-long (t/now)))


;; Probably makes more sense at the top or in its own file.

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     cards-db-location})

(defn qry
  "Return the result of a query from a string when the result is expected to be a single value"
  [q]
  (first (j/query db q)))

;; So far, most functions are pure. Excellent on that point.
;; It's convention among functional programmers to divide
;; code into the pure parts up top, side-effecting parts
;; at the bottom. 
;;
;; If you need to re-arrange things for clarity, you can
;; (declare) your functions in advance. 

(defn newest
  []
  (p/print-table (j/query db "REVIEWQUERY"))) 


(defn count-cards
  "Return the current number of cards in the database"
  []
  (:count (qry "select count(*) as count from cards;")))


;; Have a look at the pattern here. You can often tell when
;; code is written in a functional style from imperative
;; or object-oriented by the shape of the code. It "flows"
;; rather than being a solid block.

(defn largest-card-id
  "Find the largest card id, if none exists, return 0"
  []
  (let [lgid (:max (qry "select max(card_id) as max from cards;"))]
    (if (nil? lgid)
      0
      lgid)))

(defn new-card-id
  "Create a new integer card id by incrementing the largest id"
  []
  (+ 1 (largest-card-id)))

;; Good use of composition here.
;;
;; It's also a good chance to familiarize yourself with Clojure's
;; threading macro. I usually reach for it when there are maybe
;; four or more "layers" that a reader has to navigate.
;;
;; With the threading macro, you could write create-id like this:
;
; (defn create-id
;   []
;   (-> (str (java.util.UUID/randomUUID))
;       (str/split #"-")
;       first))
;
;; It's possible to overuse and make code unreadable, but when it
;; helps, it's hard to imagine coding without it!

(defn create-id
  []
  (first (str/split (str (java.util.UUID/randomUUID)) #"-"))) 

;; I see this map is being passed around a lot. There are a few things
;; you could do to make it easier to work with.
;;
;; One option might be to have a declared map with a particular structure
;; at top-level and use it to create new maps with different contents,
;; using "assoc", "dissoc", "merge", and friends.
;; You also might 

(defn insert-card-query-string
  [{id :id
    attempt :attempt
    card_id :card_id
    front :front
    back :back
    review :review
    next-review :next-review
    confidence :confidence}]
  (str "insert into cards"
       "(id,card_id,front,back,attempt,review,confidence,next_attempt,next_review) "
       "values('"
        (create-id) "',"
        card_id ",'"
        front "','"
        back "',"
        attempt ","
        review ","
        confidence ","
        (+ 1 attempt) ","
        next-review ");"))

;; Typically, you would only define a function inside of another when you're
;; trying to make a lexical closure, and it's usually anonymous in that case.
;;
;; Also, while you can use "def" anywhere, it's meant to be used at top-level.
;; Inside of functions, you want lexically scoped variables, usually declared
;; with "let".

(defn next-review
  [att conf t]
  (+ 86400000 (* (math/expt (int att) (Math/log conf))) t))


;; A bit heavy. A lot can be factored out, and "query-string" is the easiest
;; target of the bunch.

(defn schedule
  [{id :id attempt :attempt card_id :card_id confidence :confidence ts :ts}]
  (let [last-attempt (first (j/query db (str "select * from cards where id = '" id "';")))
        last-attempt-confidence (:confidence last-attempt)
        query-string (insert-card-query-string {:attempt (+ 1 attempt)
                                                :card_id card_id
                                                :confidence confidence
                                                :review ts
                                                :front (:front last-attempt)
                                                :back (:back last-attempt)
                                                :next-review (next-review attempt confidence ts)})]

    (println last-attempt)
    (j/execute! db query-string))) ; The caveat to lexical bindings is that they create a scope,
                                   ; and all dependent statements have to exist in that scope to work.


;; Again a style thing for me, but when writing imperative code,
;; I like to break it up so the effects are distinct.
;; When the code is blocky, I find it hard to read.
;;
;; This function does a lot of side-effecty things.
;; I would factor it out into smaller functions.

(defn review-card
  [{id :id front :front, back :back, attempt :attempt card_id :card_id}]

  (println (str "\u001b[32;1m" front "\u001b[0m\n"))

  (println "press any key when ready to flip card")

  (def pause (read-line))

  (println (str "\u001b[34;1m" back "\u001b[0m"))

  (do (print "How easy was that [0-10]? ")
      (flush)
      (let [confidence (read-line)]
        (schedule {:id id :attempt attempt :card_id card_id :confidence (Integer/parseInt confidence) :ts (current-time)}))))


(defn review
  []
  (let [ts (current-time)
        cards (j/query db (review-query ts))
        len (count cards)]
    (if (= len 0)
      (println "Congrats, you're all done for now!")
      (do
        (println (str "\u001b[33;1m" len " cards left to review.\u001b[0m\n"))
        (map review-card cards)))))


;; DEFINITELY needs to be broken up, but it's lexically scoped now.

(defn add-card
  [{:keys [front back]}]
  (let [card_id (new-card-id)
        attempt 0
        id (create-id)
        confidence 0
        review (current-time)
        query-string (str
                      "insert into cards "
                      "(id,card_id,front,back,attempt,review,confidence,next_attempt,next_review) "
                      "values('"
                      id "',"
                      card_id ",'"
                      front "','"
                      back "','"
                      attempt "','"
                      review "','"
                      confidence "','"
                      (+ 1 attempt) "','"
                      review "');")]
    (j/execute! db query-string)
    (println (str "card " id " added to ship."))))

(defn list-cards
  []
  (p/print-table (j/query db (str "select * from cards;"))))

(defn init-table
  [{:keys [& args]}]
  (println (str "creating " astro-directory))
  (.mkdir (java.io.File. astro-directory))
  (println (str "creating " cards-db-location))
  (spit cards-db-location nil)
  (j/execute! db INITIALIZE-DB)
  (println "cards db initialized"))

(def CONFIGURATION
  {:app         {:command     "astronaut"
                 :description "Command-line spaced-repition"
                 :version     "0.0.1"}
   ; :global-opts [{:option  "tags"
   ;                :short   "t"
   ;                :as      "Card context"
   ;                :type    :string
   ;                :default ""}]
   :commands    [{:command     "init"
                  :description "Initialize astronaut cli"
                  :runs        init-table}
                 {:command     "add-card" :short "a"
                  :description "Adds a card to the backlog"
                  :opts        [{:option "front"
                                 :short "f"
                                 :as "Front of the card"}
                                {:option "back"
                                 :short "b"
                                 :as "Back of the card"}]
                  :runs        add-card}
                 {:command     "inspect"
                  :description "Review scheduled cards"
                  :runs        review}
                 {:command     "list"
                  :description "List all cards"
                  :runs        list-cards}
                 ]})

(defn -main
  [& args]
  (run-cmd args CONFIGURATION))
