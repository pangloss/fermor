(ns fermor.cypher-examples
  (:require  [clojure.test :as t]
             [fermor.core :refer :all]
             [clojure.string :as str]))

(defn properties [p r]
  (->> r documents (map p)))

(defn property [p e]
  (->> e get-document p))

;; From https://medium.com/neo4j/the-power-of-subqueries-in-neo4j-4-x-4f1888739bec
;; This seems to be the query that it builds towards. I'm not quite sure what's
;; so special about it. One of the problems with cypher queries is that you end
;; up having to name virtually every intermediate node even if it doesn't really
;; have any relevance to the overall result of the query meaning the query can
;; be hard to understand because it's not obvious which parts of the query are
;; significant for the result vs just intermediate steps or filtering steps.
;; Anyway, if I understand the query correctly, it's easy enough to represent directly
;;
;; MATCH (a:Person)-[:ACTED_IN]->(m)
;; CALL {
;;       WITH a,m
;;       MATCH (m)<-[:ACTED_IN]-(co:Person)
;;       WHERE a <> co AND co.name contains 'T'
;;       WITH distinct co LIMIT 10
;;       RETURN collect(co.name) AS coactors}
;;
;; CALL {
;;       WITH m d
;;       MATCH (m)<-[:DIRECTED]-(d:Person)-[:DIRECTED]->(m2)
;;       WITH  d, collect(m2.title) AS movies
;;       RETURN collect(d {.name, movies:movies}) AS directors}
;;
;; RETURN a.name, m.title, coactors, directors

;; This version is a very direct port of the above query and in a fermor system
;; would never pass code review. It has all of the guts of the query hanging
;; out. Instead we can trivially create a domain model and work at a much higher
;; level of abstraction.

(defn query-directly-ported [g]
  (for [a (->> (vertices :Person g))
        m (out :acted-in a)
        :let [coactors (->> m (in :acted-in) distinct (isn't a)
                            (properties :name) (filter #(str/includes? % "T"))
                            (take 10))
              directors (for [d (out :directed m)]
                          [(property :name d)
                           (->> m (in :directed) (properties :title))])]]
    [(property :name a) (property :title m) coactors directors]))


;; This version has a domain model, meaning that you never need to remember the
;; direction of an edge or exactly how a relationship is modelled outside of the
;; base definitions in the model that can be trivially tested independently of
;; any larger query.

(defn people [g]
  (vertices :Person g))

(defn acted-in [actors]
  (out :acted-in actors))

(defn cast-members [films]
  (in :acted-in films))

(defn directed [directors]
  (->> directors (out :directed)))

(defn directors [films]
  (->> films (in :directed)))

(defn names [r]
  (properties :name))

(defn titles [r]
  (properties :title))

(defn title [movie]
  (property :title movie))

(defn person-name [person]
  (property :name person))

(defn coactors [actor movie]
  (->> movie cast-members distinct (isn't actor)))

(defn with-substr [substr strs]
  ;; Even weird domain-specific queries like this can be easily incorporated to the model
  (filter #(str/includes? % substr) strs))


;; With a few lines of reusable domain modelling in place, the query above can now be written like this:

(defn query-with-model [g]
  (for [a (people g)
        m (acted-in a)
        :let [coactors (->> (coactors a m) names (with-substr "T") (take 10))
              directors (for [d (directors m)]
                          [(person-name d) (->> d directed titles)])]]
    [(person-name a) (title m) coactors directors]))



;;--------------------------------------------------------------------
;; From https://neo4j.com/docs/stable/cypher-cookbook-hyperedges.html
;; Find common groups based on shared roles:

(comment
  ;; Interestingly, Cypher may be valid for the clojure reader. This is.
  MATCH (u1)-[:hasRoleInGroup]->(hyperEdge1)-[:hasGroup]->(group),(hyperEdge1)-[:hasRole]->(role),
  (u2)-[:hasRoleInGroup]->(hyperEdge2)-[:hasGroup]->(group),(hyperEdge2)-[:hasRole]->(role)
  WHERE u1.name = 'User1' AND u2.name = 'User2'
  RETURN group.name, count(role)
  ORDER BY group.name ASC)


;; One of the problems with Cypher is that every query starts from square one.
;; You can't feed the results that you already have that are interesting to you
;; into a cypher query. The best you can do is look up known nodes as this
;; example does. This is very different from how Fermor works and how in my
;; experience I usually need to work with graphs, which is starting from a
;; particular place, infer information from the local graph.

;; Again, some very simple modelling to build the query upon. The edge
;; traversals could be inline, but it is much easier to understand named methods
;; than edge traversals, and getting edges backwards is easily done and quite
;; confusing to debug in larger queries.

;; I called "hyper-edges" "user-roles" since that is what they are in the example.
;; The structure is identical. The whole idea of calling a node an edge is silly.
(defn user->user-roles [r] (out :hasRoleInGroup r))
(defn user-roles->users [r] (in :hasRoleInGroup r))
(defn user-roles->groups [r] (out :hasGroup r))
(defn groups->user-roles [r] (in :hasGroup r))
(defn user-roles->roles [r] (out :hasRole r))
(defn roles->user-roles [r] (in :hasRole r))
(defn group-name [e] (property :name e))

;; The best way to find the users depends on the graph used. For an in-memory
;; graph, expect to have an index as hash map if the lookup is common, otherwise
;; scan for it. Using a graph DB, you may pull it from a built-in index or scan.

(defn find-user-by-index-on-name [g name]
  (when-let [id (@(:name-index (meta g)) name)]
    (get-vertex g id)))

(defn find-user-by-scan [g name]
  (->> (vertices g :User)
       (has-property :name name)
       first))

(defn common-groups-from-shared-roles [u1 u2]
  ;; I don't normally use ->> to pull the body of the for statement up, but it
  ;; works fine and here it makes it look a lot like a SQL query which seems
  ;; appropriate for this graph query.
  (->> {:group-name (group-name group)
        :role-count (count roles)}
       (for [ur1 (user->user-roles u1)
             group (user-roles->groups ur1)
             ur2 (groups->user-roles group)
             :when (->> ur2 user-roles->users (is u2))
             :let [roles (->> ur1 user-roles->roles
                              (lookahead (f->> roles->user-roles (is ur2))))]
             :when (seq roles)])
       (sort-by :group-name)))



;;----
;; From https://neo4j.com/docs/stable/cypher-cookbook-path-tree.html
;; TODO
