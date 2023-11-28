<p align="center">
<img src=doc/mani.jpg />
</p>

[Fermor](https://github.com/pangloss/fermor) provides a flexible and high
performance streaming data traversal library built upon Clojure's lazy seq
abstraction. It's designed to allow exploration of complex graphs of data,
elegantly handling cycles, deep nesting and other patterns commonly seen in
graph data.

This library is a distillation of my experience creating and working with my
[Pacer](https://github.com/pangloss/pacer) library, which I have used to build
diverse sophisticated applications. Compared to Pacer, Fermor is much lighter
weight, more flexible, simpler and far faster, despite Pacer (as of a few years
ago) itself generally being much faster than other graph traversal mechanisms
that I had seen.

## Alpha software and roadmap

This project has mostly stabilised. The next major steps are to performance tune
and possibly integrate the Neo4j embedded API which may require some minor
protocol adjustments, but I don't anticipate major changes.

## High performance in-memory immutable graph database

Bundled with the traversal library is a fast immutable in-memory directed
property graph database built on the very elegant
[Bifurcan](https://github.com/lacuna/bifurcan) library. I use it to
build up graphs of 1-10 million vertices and edges in under 10 seconds, with
all queries I've needed to do so far running in 10-20ms, and full edge counts in
1-2s, all on my laptop.

The Fermor traversal namespace works well with any data source and
there is no dependency between most of the functions in the library and the
included graph.

## Powerful, composable and expressive traversal library

Like the Clojure core library, the functions in Fermor's traversal library are composable.

The key to using this library effectively is to treat it as a set of primitive building blocks
for creating your own domain-specific library.

In my examples below I will try to follow the pattern of decomposing functions
into small atomic units. This decomposition leads to surprising flexibility
without any performance overhead at all.

### Transducers

The most recent work on Fermor is in replicating all of the graph query
functionality in transducer variants. This is currently an experimental branch,
but seems very promising for both cleaner library design and higher performance.

I think this is an exciting development!

## Force directed graph layout

This project also includes a force directed layout engine that was originally
based on Force Atlas in Gephi. It has been substantially changed and behaves 
very differently from Force Atlas now, though.

My goal with this layout engine is to rapidly unfold nets that look like long
chains, minimizing unneeded crossings and producing a final layout that is
reasonably dense in a minimal rectangular area.

You can try the graph layout out in the `fermor.ui` namespace, either in the
repl or by running `clj -M -m fermor.ui`.

## Using Fermor

Fermor has several internal namespaces, but designed that anything typically
used by consumers of the library is exposed via the `fermor.core` namespace. All
fermor functionality below would be imported from that namespace.

### Building a Graph

To create a graph, simply call `(graph)`.

``` clojure
(def g (graph))
```

> It is possible to specify options allowing edges with a given label to have
> different characteristics (undirected, acyclic, etc.), but typically, the
> default settings are best.

Once you've built a graph, there are both bulk and individual methods to create a graph.

Fermor is backed by Bifurcan, and adopts its idea of `linear` and `forked` modes. 
Linear mode is a mutable graph. It's called linear because the work editing a mutable
graph should be done in a linear single process. Unlike Bifurcan, Fermor's forked mode is
read-only. You must switch back to linear mode to edit the graph.

The graph is created in linear mode.

Vertices must have an ID value when they are created. The ID may be any value.
It's common to use integer IDs, but symbols, keywords or any other immutable Clojure
value may be used.

Nodes may be either created standalone or created implicitly when
adding an edge between two node IDs using the `add-edges` function. This is
similar to how many much simpler graph tools create graphs. It allows 
graphs to be defined flexibly and easily. Both approaches may be intermixed.

For example, the following graph has 8 edges labelled `:to`, which I use in my
graph algos test. 

<img src="https://eli.thegreenplace.net/images/2015/dgraph1.png" />

``` clojure
(def simple-graph
  (-> (g/graph)
      (g/add-edges :to '[[A T] [A B] [A C] [T B] [C B] [B D] [C E] [E D]])
      g/forked))
```

I can add more edges using the `:to` label or add edges using any other label
with additional calls to `add-edges`.

Once a graph is created, a specific vertex can be selected with either
`get-vertex` or `get-vertex!`. The latter verifies that the vertex actually
exists in the graph. A nonexistent vertex will simply have no edges and no document.

``` clojure
(g/get-vertex simple-graph 'A)
;; => (v A)

(def A (g/get-vertex simple-graph 'A))
```

### Traversal

To traverse from one sequence of vertices to a related sequence of vertices
means following an edge in the graph. You can do that using the `in` and `out`
functions: `out` follows edges that originate in each element (the "out edges")
of the sequence and returns the vertex that the edge arrives at. You could think
of it as following edges in their forward direction. Sometimes we want to go the
other way, and we can do that with `in`, which follows edges that point to the
vertices in the sequence and returns the vertices that those edges originate
from.

Using A from the example above, we can traverse out from A to the vertices
related on the :to edges
``` clojure
(->> A (g/out :to))
;; => ((v B) (v C) (v T))
```

Or traverse out from a collection of vertices to all related on the :to edges
``` clojure
(->> [A A]
     (g/out :to))
;; => ((v B) (v C) (v T) (v B) (v C) (v T))
```

Or traverse along any type of edge out from A, then in from all of those edges. 
This will bring us back to A multiple times, but also to other vertices that 
can traverse into those edges.
``` clojure
(->> A g/out g/in)
;; => ((v A) (v C) (v T) (v A) (v A))
```

We can also traverse to the edges that outward from A
``` clojure
(->> A (g/out-e :to))
;; => ((e-> A :to B) (e-> A :to C) (e-> A :to T))
```


#### Building up toward a domain-specific query language

By defining simple functions like the following, you can rapidly create a very
expressive and high performance graph query system.

```clojure
(defn cities [state-nodes]
  (->> state-nodes
       (out :state->city)))

(defn states [city-nodes]
  (->> city-nodes
       (in :state->city)))
```

#### Filtering

In Clojure, we `filter` data all the time. We don't need to change that. First
we'll create a predicate function, then a traversal that uses it.

```clojure
(defn large-city? [city]
  (< 1000000 (population city)))


(defn large-cities [state-nodes]
  (->> state-nodes
       cities
       (filter large-city?)))
```

Lookaheads are useful for filtering based on data relationships. They come in a
few varieties. They are filters that match based on whether there is or isn't
the expected data connected to the element at hand.

```clojure
(defn states-with-a-large-city [state-nodes]
  (->> state-nodes
       (lookahead large-cities)))
```

We can also do lookaheads with specific min and max arguments.

```clojure
(defn states-with-2to5-large-cities [state-nodes]
  (->> state-nodes
       (lookahead {:min 2 :max 5} large-cities)))
```

Or do a negative lookahead to say what we don't want (like the core `remove` function).

```clojure
(defn states-without-a-large-city [state-nodes]
  (->> state-nodes
       (neg-lookahead large-cities)))
```

#### Recursive traversal

If the cities have rail connections defined, we can use those to find all available train destinations.

``` clojure
(defn outbound-trains [city-nodes]
  (out :train/city->city city-nodes))

(defn inbound-trains [city-nodes]
  (in :train/city->city city-nodes))
```

By using `all`, we can recursively query each city the train arrives at to find
the available destinations. Cycles are detected and eliminated, but all paths to
each destination will be found, meaning that if the train  network is well
connected, some destinations may be included many times in the results. To
counter that a simple strategy is to add `distinct`.

``` clojure
(defn train-destinations [city-nodes]
  (->> city-nodes (all outbound-trains) distinct)
```

There are many variations on `all`, including `deepest`, `all-cycles`, `all-paths`, `search`, and several others.

All of those functions are built using the `descend` graph traversal power tool which may be used to easily implement 
sophisticated custom graph traversals. 

#### More complex traversal and filtering examples

For more complex examples, please see the [gremlin examples](/test/fermor/gremlin_examples_test.clj) and
[cypher examples](/test/fermor/cypher_examples.clj), where I've taken the most
sophisticated examples present in the documentation of those projects
and translated them to use Fermor.

## Graph algos

I have used this project extensively for the back end of a Sea of Nodes-style
optimizing compiler. For that work  I've needed several graph algorithms, which
I've been accumulating in the `fermor.graph.algo` namespace more-or-less
as-needed. I've also exposed the algos in Bifurcan wherever I could find a good way to do so.

They include `strongly-connected-components`, `shortest-path`, `strongly-connected-subgraphs`, `cycles`
`connected-components`, `biconnected-components`, `articulation-points`, `postwalk`, `reverse-postwalk`, 
`postwalk-reduce`, `prewalk-reduce` `reverse-postwalk-reduce`, `reverse-post-order-numbering`,
`post-order-numbering`, `immediate-dominators`, `dominator-depth`, `dominance-frontiers`, `intervals`, 
`loop-tree`, `breadth-first-nodes`, `breadth-first-reduce`, `non-loop-vertices-between`, `loop-info`.

Most of those algos are implemented in a flexible way allowing you to define the
`predecessor` and `successor` traversals (as needed), enabling the algos to be run even in
complex labeled graphs.

## Pattern matching

My [Pattern](https://github.com/pangloss/pattern) library can be extended to create powerful graph matching. I have used that extensively 
in my internal projects, but highly customized to the projects' specific domain requirements. I would be interested in working 
with someone to develop a set of matchers to include in this project.
