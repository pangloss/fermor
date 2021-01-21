![](doc/mani.jpg)

[Fermor](https://github.com/pangloss/fermor) provides a flexible and high
performance streaming data traversal library built upon Clojure's lazy seq
abstraction. It's designed to allow exploration of complex graphs of data,
elegantly handling cycles, deep nesting and other patterns commonly seen in
graph data.

This library is a distillation of my experience creating and working with my
[Pacer](https://github.com/pangloss/pacer) library, which I used to build
diverse applications. Compared to Pacer, Fermor is much lighter weight, more
flexible, simpler and far faster, despite Pacer (as of a few years ago) itself
generally being much faster than other graph traversal mechanisms that I had
seen.

Bundled with the traversal library is a (currently) rudimentary but still useful
fast immutable in-memory directed property graph database built on top of the
very elegant Bifurcan library. However, the Fermor traversal namespace works
well with any data source and there is no dependency between it and the
bifurcan-based graph.

## The traversal library

Like the Clojure core library, the functions in Fermor's traversal library are composable.

The key to using this library effectively is to treat it as a set of primitive building blocks
for creating your own domain-specific library.

In my examples below I will try to follow the pattern of decomposing functions
into small atomic units. This decomposition leads to surprising flexibility
without any performance overhead at all.

### Metadata preservation

The library is metadata-preserving, so provides metadata-preserving wrappers
for the simple arities of the core Clojure seq functions like `map`, `filter`,
`remove`, `take`, `sort`, etc. Preserving metadata is important for traversing the
fermor.bifurcan graph, but if your application does not make use of metadata you
can ignore those functions.

### Introduction

To traverse from one sequence of vertices to a related sequence of vertices
means following an edge in the graph. You can do that using the `in` and `out`
functions: `out` follows edges that originate in each element (the "out edges")
of the sequence and returns the vertex that the edge arrives at. You could think
of it as following edges in their forward direction. Sometimes we want to go the
other way, and we can do that with `in`, which follows edges that point to the
vertices in the sequence and returns the vertices that those edges originate
from.

It's good practice to include both element types in an edge name as I do here.

I'll also use `ensure-seq` everywhere here, which simply wraps elements in a
vector, allowing each helper function to be used either in functions that work
element-by-element, or in functions that work on collections of data. To squeeze
the last nanoseconds of performance in performance critical routes you could
define separate functions that operate per-element and per-sequence.

```clojure
(defn cities [states]
  (->> (ensure-seq states)
       (out :state->city)))

(defn states [cities]
  (->> (ensure-seq cities)
       (in :state->city)))

(defn large-city? [city]
  (< 1000000 (population city)))

```

In Clojure, we `filter` data all the time. We don't need to change that:

```clojure
(defn large-cities [states]
  (->> (ensure-seq states)
       cities
       (filter large-city?)))
```

Lookaheads come in a few varieties. They are filters that match based on whether
there is or isn't the expected data connected to the element at hand.


```clojure
(defn states-with-a-large-city [states]
  (->> (ensure-seq states)
       (lookahead large-cities)))
```

We can also do lookaheads with specific min and max arguments.

```clojure
(defn states-with-2to5-large-cities [states]
  (->> (ensure-seq states)
       (lookahead {:min 2 :max 5} large-cities)))
```

Or do a negative lookahead to say what we don't want (like the core `remove` function).

```clojure
(defn states-without-a-large-city [states]
  (->> (ensure-seq states)
       (neg-lookahead large-cities)))
```

The handy `f->>` macro is very useful for defining nested routes as arguments to
functions. I especially use it when exploring data interactively. Writing `(map)
(f->> walk chew-gum)` is equivalent to writing `(map #(->> % walk chew-gum))`.


Don't like these examples:

```clojure
(defn desireable-attributes [cities]
  (->> cities
       (branch [(f->> (filter low-crime?))
                (f->> (filter good-weather?))
                (f->> (filter good-jobs?))
                (f->> (filter low-prices?))
                (f->> (filter mountains?))
                (f->> (filter coastal?))])
       merge-exhaustive))

(defn desireable-attributes [city]
  (->> city
       (branch [low-crime?
                good-weather?
                good-jobs?
                low-prices?
                mountains?
                coastal?])
       (remove nil?)))

(defn desireable-cities [city]
  (lookahead {:min 4} desireable-attributes))
```
