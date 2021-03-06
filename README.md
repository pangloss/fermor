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

Bundled with the traversal library is a fast immutable in-memory directed
property graph database built on the very elegant
[Bifurcan](https://github.com/lacuna/bifurcan) library. I use it to
build up graphs of 1-10 million vertices and edges in under 10 seconds, with
all queries I've needed to do so far running in 10-20ms, and full edge counts in
1-2s, all on my laptop.

The Fermor traversal namespace works well with any data source and
there is no dependency between most of the functions in the library and the
included graph.

## Early pre-alpha software

I am using this and tweaking it for my needs as I go. Don't expect *any*
stability until I move it out of alpha.

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
functions. I especially use it when exploring data interactively. These two statements are equivalent:

```clojure
(map (f->> walk chew-gum) data)
(map #(->> % walk chew-gum) data)`.
```

Need to find a better example, and haven't written about the interesting stuff yet...
