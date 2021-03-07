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

The following example is admittedly terrible. Until I get a better example set
up, please see the [gremlin examples](/test/fermor/gremlin_examples.clj) and
[cypher examples](/test/fermor/cypher_examples.clj), where I've taken the most
sophisticated examples I've been able to find in those projects' documentation
and translated them to use fermor. More interesting examples are very welcome so
please do send them my way if you know of good ones.

```clojure
(defn cities [states]
  (->> states
       (out :state->city)))

(defn states [cities]
  (->> cities
       (in :state->city)))

(defn large-city? [city]
  (< 1000000 (population city)))

```

In Clojure, we `filter` data all the time. We don't need to change that:

```clojure
(defn large-cities [states]
  (->> states
       cities
       (filter large-city?)))
```

Lookaheads come in a few varieties. They are filters that match based on whether
there is or isn't the expected data connected to the element at hand.


```clojure
(defn states-with-a-large-city [states]
  (->> states
       (lookahead large-cities)))
```

We can also do lookaheads with specific min and max arguments.

```clojure
(defn states-with-2to5-large-cities [states]
  (->> states
       (lookahead {:min 2 :max 5} large-cities)))
```

Or do a negative lookahead to say what we don't want (like the core `remove` function).

```clojure
(defn states-without-a-large-city [states]
  (->> states
       (neg-lookahead large-cities)))
```

