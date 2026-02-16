# Functional SoN

A Sea-of-Nodes intermediate representation for a pure functional language.

See the [Simple](https://github.com/SeaOfNodes) repo for an explanation on what
Sea of Nodes is.


### Other useful SoN references:
- A more introductory [talk](https://youtu.be/NxiKlnUtyio) on SoN.
The latter half of the talk is much more relevent to this repo.

- An in-depth [talk](https://youtu.be/98lt45Aj8mo) on SoN specifically about
the HotSpot JVM compiler, but the general concepts sitll apply here.


## The IR
An example program represented as a graph may be seen below

![tail recursive factorial functiom graph](/generated_graphs/example.svg)
This graph represents the factorial function, using a tail-recursive approach.

The graph below does the same, but is not tail-recursive.
![non tail recursive factorial function graph](/generated_graphs/example2.svg)


### Motivation
All existing SoN IRs I am aware of are for imperative languages, but the concept
seemed like it would map very well to a functional (especially pure) representation
(as there are less control structures that need to be treated with special behaivour).

