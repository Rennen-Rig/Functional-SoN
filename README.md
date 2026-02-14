# Functional SoN

A Sea-of-Nodes intermediate representation for a pure functional language.

See the [Simple](https://github.com/SeaOfNodes) repo for an explanation on what
Sea of Nodes is.


### Other useful SoN references:
- A more introductory [talk](https://youtu.be/NxiKlnUtyio) on SoN.
The latter half of the talk is much more relevent to this repo.

- An in-depth [talk](https://youtu.be/98lt45Aj8mo) on SoN specifically about
the HotSpot JVM compiler, but the general concepts sitll apply here.


### Motivation
All existing SoN IRs I am aware of are for imperative languages, but the concept
seemed like it would map very well to a functional (especially pure) representation
(as there are less control structures that need to be treated with special behaivour).

