vector-space-map
================

[![Build Status](https://travis-ci.org/conklech/vector-space-map.svg?branch=master)](https://travis-ci.org/conklech/vector-space-map)

vector-space operations for finite maps using Data.Map

Data.Map.Vector provides MapVector, a wrapper around Map from containers which supports constant maps, i.e. maps containing only one value.  This allows an identity under intersection and an Applicative instance.  It also has instances of AdditiveGroup, VectorSpace, InnerSpace, and Num with appropriate value types.  Provides operations for addition, subtraction, element-wise operations (through Applicative), scalar multiplication (through VectorSpace), and dot product.

Also consider Conal Elliott's [total-map package](http://hackage.haskell.org/package/total-map), which provides a different Applicative map.
