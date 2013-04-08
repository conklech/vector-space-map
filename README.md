vector-space-map
================

vector-space operations for finite maps using Data.Map

Data.Map.Vector provides MapVector, a wrapper around Map from containers which supports constant maps, i.e. maps containing only one value.  This allows an identity under intersection and an Applicative instance.  It also has instances of AdditiveGroup, VectorSpace, InnerSpace, and Num with appropriate value types.  Provides operations for addition, subtraction, element-wise multiplication (through Num), scalar multiplicationn (through VectorSpace), and dot product.
