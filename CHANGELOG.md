0.2.0
=====

- Include reference to `total-map` in the description.
- Bump vector-space dependency to < 0.11.
- Add HasBasis instance. Thanks to Justus Sagemüller for implementing this.
- Add Monoid instance respecting value monoid.
- Remove Num instance. Use `pure` instead of `fromInteger`, and `liftA2 (*)` instead of `(*)`.