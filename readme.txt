A simple BigFloat implementation in pure, modern JavaScript.

# Status

Calculating exponents in `exp2BigIntFixed` is a little buggy, especially if the precision value is large, the calculation of `2^2^-i` can overflow JS number. Precision is not great in this method, causing log identities like `ln(exp(a)` to fail because the logarithm collapses to zero.

Constants are mostly not implemented, but Euler's number, Euler-Mascheroni, Pi, SQRT2, and LOG2_10/LOG2_E have values based on the `Math` constants.

Todos:

- Add a `BigFloatMath` namespace with static functions to perform the operations instead of putting everything on `BigFloat`, make `BigFloat` member methods call the `BigFloatMath` versions (this will allow creating precision contexts).
- Rework `precision` so that it is not specifying _additional_ precision but _total final_ precision.

## Tests

```
10000 Fuzzing Iterations
Passed: 57593
Failed: 98
Crashed: 867
```
