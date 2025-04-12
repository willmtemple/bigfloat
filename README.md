# `xfloat` - Arbitrary-precision binary floating point for JavaScript

[![npm](https://img.shields.io/npm/v/xfloat)](https://www.npmjs.com/package/xfloat)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**xfloat** is a small (one ES module with ~600 lines of code, ~12KiB minified), dependency-free JavaScript library for representing and computing with arbitrary-precision binary floating-point numbers. It is implemented in pure JavaScript and relies on native `bigint` for base arithmetic, without the use of limb arrays or external number theory libraries.

## Overview

`xfloat` provides a type `BigFloat`. A `BigFloat` represents numbers in the form `n × 2^e`, where:

- `n` is a signed `bigint` (the significand),
- `e` is a regular JavaScript `number` (the exponent).

This structure allows exact representation of any finite binary-rational number and supports a wide range of operations: arithmetic, comparison, rounding, transcendental functions (like `log`, `exp`, `sqrt`), and precision-controlled division.

The library closely follows IEEE-754 semantics, including handling of `NaN`, `Infinity`, and precision loss in repeating fractions. However, it avoids some of IEEE-754's complexities, such as subnormals and negative zero.

## Features

- Pure JavaScript implementation (no WebAssembly, no native code)
- Zero external dependencies
- Uses `bigint` directly; no "limb" arrays or bignum engines
- Exact representation of binary-rational numbers
- Precise, user-controllable rounding and precision
- IEEE-754-like behavior for infinities, NaN, and division edge cases
- Support for common transcendental functions (`log`, `exp`, `sqrt`, `pow`)
- Includes a set of physical, mathematical, and statistical constants

### Use Cases

`xfloat` is suitable for applications requiring rational numbers with precision beyond `Number.MAX_SAFE_INTEGER` or smaller than the machine epsilon.

### Anti-cases

**`xfloat` is not suitable for financial calculations requiring decimal precision.** `xfloat` is still fundamentally a **binary** representation of numbers. Support for decimally-accurate representations may be added in the future in a separate module, but `xfloat` does not provide this.

## Usage

Install `xfloat` from NPM:

```sh
$ npm i --save xfloat

added 1 package, and audited 2 packages in 871ms

found 0 vulnerabilities
```

### Example

```ts
import BigFloat from 'xfloat';

const a = BigFloat(1.5);               // Convert a JS number to a BigFloat.
const b = BigFloat.fromParts(6n, -2);  // Construct a BigFloat from raw `n` and `e` values.
                                       // This is also `1.5` (not normalized).

const sum = a.add(b);                  // 3F
const sqrt = sum.sqrt(128);            // sqrt(3) with 128 bits of numeric precision.

console.log(sqrt.toString());          // 1.732050807568877293527446341505872366943F
console.log(sqrt.toNumber());          // 1.7320508075688772
console.log(Math.sqrt(3));             // 1.7320508075688772
```

## Design

Internally, a `BigFloat` is defined by:

- `n: bigint` — the significand
- `e: number` — the binary exponent

You can think of a `BigFloat` as being an integer `n` with the radix point moved to the right by `e` bits. So a significand `n` value of 3 with an exponent `e` of 4 is equal to `3 × 2^4`, or `48`. A significand value of 3 with an exponent of `-2` is `3 × 2^-2`, or `3/4 = 0.75`.

Two `BigFloat` values with different values of `n` and `e` can be equal. For example, given a `BigFloat` with `n = 1` and `e = 0` (this is the value 1), and another `BigFloat` with `n = 2` and `e = -1` (`2 × 2^-1 = 1`). However, only one `BigFloat` of any particular numeric value is **normalized**. Normalization ensures that `n` is odd, meaning that the representation is minimal (no trailing zeros in binary). Normalization is optional and can be applied via `.normalize()`, but is also applied by most operations and numbers are normalized as-needed within operations.

### Comparison to Other Libraries

- **`xfloat` is binary-based**, not decimal-based.
- **No limb arrays**: Computations operate directly on `bigint`, offloading most computation to the JavaScript engine itself rather than requiring them to be performed in software.
- **Fully immutable API**: All operations return new `BigFloat` instances.

## Constants

A large collection of scientific and mathematical constants is included, such as:

- `BigFloat.constants.PI`: The ratio of a circle's circumference to its diameter.
- `BigFloat.constants.EULER`: Euler's constant (e).
- `BigFloat.constants.SPEED_OF_LIGHT`: The speed of light in a vacuum.
- `BigFloat.constants.GOLDEN_RATIO`: The golden ratio (φ).
- and many, many others

These are accurate to a reasonable default precision and can be generated with arbitrary precision using `BigFloat.constants(precision)`.

## Caveats

- **Binary only**: `xfloat` cannot exactly represent decimal fractions like 0.1, just as native JavaScript `number` cannot. This is not going to solve your gripes about how "0.2 + 0.1 isn't exactly 0.3." It's going to be _really really close_ to 0.3, but this is commonly misunderstood to be a problem with IEEE-754 binary floating-point numbers (or worse, of JavaScript numbers specifically), it is a limitation of _any_ finite binary numeric representation.
- **Slow with very large precisions**: Division and transcendental functions use iterative algorithms on `bigint` and may become slow with high precision values (hundreds or thousands of bits).
- **Not IEEE-754 compliant**: While behavior is similar, this is not a drop-in IEEE-754 replacement (e.g., no negative zero, no subnormals).
- **No trigonometry**: These are coming, possibly in a separate module to keep the implementation tiny for users who don't need trig.

### Implementation Status

This library is tested with a fuzzing strategy:

```txt
100000 Fuzzing iterations.
Passed: 583756
Failed: 27
Crashed: 0
```

**⚠️ WARNING**: The implementation is incomplete and is of alpha quality:

- Fuzzing is rather incomplete. The 0.0004% of failing fuzzy tests are due to `BigFloat.exp` collapsing to zero when the argument is a negative number with high magnitude (<= -50 or so) because the value of `arg.log2()` is very small and our definition of Euler's constant is too low precision.
- Constants described in the TypeScript API surface are mostly unimplemented, and arbitrary-precision constants are fully unimplemented.
- More thorough testing around edge-cases, rounding modes, etc. is coming.
- Stack traces are a little screwed up right now on account of minification.

## License

[MIT License](./LICENSE)
