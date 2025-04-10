// BigFloat - JavaScript arbitrary-precision binary floating point
// Copyright (c) Will Temple
// SPDX-License-Identifier: MIT

export default BigFloat;

export type AsBigFloat = BigFloat | number | bigint;

/**
 * A binary floating-point number with arbitrary precision.
 *
 * Represented as a number of the form `n * 2^e`, where `n` is an arbitrarily large integer and
 * `e` is an integer exponent with the range and precision of a JavaScript number.
 *
 * The BigFloat value can represent:
 * - A finite number.
 * - A NaN value.
 * - Positive or negative infinity.
 *
 * BigFloat values generally have the semantics of IEEE-754 floating point numbers, but with
 * arbitrary floating point precision.
 *
 * - Operations on NaN values return NaN.
 * - Dividing by zero returns positive or negative infinity, depending on the sign of the dividend.
 * - Dividing zero by zero returns NaN.
 * - Comparisons with NaN always return false.
 *
 * BigFloat differs from IEEE-754 in that:
 *
 * - There is no representation of negative zero.
 * - There is no concept of a normal vs. subnormal number.
 *
 * ## Representation
 *
 * BigFloat values can store an arbitrarily large integer base `n`, and an integer exponent `e` in
 * the range of a JavaScript double-int. This means that BigFloat can represent any multiple of
 * any power of two between `2^-9007199254740991` and `2^9007199254740991`.
 *
 * Since BigFloat is just an expression of multiples of powers of two (it's a binary integer with a
 * movable radix point), it can represent any ratio that terminates in binary exactly.
 *
 */
export declare class BigFloat {
  /**
   * The power of two (exponent) that this BigFloat is a multiple of.
   */
  readonly e: number;

  /**
   * The base factor (significand) of this BigFloat.
   */
  readonly n: bigint;

  /**
   * Creates a new BigFloat.
   *
   * @param n -
   */
  constructor(n?: AsBigFloat);

  /**
   * Returns the normalized form of this BigFloat, or this BigFloat if it is normalized.
   *
   * A BigFloat is considered `normalized` if the base `n` is odd. This means that the number
   * has the smallest representation possible. For example, a BigFloat with a base of `4` and
   * a scale of `-2` is not normalized, but a BigFloat with a base of `1` and a scale of `0`
   * represents the same number and is normalized.
   */
  normalize(): BigFloat;

  /**
   * Returns true if this BigFloat is normalized, false otherwise.
   *
   * A finite BigFloat is considered `normalized` if the base `n` is odd. This means that the
   * number has the smallest representation possible. For example, a BigFloat with a base of `4`
   * and a scale of `-2` is not normalized, but a BigFloat with a base of `1` and a scale of `0`
   * represents the same number and is normalized.
   *
   * An infinite/NaN BigFloat is considered normalized if the base is zero.
   *
   * Beware that BigFloat numbers are objects, and they compare by reference equality.
   */
  isNormalized(): boolean;

  /**
   * Returns true if this BigFloat is a finite number (not positive or negative infinity or NaN),
   * false otherwise.
   */
  isFinite(): boolean;

  /**
   * Returns true if the BigFloat is an integer containing no fractional part (i.e. the exponent of
   * the normalized form would be greater than or equal to zero).
   */
  isInteger(): boolean;

  /**
   * Returns true if the BigFloat is not a number. False otherwise.
   */
  isNaN(): boolean;

  /**
   * Returns true if the BigFloat is zero.
   */
  isZero(): boolean;

  /**
   * Creates a new BigFloat from the given base and scale.
   *
   * This does not return a normalized number.
   *
   * @param base - The base `n` of the number, represented as a bigint.
   * @param scale - The exponent `e` of the number, represented as a number.
   */
  static fromParts(base: bigint, scale: number): BigFloat;

  /**
   * Converts this BigFloat to a JavaScript number.
   *
   * This will return Infinity or -Infinity if the BigFloat is out of range, or NaN if this BigFloat
   * is a NaN value. Otherwise, it will return the closest JavaScript number (IEEE-754 double) to this
   * BigFloat.
   */
  toNumber(): number;

  /**
   * Returns the absolute value of the BigFloat.
   *
   * - `NaN`      : returns NaN.
   * - `Infinity` : returns Infinity.
   * - `-Infinity`: returns Infinity.
   */
  abs(): BigFloat;

  /**
   * Returns the result of adding `addend` to this BigFloat.
   *
   * @param addend - The number to add to this BigFloat.
   */
  add(addend: BigFloat): BigFloat;

  /**
   * Returns the result of subtracting `subtrahend` from this BigFloat.
   *
   * @param subtrahend - The number to subtract from this BigFloat.
   */
  subtract(subtrahend: BigFloat): BigFloat;

  /**
   * Returns the result of multiplying this BigFloat by `factor`.
   *
   * @param multiplicand - The number to multiply this BigFloat by.
   */
  multiply(factor: BigFloat): BigFloat;

  /**
   * Returns the result of dividing this BigFloat by `divisor`.
   *
   * The result may be a cyclic fraction in base 2. For example, `1/3` is `0b0.0101010101...` in base 2.
   * BigFloat cannot represent cyclic expansions losslessly, so this function accepts a `precision`
   * parameter that specifies the number of additional fractional bits to keep in the result. By default,
   * we keep 64 bits of additional binary precision, but this can be changed to any positive integer.
   *
   * Beware that if you pick a very large precision value, the computation may become extremely inefficient
   * as we use bigint division to compute the result. If the value is absurdly large, the JavaScript engine
   * may fail to allocate the underlying integer. Specifying additional precision is not a silver bullet.
   *
   * @param divisor - The number to divide this BigFloat by.
   * @param precision - The precision to use for the division. Default is 64.
   */
  divide(divisor: BigFloat, precision?: number): BigFloat;

  /**
   * Returns the remainder of division of this BigFloat by `divisor`.
   *
   * The result may involve computing cyclic divisions, so this function accepts a `precision`
   * parameter that specifies the number of additional fractional bits to keep in the result. By default,
   * we keep 64 bits of additional binary precision, but this can be changed to any positive integer.
   *
   * Beware that if you pick a very large precision value, the computation may become extremely inefficient
   * as we use bigint division to compute the result. If the value is absurdly large, the JavaScript engine
   * may fail to allocate the underlying integer. Specifying additional precision is not a silver bullet.
   *
   * @param divisor - The number to divide this BigFloat by.
   * @param precision - The precision to use for the division. Default is 64.
   */
  mod(divisor: BigFloat, precision?: number): BigFloat;

  remainder(divisor: BigFloat, precision?: number): BigFloat;

  /**
   * Returns the negative of this BigFloat.
   */
  negate(): BigFloat;

  /**
   * Returns the result of raising this BigFloat to the power of `exponent`.
   *
   * @param exponent - The exponent to raise this BigFloat to.
   * @param precision - The precision to use for the exponentiation. Default is 64.
   */
  pow(exponent: BigFloat, precision?: number): BigFloat;

  /*, precision*
   * Returns the logarithm of this BigFloat with the given base.
   *
   * @param base - The base to use for the logarithm.
   */
  log(base: BigFloat, precision?: number): BigFloat;

  /**
   * Returns the logarithm of this BigFloat in base 2.
   */
  log2(precision?: number): BigFloat;

  /**
   * Returns the logarithm of this BigFloat in base 10.
   */
  log10(precision?: number): BigFloat;

  /**
   * Returns the natural logarithm of this BigFloat.
   */
  ln(precision?: number): BigFloat;

  /**
   * Returns the square root of this BigFloat.
   */
  sqrt(precision?: number): BigFloat;

  /**
   * Rounds this BigFloat towards `precision` bits of precision after the radix point.
   *
   * For positive numbers:
   * - Rounds up if the fractional part is 1/2 or greater.
   * - Rounds down if the fractional part is less than 1/2.
   *
   * For negative numbers:
   * - Rounds up if the fractional part is greater than -1/2.
   * - Rounds down if the fractional part is less than or equal to -1/2.
   *
   * @param precision - The number of bits of precision to keep after the radix point. Default: 0.
   */
  round(precision?: number): BigFloat;

  /**
   * Rounds this BigFloat towards positive Infinity, maintaining `precision` bits of precision after the radix point.
   *
   * @param precision - The number of bits of precision to keep after the radix point. Default: 0.
   */
  ceil(precision?: number): BigFloat;

  /**
   * Rounds this BigFloat towards negative Infinity, maintaining `precision` bits of precision after the radix point.
   *
   * @param precision - The number of bits of precision to keep after the radix point. Default: 0.
   */
  floor(precision?: number): BigFloat;

  /**
   * Rounds this BigFloat towards zero, maintaining `precision` bits of precision after the radix point.
   *
   * You can use this to round towards any power of two. For example:
   *
   * - `BigFloat(10).trunc(3)` will return `8` (`0b100`).
   * - `BigFloat(1.5).trunc(0)` will return `1`.
   * - `BigFloat(1.6).trunc(-1)` will return `1.5` (`0b1.1`).
   *
   * @param precision - The number of bits of precision to keep after the radix point. Default: 0.
   */
  trunc(precision?: number): BigFloat;

  // Comparators

  /**
   * Compares this BigFloat to another BigFloat, returning `-1` if this BigFloat is less than the other,
   * `0` if they are equal, and `1` if this BigFloat is greater than the other.
   *
   * NOTE: Attempting to compare a `NaN` value will throw an Error.
   *
   * @param other - The number to compare this BigFloat to.
   * @returns -1 if this BigFloat is less than the other, 0 if they are equal, and 1 if this BigFloat is greater than the other.
   * @throws Error if either BigFloat is NaN.
   */
  compare(other: BigFloat): -1 | 0 | 1;

  /**
   * Compares this BigFloat to another BigFloat, returning `0` if this BigFloat is within `epsilon` of the other,
   * `-1` if this BigFloat is less than the other, and `1` if this BigFloat is greater than the other.
   *
   * NOTE: Attempting to compare a `NaN` value will throw an Error.
   *
   * @param other - The number to compare this BigFloat to.
   * @param epsilon - The maximum difference between this BigFloat and `other` for them to be considered equal. Default: `BigFloat.constants.MACHINE_EPSILON_DOUBLE`.
   * @returns -1 if this BigFloat is less than the other, 0 if they are equal, and 1 if this BigFloat is greater than the other.
   * @throws Error if either BigFloat is NaN.
   */
  compareNear(other: BigFloat, epsilon?: BigFloat): -1 | 0 | 1;

  /**
   * Returns true if this BigFloat is less than the given BigFloat.
   *
   * NOTE: Comparisons with NaN always return false.
   *
   * @param other - The number to compare this BigFloat to.
   */
  lt(other: BigFloat): boolean;

  /**
   * Returns true if this BigFloat is less than or equal to the given BigFloat.
   *
   * NOTE: Comparisons with NaN always return false.
   *
   * @param other - The number to compare this BigFloat to.
   * @see {@link BigFloat#eq}
   */
  leq(other: BigFloat): boolean;
  /**
   * Returns true if this BigFloat is greater than the given BigFloat.
   *
   * NOTE: Comparisons with NaN always return false.
   *
   * @param other - The number to compare this BigFloat to.
   */
  gt(other: BigFloat): boolean;
  /**
   * Returns true if this BigFloat is greater than or equal to the given BigFloat.
   *
   * NOTE: Comparisons with NaN always return false.
   *
   * @param other - The number to compare this BigFloat to.
   * @see {@link BigFloat#eq}
   */
  geq(other: BigFloat): boolean;
  /**
   * Returns true if this BigFloat is equal to the given BigFloat.
   *
   * NOTES:
   * - Comparisons with `NaN` always return false. Even `BigFloat.NAN.eq(BigFloat.NAN)` returns false.
   * - Equality is defined as having the same binary information. The only way two BigFloat numbers with
   *   different scales can be equal is if one of them is not normalized. Two finite BigFloat numbers are
   *   equal if, after converting both to the same scale, the bases are exactly equal.
   * - Often, comparing two BigFloat numbers with different scales will return false, even if they are
   *   _very_ close to each other. See {@link BigFloat#near} for a way to compare two BigFloat numbers
   *   within a small epsilon range.
   *
   * @param other - The number to compare this BigFloat to.
   */
  eq(other: BigFloat): boolean;

  /**
   * Returns true if the difference between this BigFloat and `other` is strictly less than `epsilon`.
   *
   * @param other - The number to compare this BigFloat to.
   * @param epsilon - The maximum difference between this BigFloat and `other` for them to be considered near. Default: `BigFloat.constants.MACHINE_EPSILON_DOUBLE`.
   */
  near(other: BigFloat, epsilon?: BigFloat): boolean;

  /**
   * Computes an exponential (e^x) as a BigFloat.
   *
   * @param power - the natural power to compute.
   * @param precision - the number of bits of precision to keep. Default: 64.
   */
  static exp(power: BigFloat, precision?: number): BigFloat;

  /**
   * Creates the smallest positive BigFloat with the given scale.
   *
   * This function always returns a normalized BigFloat.
   *
   * @param scale - The scale of the BigFloat.
   * @returns A BigFloat representing the smallest positive number with the given scale.
   * @throws Error if the scale is not a finite integer.
   *
   * @example
   * ```ts
   * // Creates the value `2^-10`, the smallest positive BigFloat with scale -10
   * const epsilon = BigFloat.epsilon(-10);
   *
   * // Epsilon values can be useful to determine if two BigFloat numbers are very close to each other,
   * // which can be important when dealing with arbitrary precision numbers.
   * const a = BigFloat(1.0000000001);
   * const b = BigFloat(1.0000000002);
   *
   */
  static epsilon(scale: number): BigFloat;

  static constants: BigFloatConstants;

  /**
   * The normalized form of BigFloat NaN.
   */
  static NAN: BigFloat;

  /**
   * The normalized form of BigFloat positive infinity (+∞).
   */
  static INFINITY: BigFloat;

  /**
   * The normalized form of BigFloat negative infinity (-∞).
   */
  static NEGATIVE_INFINITY: BigFloat;
}

export interface BigFloatConstants extends PreciseBigFloatConstants {
  /**
   * Returns a record of BigFloat constants where analytically computable constants have the specified precision
   * after the radix point.
   */
  (precision: number): PreciseBigFloatConstants;

  /**
   * A constant representing the ratio of a circle's circumference to its diameter (π), which is approximately
   * equal to 3.14159 that arises in various geometric and trigonometical calculations.
   */
  PI: BigFloat;

  /**
   * A constant representing Euler's number (e), the base of the natural logarithm, which is approximately
   * equal to 2.71828 that arises in various mathematical calculations, particularly in calculus and complex
   * analysis relating to exponential growth and decay.
   */
  EULER: BigFloat;

  /**
   * A constant representing Euler-Mascheroni constant (γ), which is approximately equal to 0.57721 that
   * arises in number theory and analysis in the study of harmonic series and integrals.
   */
  EULER_MASCHERONI: BigFloat;

  /**
   * A constant representing the smallest positive number such that 1 + ε = 1 in double-precision floating
   * point arithmetic, approximately equal to 2.22 x 10^-16.
   */
  MACHINE_EPSILON_DOUBLE: BigFloat;

  /**
   * A constant representing the speed of light in vacuum (c), which is approximately equal to
   * 299792458 m/s, a fundamental constant in physics that is used in various equations
   * and theories, including Einstein's theory of relativity.
   */
  SPEED_OF_LIGHT: BigFloat;

  /**
   * A constant representing Newton's gravitational constant (G), which is approximately equal to
   * 6.67430 x 10^-11 m^3 kg^-1 s^-2, a fundamental constant in physics that describes the
   * strength of the gravitational force between two objects.
   */
  GRAVITATION: BigFloat;

  /**
   * A constant representing Planck's constant (h), which is approximately equal to
   * 6.62607015 x 10^-34 J s, a fundamental constant in quantum mechanics that relates
   * the energy of a photon to its frequency.
   */
  PLANCK: BigFloat;

  /**
   * A constant representing the Boltzmann constant (k), which is approximately equal to
   * 1.380649 x 10^-23 J/K, a fundamental constant in statistical mechanics that relates
   * the average kinetic energy of particles in a gas to the temperature of the gas.
   */
  BOLTZMANN: BigFloat;

  /**
   * A constant representing the Avogadro constant (NA), which is approximately equal to
   * 6.02214076 x 10^23 mol^-1, a fundamental constant in chemistry that relates the number
   * of particles in a mole to the amount of substance.
   */
  AVOGADRO: BigFloat;

  /**
   * A constant representing the gas constant (R), which is approximately equal to
   * 8.314 J/(mol K), a fundamental constant in thermodynamics that relates the pressure,
   * volume, and temperature of an ideal gas.
   */
  IDEAL_GAS: BigFloat;

  /**
   * A constant representing the fine-structure constant (α), which is approximately equal to
   * 1/137, a dimensionless constant that characterizes the strength of electromagnetic interactions.
   */
  FINE_STRUCTURE: BigFloat;

  /**
   * A constant representing the elementary charge (e), which is approximately equal to
   * 1.602176634 x 10^-19 C, a fundamental constant in electromagnetism that represents the
   * charge of a single proton or the magnitude of the charge of an electron.
   */
  ELEMENTARY_CHARGE: BigFloat;

  /**
   * A constant representing the permeability of free space (μ0), which is approximately equal to
   * 4π x 10^-7 N/A^2, a fundamental constant in electromagnetism that describes the magnetic
   * permeability of a classical vacuum.
   */
  PERMEABILITY: BigFloat;

  /**
   * A constant representing the permittivity of free space (ε0), which is approximately equal to
   * 8.854187817 x 10^-12 F/m, a fundamental constant in electromagnetism that describes the
   * electric permittivity of a classical vacuum.
   */
  PERMITTIVITY: BigFloat;

  /**
   * A constant representing the Stefan-Boltzmann constant (σ), which is approximately equal to
   * 5.670374419 x 10^-8 W/(m^2 K^4), a fundamental constant in thermodynamics that describes
   * the power radiated by a black body in terms of its temperature.
   */
  STEFAN_BOLTZMANN: BigFloat;

  /**
   * A constant representing the square root of 2π (√(2π)), which is approximately equal to
   * 2.506628, a constant that arises in the Gaussian normal distribution and is used in various
   * statistical calculations.
   */
  SQRT_2PI: BigFloat;

  /**
   * A constant representing Catalan's constant (G), which is approximately equal to
   * 0.915965594, a mathematical constant that arises in various combinatorial problems
   * and number theory.
   */
  CATALAN: BigFloat;

  /**
   * A constant representing the Feigenbaum constant (δ), which is approximately equal to
   * 4.669201609, a mathematical constant that arises in chaos theory and bifurcation theory.
   */
  FEIGENBAUM_DELTA: BigFloat;

  /**
   * A constant representing the Feigenbaum constant (α), which is approximately equal to
   * 2.502907875, a mathematical constant that arises in chaos theory and bifurcation theory.
   */
  FEIGENBAUM_ALPHA: BigFloat;
}

export interface PreciseBigFloatConstants {
  /**
   * A constant representing the square root of 2 (√2), which is approximately equal to 1.41421.
   */
  SQRT2: BigFloat;

  /**
   * A constant representing the square root of 3 (√3), which is approximately equal to 1.73205.
   */
  SQRT3: BigFloat;

  /**
   * A constant representing the square root of 5 (√5), which is approximately equal to 2.23607.
   */
  SQRT5: BigFloat;

  /**
   * A constant representing the natural logarithm of 2 (ln(2)), which is approximately equal to 0.69315.
   *
   * This constant is useful for converting between logarithmic bases.
   */
  LN2: BigFloat;

  /**
   * A constant representing the natural logarithm of 10 (ln(10)), which is approximately equal to 2.30259.
   *
   * This constant is useful for converting between logarithmic bases.
   */
  LN10: BigFloat;

  /**
   * A constant representing the base-2 logarithm of 10 (log2(10)), which is approximately equal to 3.32193.
   */
  LOG2_10: BigFloat;

  /**
   * A constant representing the base-2 logarithm of Euler's number (log2(e)), which is approximately equal to 1.44269.
   */
  LOG2_E: BigFloat;

  /**
   * A constant representing the golden ratio (φ), which is approximately equal to 1.61803.
   *
   * This constant is often found in mathematics, art, and nature.
   */
  GOLDEN_RATIO: BigFloat;
}

const NORMALIZED = Symbol("normalized");
const CONVERSION_BUFFER = new ArrayBuffer(8);
const CONVERSION_VIEW = new DataView(CONVERSION_BUFFER);

export function BigFloat(this: BigFloat, n?: AsBigFloat): BigFloat;
export function BigFloat(n: AsBigFloat): BigFloat;
export function BigFloat(this: BigFloat, n: AsBigFloat = 0): BigFloat {
  const target: { -readonly [K in keyof BigFloat]: BigFloat[K] } =
    this instanceof BigFloat ? this : Object.create(BigFloat.prototype);

  if (n instanceof BigFloat) {
    target.n = n.n;
    target.e = n.e;
    return Object.seal(target) as BigFloat;
  }

  if (n === 0) {
    target.n = 0n;
    target.e = 0;
    return Object.seal(target) as BigFloat;
  } else {
    if (Number.isNaN(n)) {
      target.n = 0n;
      target.e = NaN;
      return Object.seal(target) as BigFloat;
    } else if (n === Infinity) {
      target.n = 0n;
      target.e = Infinity;
      return Object.seal(target) as BigFloat;
    } else if (n === -Infinity) {
      target.n = 0n;
      target.e = -Infinity;
      return Object.seal(target) as BigFloat;
    } else if (typeof n === "bigint") {
      target.n = n;
      target.e = 0;
      return Object.seal(target) as BigFloat;
    } else {
      const { mantissa, exponent } = extractMantissaAndExponent(n);

      target.n = BigInt(mantissa);
      target.e = exponent;
      return Object.seal(target) as BigFloat;
    }

    const bf = _normalize(target as BigFloat);
    target.n = bf.n;
    target.e = bf.e;
    return Object.seal(target) as BigFloat;
  }

  function extractMantissaAndExponent(num: number): {
    mantissa: number;
    exponent: number;
  } {
    CONVERSION_VIEW.setFloat64(0, num);

    const high = CONVERSION_VIEW.getUint32(0); // High 32 bits
    const low = CONVERSION_VIEW.getUint32(4); // Low 32 bits

    const sign = high >>> 31;
    const exponentBits = (high >>> 20) & 0x7ff;
    const fractionHigh = high & 0xfffff;
    const fractionLow = low;

    let exponent, mantissa;

    if (exponentBits === 0) {
      // Subnormal numbers
      exponent = -1022 - 52;
      mantissa = fractionHigh * Math.pow(2, 32) + fractionLow;
    } else {
      // Normalized numbers
      exponent = exponentBits - 1023 - 52;
      mantissa = (fractionHigh | (1 << 20)) * Math.pow(2, 32) + fractionLow;
    }

    // Adjust sign
    if (sign) {
      mantissa = -mantissa;
    }

    return { mantissa, exponent };
  }
}

BigFloat[NORMALIZED] = new WeakMap<BigFloat, BigFloat>();

BigFloat.fromParts = function fromParts(base: bigint, scale: number): BigFloat {
  const target: { -readonly [K in keyof BigFloat]: BigFloat[K] } =
    Object.create(BigFloat.prototype);

  target.e = scale;
  target.n = base;

  return Object.seal(target) as BigFloat;
};

BigFloat.prototype.toNumber = function toNumber(this: BigFloat): number {
  if (!this.isFinite()) {
    if (this.e === Infinity) return Infinity;
    if (this.e === -Infinity) return -Infinity;
    return NaN;
  } else if (this.isZero()) return 0;

  let { n, e } = this;

  const sign = n < 0n ? 1 : 0;
  n = n < 0n ? -n : n;

  const EXPONENT_BIAS = (1 << 10) - 1;

  // Get bit length of n without using toString
  function bitLength(x: bigint): number {
    let bits = 0;
    while (x !== 0n) {
      x >>= 1n;
      bits++;
    }
    return bits;
  }

  const nBits = bitLength(n);
  let floatExp = e + nBits - 1;

  if (floatExp > 1023) return sign ? -Infinity : Infinity;
  if (floatExp < -1075) return sign ? -0 : 0;

  let mantissa = n;

  // Shift mantissa to fit into 53 bits
  const shift = nBits - 53;
  if (shift > 0) {
    const extra = (mantissa >> BigInt(shift - 1)) & 0b11n;
    mantissa = (mantissa >> BigInt(shift)) + (extra === 0b11n ? 1n : 0n);
    if (mantissa >> 53n) {
      mantissa >>= 1n;
      floatExp++;
    }
  } else {
    mantissa <<= BigInt(-shift);
  }

  // Subnormal case
  if (floatExp <= -1023) {
    const subShift = -1022 - floatExp;
    if (subShift >= 53) {
      mantissa = 0n;
    } else {
      mantissa >>= BigInt(subShift);
    }
    floatExp = -1023;
  }

  const biasedExp = floatExp + EXPONENT_BIAS;
  const mantissaBits = mantissa & ((1n << 52n) - 1n);

  const hi =
    (sign << 31) | ((biasedExp & 0x7ff) << 20) | Number(mantissaBits >> 32n);
  const lo = Number(mantissaBits & 0xffffffffn);

  CONVERSION_VIEW.setUint32(0, lo, true); // little-endian
  CONVERSION_VIEW.setUint32(4, hi, true); // little-endian

  return CONVERSION_VIEW.getFloat64(0, true);
};

// TODO: implement all the constants.
BigFloat.constants = Object.assign(
  function preciseConstants(_precision: number) {
    return BigFloat.constants;
  },
  {
    EULER: BigFloat.fromParts(
      0b101011011111100001010100010110001010001010111011010010101001101011n,
      -64
    ),
    EULER_MASCHERONI: BigFloat(0.5772156649015328606),
    PI: BigFloat(Math.PI),
    SQRT2: BigFloat(Math.SQRT2),
    LOG2_10: BigFloat.fromParts(
      0b110101001001101001111000010010111100110100011011100010101111111001n,
      -64
    ),
    LOG2_E: BigFloat.fromParts(
      0b10111000101010100011101100101001010111000001011111110000101110111n,
      -64
    ),
  }
) as any;

BigFloat.exp = function exp(power: BigFloat, precision: number = 64) {
  return BigFloat.constants.EULER.pow(power, precision);
};

BigFloat.NAN = BigFloat(NaN);
BigFloat.INFINITY = BigFloat(Infinity);
BigFloat.NEGATIVE_INFINITY = BigFloat(-Infinity);

BigFloat.prototype.toString = function toString(this: BigFloat): string {
  if (this.isNaN()) return "NaNF";
  if (!this.isFinite()) return this.e < 0 ? "-InfinityF" : "InfinityF";
  if (this.isZero()) return "0F";

  const [sign, abs] = this.n < 0n ? ["-", -this.n] : ["", this.n];

  const wholePart = abs >> BigInt(-this.e);
  if (this.e < 0) {
    let fractionalPart = abs % (1n << BigInt(-this.e));

    // IDEA: Get as many bits of the fractional part as will fit in a safeint, convert to a Number, then divide by 2 ^ scale.
    //       This gives a number between 0 and 1, which we can print as a decimal number 0.<fractionalPart> using toFixed.
    //       Finally, slice the string to get the fractional string.

    // Adjust for precision, allow up to 48 bits of significant bits in string repr.
    fractionalPart >>= BigInt(-this.e - 48);

    // Magic number: 281474976710656 = 2^48
    const fractionalStr = Number(fractionalPart) / 281474976710656;
    const fractionString = fractionalStr.toString().slice(2); // Remove "0."

    // Print Base 10, then if the fractional part is non-zero, print it after a decimal point.

    return `${sign}${wholePart}.${fractionString}F`;
  } else {
    return `${sign}${wholePart}F`;
  }
};

BigFloat.prototype.normalize = function normalize(this: BigFloat): BigFloat {
  // Check if already normalized
  if (this.isNormalized()) return this;

  let normalized = BigFloat[NORMALIZED].get(this);
  if (normalized) return normalized;
  normalized = _normalize(this);
  BigFloat[NORMALIZED].set(this, normalized);
  return normalized;
};

function _normalize(f: BigFloat): BigFloat {
  if (isNaN(f.e)) return BigFloat.NAN;
  if (f.e === Infinity) return BigFloat.INFINITY;
  if (f.e === -Infinity) return BigFloat.NEGATIVE_INFINITY;

  if (f.n === 0n) return BigFloat.fromParts(0n, 0);

  const [sign, abs] = f.n < 0n ? [-1n, -f.n] : [1n, f.n];

  // Find the greatest power of two that divides the base exactly.
  const pow2 = abs & -abs;

  const diff = leastSignificantBitIndex(pow2);

  return BigFloat.fromParts(sign * (abs >> BigInt(diff)), f.e + diff);
}

/**
 * Finds the index of the least significant bit in the binary representation of a number.
 *
 * @param n - The number to find the least significant bit index of, must be a positive integer.
 * @returns The index of the least significant bit in the binary representation of n.
 */
function leastSignificantBitIndex(n: bigint): number {
  let low = 0;
  let high = 1;

  // Exponentially find an upper bound
  while (test(high)) {
    low = high;
    high <<= 1;
    if (high > Number.MAX_SAFE_INTEGER) {
      throw new Error("BigFloat exponent overflow");
    }
  }

  // Binary search between low and high
  while (high > low) {
    const mid = Math.floor((low + high) / 2);
    if (test(mid)) {
      low = mid + 1;
    } else {
      high = mid;
    }
  }

  return low - 1;

  // returns false if bitpos is too high
  function test(bitpos: number): boolean {
    return n % (1n << BigInt(bitpos)) === 0n;
  }
}

BigFloat.prototype.isNormalized = function isNormalized(
  this: BigFloat
): boolean {
  if (!this.isFinite()) return this.n === 0n;
  if (this.n === 0n) return this.e === 0;

  const abs = this.n < 0n ? -this.n : this.n;

  // The BigFloat is normalized if the base is odd.
  return (abs & 1n) === 1n;
};

BigFloat.prototype.isFinite = function isFinite(this: BigFloat): boolean {
  return !this.isNaN() && this.e !== Infinity && this.e !== -Infinity;
};

BigFloat.prototype.isInteger = function isInteger(this: BigFloat): boolean {
  return this.normalize().e >= 0;
};

BigFloat.prototype.isNaN = function isNaN(this: BigFloat): boolean {
  return Number.isNaN(this.e);
};

BigFloat.prototype.isZero = function isZero(this: BigFloat): boolean {
  return this.n === 0n;
};

BigFloat.prototype.abs = function abs(this: BigFloat): BigFloat {
  return BigFloat.fromParts(
    this.n < 0n ? -this.n : this.n,
    this.e === -Infinity ? Infinity : this.e
  );
};

BigFloat.prototype.negate = function negate(this: BigFloat): BigFloat {
  return BigFloat.fromParts(-this.n, this.e);
};

BigFloat.prototype.add = function add(
  this: BigFloat,
  other: BigFloat
): BigFloat {
  if (this.isNaN() || other.isNaN()) {
    return BigFloat.NAN;
  }

  if (this.e === Infinity && other.e === -Infinity) return BigFloat.NAN;
  if (this.e === -Infinity && other.e === Infinity) return BigFloat.NAN;
  if (this.e === Infinity || other.e === Infinity) return BigFloat.INFINITY;
  if (this.e === -Infinity || other.e === -Infinity)
    return BigFloat.NEGATIVE_INFINITY;

  if (this.isZero()) return other;
  if (other.isZero()) return this;

  let [n1, e1, n2, e2] = [this.n, this.e, other.n, other.e];

  if (e1 === e2) {
    return _normalize(BigFloat.fromParts(n1 + n2, e1));
  }

  // Align the numbers by shifting the smaller one
  if (e1 > e2) {
    [n1, e1, n2, e2] = [n2, e2, n1, e1];
  }

  const eDiff = e2 - e1;

  const resultN = n1 + (n2 << BigInt(eDiff));

  return _normalize(BigFloat.fromParts(resultN, e1));
};

BigFloat.prototype.subtract = function subtract(
  this: BigFloat,
  other: BigFloat
): BigFloat {
  if (this.isNaN() || other.isNaN()) {
    return BigFloat.NAN;
  }

  if (this.e === Infinity && other.e === Infinity) return BigFloat.NAN;
  if (this.e === -Infinity && other.e === -Infinity) return BigFloat.NAN;
  if (this.e === Infinity && other.e === -Infinity) return this;
  if (this.e === -Infinity && other.e === Infinity) return this;

  if (this.isZero()) return other.negate();
  if (other.isZero()) return this;

  if (this.e === other.e) {
    return _normalize(BigFloat.fromParts(this.n - other.n, this.e));
  }

  // Adjust to same scale
  const scale = Math.min(this.e, other.e);
  const thisBase = this.n << BigInt(this.e - scale);
  const otherBase = other.n << BigInt(other.e - scale);

  return _normalize(BigFloat.fromParts(thisBase - otherBase, scale));
};

BigFloat.prototype.multiply = function multiply(
  this: BigFloat,
  other: BigFloat
): BigFloat {
  if (this.isNaN() || other.isNaN()) {
    return BigFloat.NAN;
  }

  return _normalize(BigFloat.fromParts(this.n * other.n, this.e + other.e));
};

BigFloat.prototype.divide = function divide(
  this: BigFloat,
  other: BigFloat,
  precision: number = 64
): BigFloat {
  if (precision < 1 || !Number.isSafeInteger(precision))
    throw new Error(
      `BigFloat.divide precision must be a positive safe integer, found '${precision}'`
    );

  if (this.isNaN() || other.isNaN()) {
    return BigFloat.NAN;
  }

  if (other.isZero()) {
    return this.isZero()
      ? BigFloat.NAN
      : this.e < 0
      ? BigFloat.NEGATIVE_INFINITY
      : BigFloat.INFINITY;
  }

  if (this.isZero()) return BigFloat.fromParts(0n, 0);

  // We are trying to divide two numbers a and b of the form:
  // a = a_n * 2^a_e
  // b = b_n * 2^b_e
  //
  // So we have:
  // a/b = (a_n * 2^a_e) / (b_n * 2^b_e)
  // a/b = (a_n / b_n) * 2^(a_e - b_e)
  //
  // Problem: a_n / b_n uses integer division, truncating the result and leading to a loss of precision.
  // Solution: multiply a_n by a large power of two `p` so that even if the division truncates, it will
  // do so with a lot of precision, then divide the result by the same power of two.
  //
  // a/b = (a_n * 2^p / b_n) * (2^(a_e - b_e) / 2^p)
  //
  // Analytic Result: a/b = (a_n * 2^p / b_n) * 2^(a_e - b_e - p)

  return _normalize(
    BigFloat.fromParts(
      (this.n << BigInt(precision)) / other.n,
      this.e - other.e - precision
    )
  );
};

BigFloat.prototype.mod = function mod(
  this: BigFloat,
  other: BigFloat
): BigFloat {
  if (this.isNaN() || other.isNaN()) return BigFloat.NAN;

  if (other.isZero()) return BigFloat.NAN;

  if (this.isZero()) return BigFloat(0);

  if (
    this.e === Infinity ||
    this.e === -Infinity ||
    other.e === Infinity ||
    other.e === -Infinity
  ) {
    return BigFloat.NAN;
  }

  // q = floor(this / other)
  const q = this.divide(other).floor();

  // r = this - q * other
  const r = this.subtract(q.multiply(other));

  return r;
};

BigFloat.prototype.sqrt = function sqrt(
  this: BigFloat,
  precision: number = 64
): BigFloat {
  if (this.isNaN() || this.e === -Infinity || this.n < 0n) {
    return BigFloat.NAN;
  }

  if (this.isZero()) return this;

  let { n: base, e: scale } = this;

  base <<= BigInt(precision * 2);
  scale -= precision * 2;

  if (scale % 2 !== 0) {
    base <<= 1n;
    scale -= 1;
  }

  const sqrtBase = bigintSqrt(base);

  return _normalize(BigFloat.fromParts(sqrtBase, scale / 2));

  // Heron's method (Newton-Raphson) for bigints
  function bigintSqrt(base: bigint): bigint {
    if (base < 2n) return base;

    let x0 = base;
    let x1 = (base >> 1n) + 1n;

    while (x1 < x0) {
      x0 = x1;
      x1 = (base / x1 + x1) >> 1n;
    }

    return x0;
  }
};

BigFloat.prototype.pow = function pow(
  this: BigFloat,
  exponent: BigFloat,
  _precision: number = 64
): BigFloat {
  if (this.isNaN() || exponent.isNaN()) {
    return BigFloat.NAN;
  }

  if (this.isZero()) {
    return exponent.isZero() ? BigFloat.NAN : BigFloat(0);
  }

  if (exponent.isZero()) {
    return BigFloat.fromParts(1n, 0);
  }

  if (this.isInteger() && exponent.isInteger()) {
    // Fast path for the case where both numbers are integers.
    const basePow = this.n ** exponent.n;
    const scalePow = BigInt(this.e) * exponent.n;
    return _normalize(BigFloat.fromParts(basePow, Number(scalePow)));
  }

  const precision = BigInt(_precision);

  const [n1, e1, n2, e2] = [this.n, this.e, exponent.n, exponent.e];

  const log2N = log2BigIntFixed(n1, precision);
  const log2A = log2N + (BigInt(e1) << precision);

  const bFixed = (n2 << precision) >> BigInt(-e2);

  const log2Result = (log2A * bFixed) >> precision;

  const resultFixed = exp2BigIntFixed(log2Result, precision);

  return _normalize(BigFloat.fromParts(resultFixed, -Number(precision)));
};

// Experimental

function fixedLn2(precision: bigint, terms: bigint = 256n) {
  const one = 1n << precision;
  let result = 0n;

  for (let k = 1n; k <= terms; k++) {
    const denom = k << k; // k * 2^k
    const term = one / denom;
    result += term;
    if (term === 0n) break;
  }

  return result;
}

function fixedExp(y: bigint, precision: bigint, terms: bigint = 32n) {
  let result = 1n << precision; // Start with 1.0 in fixed-point
  let term = result;
  for (let i = 1n; i < terms; i++) {
    term = (term * y) / (i << precision);
    result += term;
    if (term === 0n) break; // Converged
  }
  return result;
}

function precomputeExp2Of2NegX(precision: bigint) {
  const one = 1n << precision;
  const ln2 = fixedLn2(precision);
  const table: bigint[] = [];

  for (let x = 1n; x <= precision; x++) {
    const twoNegX = one >> x; // 2^-x in fixed-point
    const exponent = (twoNegX * ln2) >> precision; // ln(2) * 2^-x
    const value = fixedExp(exponent, precision); // exp(ln(2) * 2^-x)
    table.push(value);
  }

  return table;
}

const EXP2_TABLE = Symbol("EXP2_TABLE");

function getPrecomputedExp2Table(precision: bigint): bigint[] {
  const precomputed = ((BigFloat as any)[EXP2_TABLE] ??= new Map<
    bigint,
    bigint[]
  >());

  let table = precomputed.get(precision);
  if (!table) {
    table = precomputeExp2Of2NegX(precision);
    precomputed.set(precision, table);
  }
  return table;
}

function exp2BigIntFixed(x: bigint, precision: bigint): bigint {
  const intPart = x >> precision;
  const fracPart = x & ((1n << precision) - 1n);

  const ONE = 1n << precision;

  // Precomputed constants: 2^(2^-i) in fixed-point
  const powersOfTwo: bigint[] = getPrecomputedExp2Table(precision);

  // Multiply all relevant 2^{2^-i} factors for set bits
  let result = ONE;
  for (let i = 0n; i < precision; i++) {
    const bitIndex = precision - 1n - i;
    if ((fracPart >> bitIndex) & 1n) {
      result = (result * powersOfTwo[Number(i)]) >> precision;
    }
  }

  // Shift result by intPart
  return result << intPart;
}

BigFloat.prototype.log = function log(
  this: BigFloat,
  base: BigFloat,
  precision: number = 64
): BigFloat {
  return this.log2(precision).divide(base.log2(precision), precision);
};

BigFloat.prototype.log10 = function log10(
  this: BigFloat,
  precision: number = 64
): BigFloat {
  return this.log2(precision).divide(
    BigFloat.constants(precision).LOG2_10,
    precision
  );
};

BigFloat.prototype.ln = function ln(
  this: BigFloat,
  precision: number = 64
): BigFloat {
  return this.log2(precision).divide(
    BigFloat.constants(precision).LOG2_E,
    precision
  );
};

BigFloat.prototype.log2 = function log2(
  this: BigFloat,
  _precision: number = 48
): BigFloat {
  if (!this.isFinite()) {
    if (this.e === Infinity) return BigFloat.INFINITY;
    return BigFloat.NAN;
  } else if (this.isZero()) {
    return BigFloat.NEGATIVE_INFINITY;
  }

  const precision = BigInt(_precision);

  const { n: base, e: scale } = this;

  const result = log2BigIntFixed(base, precision);

  return _normalize(
    BigFloat.fromParts(result, -Number(precision)).add(BigFloat(scale))
  );
};

/**
 * Iteratively computes the base-2 logarithm of `b` in fixed-point representation.
 *
 * @param b - Fixed(bigint, 0 bits) integer to compute the logarithm of.
 * @param precision - Number of bits of precision to use for the fractional part.
 * @returns - Fixed(bigint, precision bits) log2(b)
 */
function log2BigIntFixed(b: bigint, precision: bigint): bigint {
  const intPart = iLog2BigInt(b);

  let frac = b << (precision - intPart);
  let result = intPart << precision;

  for (let i = 0n; i < precision; i++) {
    frac = (frac * frac) >> precision;
    if (frac >= 2n << precision) {
      frac >>= 1n;
      result |= 1n << (precision - 1n - i);
    }
  }

  return result;
}

const EXPONENTIAL_SEARCH_FAST_BREAKPOINT = 8n;

/**
 * Computes the integer logarithm base 2 of a BigInt using a binary-exponential search followed by a binary search.
 *
 * FAST PATH: If `b` is small, uses a sequential search to find the logarithm.
 *
 * @param b
 * @returns
 */
function iLog2BigInt(b: bigint): bigint {
  // Fast path for small numbers
  if (b <= 1n << EXPONENTIAL_SEARCH_FAST_BREAKPOINT)
    for (let i = 1n; i < EXPONENTIAL_SEARCH_FAST_BREAKPOINT; i++) {
      if (b <= 1n << i) {
        return i - 1n;
      }
    }

  let low = EXPONENTIAL_SEARCH_FAST_BREAKPOINT;
  let high = low << 1n;

  while (1n << high <= b) {
    low = high;
    high <<= 1n;
    if (high > Number.MAX_SAFE_INTEGER) {
      throw new Error("BigFloat exponent overflow");
    }
  }

  while (low < high) {
    const mid = (low + high) / 2n;
    if (1n << mid <= b) {
      low = mid + 1n;
    } else {
      high = mid;
    }
  }

  return low - 1n;
}

BigFloat.prototype.floor = function floor(
  this: BigFloat,
  scale: number = 0
): BigFloat {
  if (!this.isFinite()) return this;

  const shift = BigInt(this.e - scale);
  if (shift >= 0n) return this; // Already exact at this scale

  const [sign, abs] = this.n < 0n ? [-1n, -this.n] : [1n, this.n];

  const remainder = abs & ((1n << -shift) - 1n);
  const offset = sign === -1n ? (remainder === 0n ? 0n : 1n) : 0n;

  const shifted = abs << shift;

  return _normalize(BigFloat.fromParts(sign * (shifted + offset), scale));
};

BigFloat.prototype.ceil = function ceil(
  this: BigFloat,
  scale: number = 0
): BigFloat {
  if (!this.isFinite()) return this;

  const shift = BigInt(this.e - scale);
  if (shift >= 0n) return this; // Already exact at this scale

  const [sign, abs] = this.n < 0n ? [-1n, -this.n] : [1n, this.n];

  const remainder = abs & ((1n << -shift) - 1n);
  const offset = sign === 1n ? (remainder === 0n ? 0n : 1n) : 0n;

  const shifted = abs << shift;

  return _normalize(BigFloat.fromParts(sign * (shifted + offset), scale));
};

BigFloat.prototype.trunc = function trunc(
  this: BigFloat,
  scale: number = 0
): BigFloat {
  if (!this.isFinite()) return this;

  // TODO: incorrect for negative finites, rounds towards negative infinity, so maybe this should be the impl of `floor`?

  const shift = BigInt(this.e - scale);
  if (shift >= 0n) return this; // Already exact at this scale

  return _normalize(BigFloat.fromParts(this.n << shift, scale));
};

BigFloat.prototype.round = function round(
  this: BigFloat,
  scale: number = 0
): BigFloat {
  if (!this.isFinite()) return this;

  const shift = BigInt(this.e - scale);
  if (shift >= 0n) return this; // Already exact at this scale

  const [sign, abs] = this.n < 0n ? [-1n, -this.n] : [1n, this.n];

  const remainder = abs & ((1n << -shift) - 1n);
  const offset = remainder >= 1n << BigInt(-shift - 1n) ? 1n : 0n;

  const shifted = abs << shift;

  return _normalize(BigFloat.fromParts(sign * (shifted + offset), scale));
};

BigFloat.prototype.near = function near(
  this: BigFloat,
  other: BigFloat,
  epsilon: BigFloat = BigFloat.constants.MACHINE_EPSILON_DOUBLE
): boolean {
  if (this.isNaN() || other.isNaN()) {
    return false;
  }

  if (this.e === Infinity && other.e === Infinity) return true;
  if (this.e === -Infinity && other.e === -Infinity) return true;

  return this.subtract(other).abs().leq(epsilon);
};

BigFloat.prototype.compare = function compare(
  this: BigFloat,
  other: BigFloat
): -1 | 0 | 1 {
  if (this.isNaN() || other.isNaN()) {
    throw new Error("Cannot compare NaN BigFloat values.");
  }

  // Handle infinities
  if (this.e === Infinity) {
    if (other.e === Infinity) return 0;
    return 1;
  }

  if (other.e === Infinity) return -1;

  if (this.e === -Infinity) {
    if (other.e === -Infinity) return 0;
    return -1;
  }

  if (other.e === -Infinity) return 1;

  // Fast path: same scale
  if (this.e === other.e) {
    if (this.n === other.n) return 0;
    return this.n < other.n ? -1 : 1;
  }

  // Normalize and compare
  const [a, b] = [this.normalize(), other.normalize()];

  // Adjust to same scale
  const scale = Math.min(a.e, b.e);
  const aBase = a.n << BigInt(a.e - scale);
  const bBase = b.n << BigInt(b.e - scale);
  if (aBase === bBase) return 0;
  return aBase < bBase ? -1 : 1;
};

BigFloat.prototype.compareNear = function compareNear(
  this: BigFloat,
  other: BigFloat,
  epsilon: BigFloat = BigFloat.constants.MACHINE_EPSILON_DOUBLE
): -1 | 0 | 1 {
  if (this.isNaN() || other.isNaN()) {
    throw new Error("Cannot compare NaN BigFloat values.");
  }

  if (this.near(other, epsilon)) return 0;
  return this.lt(other) ? -1 : 1;
};

BigFloat.prototype.eq = function eq(this: BigFloat, other: BigFloat): boolean {
  if (this.isNaN() || other.isNaN()) return false;
  return this.compare(other) === 0;
};

BigFloat.prototype.lt = function lt(this: BigFloat, other: BigFloat): boolean {
  if (this.isNaN() || other.isNaN()) return false;
  return this.compare(other) === -1;
};

BigFloat.prototype.leq = function leq(
  this: BigFloat,
  other: BigFloat
): boolean {
  if (this.isNaN() || other.isNaN()) return false;
  const cmp = this.compare(other);
  return cmp === -1 || cmp === 0;
};

BigFloat.prototype.gt = function gt(this: BigFloat, other: BigFloat): boolean {
  if (this.isNaN() || other.isNaN()) return false;
  return this.compare(other) === 1;
};

BigFloat.prototype.geq = function geq(
  this: BigFloat,
  other: BigFloat
): boolean {
  if (this.isNaN() || other.isNaN()) return false;
  const cmp = this.compare(other);
  return cmp === 1 || cmp === 0;
};

BigFloat.epsilon = function epsilon(scale: number = -64): BigFloat {
  return BigFloat.fromParts(1n, scale);
};
