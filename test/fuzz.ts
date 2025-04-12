// Copyright (c) Will Temple
// SPDX-License-Identifier: MIT
// This file is part of the BigFloat library.

import BigFloat from "../src/index.js";

// Ensure that all BigFloat methods are defined on the prototype.

const methods = [
  "add",
  "subtract",
  "multiply",
  "divide",
  "sqrt",
  "log",
  "log2",
  "log10",
  "ln",
  "isNormalized",
  "isFinite",
  "isInteger",
  "isNaN",
  "isZero",
  "abs",
  "negate",
  "floor",
  "trunc",
  "round",
  "pow",
  "ceil",
  "eq",
  "lt",
  "leq",
  "gt",
  "geq",
  "near",
  "mod",
  "normalize",
  "toString",
  "compare",
  "compareNear",
];

for (const method of methods) {
  if (typeof (BigFloat.prototype as any)[method] !== "function") {
    throw new Error(`BigFloat.${method} is not defined`);
  }
}

function randInt(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function randBigInt(bits = 64): bigint {
  const parts: number[] = Array.from({ length: Math.ceil(bits / 32) }, () =>
    randInt(0, 0xffffffff)
  );
  return (
    parts.reduce(
      (acc, part, i) => acc + (BigInt(part) << BigInt(i * 32)),
      BigInt(0)
    ) * (Math.random() < 0.5 ? BigInt(1) : BigInt(-1))
  );
}

function randFloat(): number {
  return (Math.random() - 0.5) * Math.pow(2, randInt(-1000, 1000));
}

function randBigFloat(): BigFloat {
  const mode = randInt(0, 6);
  switch (mode) {
    case 0:
      return new BigFloat(NaN);
    case 1:
      return new BigFloat(Infinity);
    case 2:
      return new BigFloat(-Infinity);
    case 3:
      return BigFloat.fromParts(randBigInt(128), randInt(-1024, 1024));
    case 4:
      return new BigFloat(randFloat());
    case 5:
      return new BigFloat(0);
    default:
      return new BigFloat(randBigInt(64));
  }
}

function printValues(values: Record<string, unknown>) {
  for (const [key, value] of Object.entries(values)) {
    console.log(`\t${key}: ${value}`);
  }
}

function assertEqualOrBothNaN(
  a: BigFloat,
  b: BigFloat,
  msg?: string,
  values: Record<string, BigFloat> = {}
) {
  if (a.isNaN() && b.isNaN()) return;
  if (!a.eq(b)) {
    stats.failed++;
    console.error(
      `FAIL: ${msg ?? "Equality assertion failed"}\n  ${a}\n  !=\n  ${b}`
    );
    printValues(values);
    debugger;
  } else {
    stats.passed++;
  }
}

function assertNear(
  a: BigFloat,
  b: BigFloat,
  epsScale = -64,
  msg?: string,
  values: Record<string, unknown> = {}
) {
  if (a.isNaN() && b.isNaN()) return;

  const epsilon = BigFloat.epsilon(epsScale);
  if (!a.near(b, epsilon)) {
    stats.failed++;
    console.error(
      `FAIL: ${
        msg ?? "Near equality failed"
      }\n  ${a}\n  ~!=\n  ${b} (eps = ${epsScale})`
    );
    printValues(values);
    debugger;
  } else {
    stats.passed++;
  }
}

const stats = {
  passed: 0,
  failed: 0,
  crashed: 0,
};

function fuzzOnce() {
  const a = randBigFloat();
  const b = randBigFloat();
  const c = randBigFloat();

  try {
    // Identity: a - a = 0
    const expect = a.isFinite() ? BigFloat(0) : BigFloat.NAN;
    assertEqualOrBothNaN(
      a.subtract(a).normalize(),
      expect,
      `a - a != 0 (or NaN)`,
      { a }
    );

    // Symmetry: a + b == b + a
    assertEqualOrBothNaN(a.add(b), b.add(a), "Addition is not commutative", {
      a,
      b,
    });

    // Associativity: (a + b) + c ≈ a + (b + c)
    assertNear(
      a.add(b).add(c),
      a.add(b.add(c)),
      -64,
      "Addition is not associative",
      { a, b, c }
    );

    // Multiplication commutativity
    assertEqualOrBothNaN(
      a.multiply(b),
      b.multiply(a),
      "Multiplication is not commutative",
      {
        a,
        b,
      }
    );

    // Multiplicative identity
    assertEqualOrBothNaN(a.multiply(new BigFloat(1)), a, "a * 1 ≠ a", { a });

    // Additive identity
    assertEqualOrBothNaN(a.add(new BigFloat(0)), a, "a + 0 ≠ a", { a });

    // Division inverse (a / a ≈ 1 unless a is 0/NaN/Inf)
    if (a.isFinite() && !a.isZero()) {
      assertNear(a.divide(a), new BigFloat(1), -64, "a / a ≠ 1", { a });
    }

    // Check NaN propagation
    const nan = new BigFloat(NaN);
    const ops = [
      () => nan.add(b),
      () => nan.subtract(b),
      () => nan.multiply(b),
      () => nan.divide(b),
      () => nan.eq(b),
      () => nan.gt(b),
    ];
    for (const op of ops) {
      const result = op();
      if (
        result instanceof BigFloat &&
        !result.isNaN() &&
        typeof result !== "boolean"
      ) {
        console.error("FAIL: NaN propagation failed");
        console.error(`  operation: ${op}`);
        console.error(`  result: ${result}`);
        debugger;
      }
    }

    // Check 0 handling: a % a == 0, 0 % a == 0
    if (a.isFinite() && !a.isZero()) {
      assertEqualOrBothNaN(a.mod(a), new BigFloat(0), "a % a ≠ 0", { a });
      assertEqualOrBothNaN(
        new BigFloat(0).mod(a),
        new BigFloat(0),
        "0 % a ≠ 0",
        { a }
      );
    }

    // sqrt(a)^2 ≈ a (only for positive values)
    if (!a.isNaN() && !a.isZero() && a.gt(new BigFloat(0))) {
      const interior_precision = (a.n.toString(2).length + Math.abs(a.e)) * 2;
      const sqrtA = a.sqrt(Math.max(interior_precision, 64));
      assertNear(sqrtA.multiply(sqrtA), a, -32, "sqrt(a)^2 ≠ a", { a, sqrtA });
    }

    // log(exp(x)) ≈ x
    if (
      a.isFinite() &&
      !a.isZero() &&
      a.gt(new BigFloat(-1000)) &&
      a.lt(new BigFloat(1000))
    ) {
      if (a.lt(BigFloat(-30))) {
        const magnitude = a.abs().ceil().toNumber();
        const maxPrecision = Math.max(64, Math.ceil(magnitude * 1.2));
        const expA = BigFloat.exp(a, maxPrecision);
        const logExpA = expA.ln(maxPrecision);
        assertNear(logExpA, a, -50, "ln(exp(a)) ≠ a", {
          a,
          expA,
          logExpA,
          maxPrecision,
        });
      } else {
        const bitLength = a.n === 0n ? 1 : a.n.toString(2).length;
        const shiftMagnitude = Math.abs(a.e);
        const interior_precision = Math.max(
          64,
          2 * (bitLength + shiftMagnitude)
        );
        const precision = Math.min(256, interior_precision);

        const expA = BigFloat.exp(a, precision);
        const logExpA = expA.ln(precision);
        assertNear(logExpA, a, -50, "ln(exp(a)) ≠ a", { a, expA, logExpA });
      }
    }

    // Normalization round trip: x.normalize().isNormalized() == true
    const normalized = a.normalize();
    if (!normalized.isNormalized()) {
      console.error("FAIL: normalize() did not produce a normalized value");
      printValues({ a, normalized });
      debugger;
    }

    // Reflexivity: a == a
    if (!a.eq(a) && !a.isNaN()) {
      console.error("FAIL: Reflexivity failed — a ≠ a");
      printValues({ a });
      debugger;
    }

    // Anti-symmetry: if a < b then b > a
    if (a.lt(b) && !b.gt(a)) {
      console.error(`FAIL: a < b but b !> a\n  a = ${a}\n  b = ${b}`);
      printValues({ a, b });
      debugger;
    }
  } catch (e) {
    console.error("CRASH: Exception during fuzzing operation", e);
    stats.crashed++;
  }
}

function startFuzz(iterations = 100000) {
  console.log(`Starting BigFloat fuzzing with ${iterations} iterations...`);
  for (let i = 0; i < iterations; i++) {
    fuzzOnce();
    if (i % 1000 === 0) console.log(`Fuzz iteration ${i}...`);
  }

  console.log(`Passed: ${stats.passed}`);
  console.log(`Failed: ${stats.failed}`);
  console.log(`Crashed: ${stats.crashed}`);
  console.log("Fuzzing complete.");
}

// Run the fuzzer
startFuzz();
