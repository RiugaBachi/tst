# TST Takehome Solutions

Solutions for TST's takehome assignment.

(Incidentally, my first attempt at writing Scala!)

## Running

To run functions in each assignment against the sample input data:

```bash
# Problem 1
sbt "runMain printBestPrices"

# Problem 2
sbt "runMain printCombinations"
```

## Testing

To run the test suites for both assignments:

```bash
sbt test
```

## Commentary

As no hard constraints for performance were given, I optimized my approach around simplicity and readability.

I utilized a mix of unit and property-based testing. I believe the latter is key to reducing the number of questionable unit tests that add minimal value and provide loose guarantees about a system. Early on, I utilized more unit tests, however as I discovered properties about the system and codified them as property tests, some of them began to subsume some of the original unit tests, and those were in turn removed.

The tests could be cleaned up and/or written in a more idiomatic way. This is my first hands-on experience with ScalaCheck (let alone Scala in general!), and I'm sure there are best practices that can be applied here (i.e., just an intuitive guess, but using quantifiers like `all` instead of folding bools might be a start).
