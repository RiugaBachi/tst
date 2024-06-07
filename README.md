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

As no hard constraints for performance were given, I optimized my approach around simplicity, readability, and ease of change.

I utilized a mix of unit and property-based testing. I believe the latter is key to reducing the number of questionable unit tests that add minimal value and provide loose guarantees about a system. Early on, I utilized more unit tests, however as I discovered properties about the system and codified them as property tests, some of them began to subsume some of the original unit tests, and those were in turn removed.

The tests could be further cleaned up and written in a more idiomatic way. This is my first hands-on experience with ScalaCheck (let alone Scala in general!), and I'm sure there are best practices that can be applied here (i.e., just an intuitive guess, but using quantifiers like `all` instead of folding bools might be a start).

There are some edge cases within each assignment that I handled according to my intuition (I have left comments where necessary). In a real scenario, this would be a discussion with the rest of the team and/or product owners in order to align on the most appropriate behavior to model in each of these scenarios. 

Some of the these edge cases could be reduced and/or voided entirely by remodeling the function signatures and case classes. I decided not to stray away from the types as defined in the assignment templates in this case, but in a real scenario I would consider the big picture and see where we can reap value from utilizing different types; perhaps even defining our own wrapper types encapsulated in their own modules (packages) with smart constructors that provide guarantees on certain invariants (whose properties we can in turn test independently, and not have cascade as edge case tests in other, unrelated, test modules).

Besides reducing the number of edge cases we need to handle, there could also be additional code clarity and/or performance gains to be reaped (the constant conversions to/from `Set`/`Seq` being one example that comes to mind).
