open Test

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="Int equals", (a, b) => a === b, a, b)

test("add", () => {
  intEqual(1 + 1, 2)
  intEqual(~message="1 + 2 === 3", 1 + 2, 3)
})
