open Grain_tests.TestFramework;
open Grain_tests.Runner;

describe("cyclic references", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertRun = makeRunner(test_or_skip);

  assertRun(
    "cycles1",
    {|
      record A {
        mut a: Option<A>
      }
      let x = { a: None }
      x.a = Some(x)
      print(x)
    |},
    "<1>: {\n  a: Some(<cycle to <1>>)\n}\n",
  );
  assertRun(
    "cycles2",
    {|
      record A {
        val: Number,
        mut a: Option<A>
      }

      let x = { val: 1, a: None }
      let y = { val: 2, a: None }
      let xOpt = Some(x)
      let yOpt = Some(y)
      x.a = yOpt
      y.a = xOpt

      print([x, y])
    |},
    "[<1>: {\n  val: 1,\n  a: Some({\n    val: 2,\n    a: Some(<cycle to <1>>)\n  })\n}, <2>: {\n  val: 2,\n  a: Some(<1>: {\n    val: 1,\n    a: Some(<cycle to <2>>)\n  })\n}]\n",
  );
  assertRun(
    "cycles3",
    {|
      record A {
        val: Number,
        mut next: Option<A>
      }

      let a = { val: 1, next: None }
      let aOpt = Some(a)
      let b = Some({ val: 2, next: aOpt })
      let c = Some({ val: 3, next: b })
      let d = Some({ val: 4, next: c })
      let e = Some({ val: 5, next: d })
      a.next = e
      print([aOpt, b, c, d, e])
    |},
    {|[Some(<1>: {
  val: 1,
  next: Some({
    val: 5,
    next: Some({
      val: 4,
      next: Some({
        val: 3,
        next: Some({
          val: 2,
          next: Some(<cycle to <1>>)
        })
      })
    })
  })
}), Some(<2>: {
  val: 2,
  next: Some(<1>: {
    val: 1,
    next: Some({
      val: 5,
      next: Some({
        val: 4,
        next: Some({
          val: 3,
          next: Some(<cycle to <2>>)
        })
      })
    })
  })
}), Some(<3>: {
  val: 3,
  next: Some(<2>: {
    val: 2,
    next: Some(<1>: {
      val: 1,
      next: Some({
        val: 5,
        next: Some({
          val: 4,
          next: Some(<cycle to <3>>)
        })
      })
    })
  })
}), Some(<4>: {
  val: 4,
  next: Some(<3>: {
    val: 3,
    next: Some(<2>: {
      val: 2,
      next: Some(<1>: {
        val: 1,
        next: Some({
          val: 5,
          next: Some(<cycle to <4>>)
        })
      })
    })
  })
}), Some(<5>: {
  val: 5,
  next: Some(<4>: {
    val: 4,
    next: Some(<3>: {
      val: 3,
      next: Some(<2>: {
        val: 2,
        next: Some(<1>: {
          val: 1,
          next: Some(<cycle to <5>>)
        })
      })
    })
  })
})]
|},
  );
});
