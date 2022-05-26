open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;
open Grain_utils;

describe("optimizations", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;

  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test_or_skip);
  let assertBinaryenOptimizationsDisabledFileRun =
    makeFileRunner(
      ~config_fn=() => {Config.optimization_level := Config.Level_two},
      test_or_skip,
    );

  let assertAnf =
      (
        ~config_fn=?,
        outfile,
        program_str,
        expected: Grain_middle_end.Anftree.anf_expression,
      ) => {
    test(outfile, ({expect}) => {
      Grain_utils.Config.preserve_all_configs(() => {
        Config.with_config(
          Config.empty,
          () => {
            switch (config_fn) {
            | None => ()
            | Some(fn) => fn()
            };
            open Grain_middle_end;
            let final_anf =
              Anf_utils.clear_locations @@
              compile_string_to_final_anf(outfile, program_str);
            let saved_disabled = Grain_typed.Ident.disable_stamps^;
            let (result, expected) =
              try(
                {
                  Grain_typed.Ident.disable_stamps := true;
                  let result =
                    Sexplib.Sexp.to_string_hum @@
                    Anftree.sexp_of_anf_expression(final_anf.body);
                  let expected =
                    Sexplib.Sexp.to_string_hum @@
                    Anftree.sexp_of_anf_expression(expected);
                  (result, expected);
                }
              ) {
              | e =>
                Grain_typed.Ident.disable_stamps := saved_disabled;
                raise(e);
              };
            expect.string(result).toEqual(expected);
          },
        )
      })
    });
  };

  assertSnapshot(
    "trs1",
    "let f1 = ((x, y) => {x}),\n         f2 = ((x, y) => {y});\n       f1(1, 2)",
  );
  assertRun(
    "regression_no_elim_impure_call",
    "let foo = (f) => { let g = f(5); 5 }; foo(print)",
    "5\n",
  );
  assertAnf(
    "test_dead_branch_elimination_1",
    "{ if (true) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_2",
    "{ if (false) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(5L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_3",
    "{ let x = true; if (x) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_4",
    "{let x = if (true) {4} else {5}; x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  assertSnapshot(
    "test_dead_branch_elimination_5",
    "\n      let x = box(1);\n      let y = box(2);\n      let z =\n        if (true) {\n          x := 3;\n          y := 4\n        } else {\n          x := 5;\n          y := 6\n        };\n      unbox(x) + unbox(y)",
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation",
    "{\n    let x = 4;\n    let y = x;\n    x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation2",
    "((x) => {\n    let x = 4;\n    let y = x;\n    x})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, HeapAllocated)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=HeapAllocated,
                Imm.const(Const_number(Const_number_int(4L))),
              ),
            ),
            HeapAllocated,
          ),
        ),
      );
    },
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation_shadowing",
    "{\n  let x = 5;\n  let y = 12;\n  let z = y;\n  {\n    let y = x;\n    x\n  }\n  x + y}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(17L))),
      ),
    ),
  );
  /* Primarily a constant-folding test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_folding",
    "{\n    let x = 4 + 5;\n    let y = x * 2;\n    let z = y - x;\n    let a = x + 7;\n    let b = 14;\n    a + b}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=HeapAllocated,
        Imm.const(Const_number(Const_number_int(30L))),
      ),
    ),
  );
  assertAnf(
    "test_dae",
    "((x) => {let a = (x, 1); let b = (x, 1); (x, 1)})",
    {
      open Grain_typed;
      let arg = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda([(arg, HeapAllocated)]) @@
        (
          AExp.comp @@
          Comp.tuple([
            Imm.id(arg),
            Imm.const(Const_number(Const_number_int(1L))),
          ]),
          HeapAllocated,
        ),
      );
    },
  );
  assertAnf(
    "test_dae_lambda_unused",
    "((x) => {1})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, HeapAllocated)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=HeapAllocated,
                Imm.const(Const_number(Const_number_int(1L))),
              ),
            ),
            HeapAllocated,
          ),
        ),
      );
    },
  );
  assertAnf(
    "test_local_mutations1",
    "export let foo = () => {let mut x = 5; x = 6}",
    {
      open Grain_typed;
      let foo = Ident.create("foo");
      let x = Ident.create("x");
      AExp.let_(
        Nonrecursive,
        ~global=Global,
        [
          (
            foo,
            Comp.lambda(
              ~name="foo",
              [],
              (
                AExp.let_(
                  Nonrecursive,
                  ~mut_flag=Mutable,
                  [
                    (
                      x,
                      Comp.imm(
                        ~allocation_type=HeapAllocated,
                        Imm.const(Const_number(Const_number_int(5L))),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  Comp.local_assign(
                    ~allocation_type=StackAllocated(WasmI32),
                    x,
                    Imm.const(Const_number(Const_number_int(6L))),
                  ),
                ),
                StackAllocated(WasmI32),
              ),
            ),
          ),
        ],
        AExp.comp(
          Comp.imm(
            ~allocation_type=StackAllocated(WasmI32),
            Imm.const(Const_void),
          ),
        ),
      );
    },
  );
  assertAnf(
    "test_no_local_mutation_optimization_of_closure_scope_mut",
    "/* grainc-flags --experimental-wasm-tail-call */ export let bar = () => { let mut x = 5; let foo = () => x; foo() }",
    {
      open Grain_typed;
      let x = Ident.create("x");
      let bar = Ident.create("bar");
      let foo = Ident.create("foo");
      AExp.let_(
        Nonrecursive,
        ~global=Global,
        [
          (
            bar,
            Comp.lambda(
              ~name="bar",
              [],
              (
                AExp.let_(
                  Nonrecursive,
                  [
                    (
                      x,
                      Comp.prim1(
                        ~allocation_type=HeapAllocated,
                        BoxBind,
                        Imm.const(Const_number(Const_number_int(5L))),
                      ),
                    ),
                  ],
                ) @@
                AExp.let_(
                  Nonrecursive,
                  [
                    (
                      foo,
                      Comp.lambda(
                        [],
                        (
                          AExp.comp(
                            Comp.prim1(
                              ~allocation_type=HeapAllocated,
                              UnboxBind,
                              Imm.id(x),
                            ),
                          ),
                          HeapAllocated,
                        ),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  Comp.app(
                    ~tail=true,
                    ~allocation_type=HeapAllocated,
                    (Imm.id(foo), ([], HeapAllocated)),
                    [],
                  ),
                ),
                HeapAllocated,
              ),
            ),
          ),
        ],
        AExp.comp(
          Comp.imm(
            ~allocation_type=StackAllocated(WasmI32),
            Imm.const(Const_void),
          ),
        ),
      );
    },
  );
  /* All optimizations are needed to work completely on this input */
  assertAnf(
    "test_optimizations_work_together",
    "/* grainc-flags --experimental-wasm-tail-call */ {\n    let x = 5;\n    let foo = ((y) => {y});\n    let y = (3, 5);\n    foo(3) + x}",
    {
      open Grain_typed;
      let plus = Ident.create("+");
      let foo = Ident.create("foo");
      let arg = Ident.create("lambda_arg");
      let app = Ident.create("app");
      AExp.let_(
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda([(arg, HeapAllocated)]) @@
            (
              AExp.comp @@
              Comp.imm(~allocation_type=HeapAllocated) @@
              Imm.id(arg),
              HeapAllocated,
            ),
          ),
        ],
      ) @@
      AExp.let_(
        Nonrecursive,
        [
          (
            app,
            Comp.app(
              ~allocation_type=HeapAllocated,
              (Imm.id(foo), ([HeapAllocated], HeapAllocated)),
              [Imm.const(Const_number(Const_number_int(3L)))],
            ),
          ),
        ],
      ) @@
      AExp.comp @@
      Comp.app(
        ~allocation_type=HeapAllocated,
        ~tail=true,
        (Imm.id(plus), ([HeapAllocated, HeapAllocated], HeapAllocated)),
        [Imm.id(app), Imm.const(Const_number(Const_number_int(5L)))],
      );
    },
  );
  assertCompileError("test_dae_sound", "let x = 2 + false; 3", "type");
  assertCompileError(
    "test_const_fold_times_zero_sound",
    "let f = ((x) => {x * 0}); f(false)",
    "Number",
  );
  assertCompileError(
    "test_const_fold_or_sound",
    "let f = ((x) => {x || true}); f(1)",
    "Bool",
  );
  assertCompileError(
    "test_const_fold_and_sound",
    "let f = ((x) => {false && x}); f(1)",
    "Bool",
  );
  assertCompileError(
    "test_const_fold_plus_sound",
    "let f = ((x) => {0 + x}); f(true)",
    "Number",
  );
  assertCompileError(
    "test_const_fold_times_one_sound",
    "let f = ((x) => {x * 1}); f(true)",
    "Number",
  );
  // Binaryen optimizations disabled
  assertBinaryenOptimizationsDisabledFileRun(
    "test_binaryen_optimizations_disabled",
    "toplevelStatements",
    "1\n2\n3\n4\n5\n",
  );

  // Removal of manual memory management calls
  assertAnf(
    "test_manual_gc_calls_removed",
    ~config_fn=() => {Grain_utils.Config.experimental_tail_call := true},
    {|
      /* grainc-flags --no-gc */
      import Memory from "runtime/unsafe/memory"
      import WasmI32 from "runtime/unsafe/wasmi32"
      @disableGC
      export let foo = (x, y, z) => {
        Memory.incRef(WasmI32.fromGrain((+)))
        Memory.incRef(WasmI32.fromGrain((+)))
        // x, y, and z will get decRef'd by `+`
        x + y + z
      }
    |},
    {
      open Grain_typed;
      let plus = Ident.create("+");
      let foo = Ident.create("foo");
      let arg = Ident.create("lambda_arg");
      let app = Ident.create("app");
      AExp.let_(
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~name=Ident.name(foo),
              ~attributes=[Disable_gc],
              [
                (arg, HeapAllocated),
                (arg, HeapAllocated),
                (arg, HeapAllocated),
              ],
              (
                AExp.let_(
                  Nonrecursive,
                  [
                    (
                      app,
                      Comp.app(
                        ~allocation_type=HeapAllocated,
                        (
                          Imm.id(plus),
                          ([HeapAllocated, HeapAllocated], HeapAllocated),
                        ),
                        [Imm.id(arg), Imm.id(arg)],
                      ),
                    ),
                  ],
                  AExp.comp(
                    Comp.app(
                      ~allocation_type=HeapAllocated,
                      ~tail=true,
                      (
                        Imm.id(plus),
                        ([HeapAllocated, HeapAllocated], HeapAllocated),
                      ),
                      [Imm.id(app), Imm.id(arg)],
                    ),
                  ),
                ),
                HeapAllocated,
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=StackAllocated(WasmI32),
          Imm.const(Const_void),
        ),
      );
    },
  );

  // Bulk Memory (memory.fill & memory.copy)
  assertAnf(
    "test_no_bulk_memory_calls",
    ~config_fn=() => {Grain_utils.Config.bulk_memory := false},
    {|
      import Memory from "runtime/unsafe/memory"
      @disableGC
      export let foo = () => {
        Memory.fill(0n, 0n, 0n)
        Memory.copy(0n, 0n, 0n)
      }
    |},
    {
      open Grain_typed;
      let foo = Ident.create("foo");
      let fill = Ident.create("fill");
      let copy = Ident.create("copy");
      AExp.let_(
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~name=Ident.name(foo),
              ~attributes=[Disable_gc],
              [],
              (
                AExp.seq(
                  Comp.app(
                    ~allocation_type=StackAllocated(WasmI32),
                    (
                      Imm.id(fill),
                      (
                        [
                          StackAllocated(WasmI32),
                          StackAllocated(WasmI32),
                          StackAllocated(WasmI32),
                        ],
                        StackAllocated(WasmI32),
                      ),
                    ),
                    [
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                    ],
                  ),
                  AExp.comp(
                    Comp.app(
                      ~allocation_type=StackAllocated(WasmI32),
                      (
                        Imm.id(copy),
                        (
                          [
                            StackAllocated(WasmI32),
                            StackAllocated(WasmI32),
                            StackAllocated(WasmI32),
                          ],
                          StackAllocated(WasmI32),
                        ),
                      ),
                      [
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                      ],
                    ),
                  ),
                ),
                StackAllocated(WasmI32),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=StackAllocated(WasmI32),
          Imm.const(Const_void),
        ),
      );
    },
  );
  assertAnf(
    "test_memory_fill_calls_replaced",
    ~config_fn=() => {Grain_utils.Config.bulk_memory := true},
    {|
      import Memory from "runtime/unsafe/memory"
      @disableGC
      export let foo = () => {
        Memory.fill(0n, 0n, 0n)
        Memory.copy(0n, 0n, 0n)
      }
    |},
    {
      open Grain_typed;
      let foo = Ident.create("foo");
      AExp.let_(
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~name=Ident.name(foo),
              ~attributes=[Disable_gc],
              [],
              (
                AExp.seq(
                  Comp.primn(
                    ~allocation_type=StackAllocated(WasmI32),
                    WasmMemoryFill,
                    [
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                    ],
                  ),
                  AExp.comp(
                    Comp.primn(
                      ~allocation_type=StackAllocated(WasmI32),
                      WasmMemoryCopy,
                      [
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                      ],
                    ),
                  ),
                ),
                StackAllocated(WasmI32),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=StackAllocated(WasmI32),
          Imm.const(Const_void),
        ),
      );
    },
  );
  assertRun(
    "test_mut_inlining",
    {|
      let mut foo = 5
      let bar = foo
      foo = 6
      print(bar)
    |},
    "5\n",
  );
});
