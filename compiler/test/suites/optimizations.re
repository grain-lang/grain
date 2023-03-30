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
    makeFileRunner(test_or_skip);

  let assertAnf =
      (
        ~config_fn=?,
        outfile,
        program_str,
        expected: Grain_middle_end.Anftree.anf_expression,
      ) => {
    test(outfile, ({expect}) => {
      Config.preserve_all_configs(() => {
        switch (config_fn) {
        | None => ()
        | Some(fn) => fn()
        };
        open Grain_middle_end;
        let final_anf =
          Anf_utils.clear_locations @@
          compile_string_to_final_anf(outfile, "module Test; " ++ program_str);
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
      })
    });
  };

  assertSnapshot(
    "trs1",
    "let f1 = ((x, y) => {x})\n         and f2 = ((x, y) => {y});\n       f1(1, 2)",
  );
  assertRun(
    "regression_no_elim_impure_call",
    "let foo = (f) => { let g = print(f(5)); 5 }; foo(toString)",
    "5\n",
  );
  assertAnf(
    "test_dead_branch_elimination_1.gr",
    "{ if (true) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=Managed,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_2.gr",
    "{ if (false) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=Managed,
        Imm.const(Const_number(Const_number_int(5L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_3.gr",
    "{ let x = true; if (x) {4} else {5} }",
    AExp.comp(
      Comp.imm(
        ~allocation_type=Managed,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_4.gr",
    "{let x = if (true) {4} else {5}; x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=Managed,
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
    "test_const_propagation.gr",
    "{\n    let x = 4;\n    let y = x;\n    x}",
    AExp.comp(
      Comp.imm(
        ~allocation_type=Managed,
        Imm.const(Const_number(Const_number_int(4L))),
      ),
    ),
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation2.gr",
    "((x) => {\n    let x = 4;\n    let y = x;\n    x})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, Managed)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=Managed,
                Imm.const(Const_number(Const_number_int(4L))),
              ),
            ),
            Managed,
          ),
        ),
      );
    },
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation_shadowing.gr",
    "{\n  let x = 5;\n  let y = 12;\n  let z = y;\n  {\n    let y = x;\n    x\n  }\n  x + y}",
    Grain_typed.(
      AExp.comp(
        Comp.app(
          ~tail=true,
          ~allocation_type=Managed,
          (Imm.id(Ident.create("+")), ([Managed, Managed], Managed)),
          [
            Imm.const(Const_number(Const_number_int(5L))),
            Imm.const(Const_number(Const_number_int(12L))),
          ],
        ),
      )
    ),
  );
  assertAnf(
    "test_dae.gr",
    "((x) => {let a = (x, 1); let b = (x, 1); (x, 1)})",
    {
      open Grain_typed;
      let arg = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda([(arg, Managed)]) @@
        (
          AExp.comp @@
          Comp.tuple([
            Imm.id(arg),
            Imm.const(Const_number(Const_number_int(1L))),
          ]),
          Managed,
        ),
      );
    },
  );
  assertAnf(
    "test_dae_lambda_unused.gr",
    "((x) => {1})",
    {
      open Grain_typed;
      let x = Ident.create("lambda_arg");
      AExp.comp(
        Comp.lambda(
          [(x, Managed)],
          (
            AExp.comp(
              Comp.imm(
                ~allocation_type=Managed,
                Imm.const(Const_number(Const_number_int(1L))),
              ),
            ),
            Managed,
          ),
        ),
      );
    },
  );
  assertAnf(
    "test_local_mutations1.gr",
    "provide let foo = () => {let mut x = 5; x = 6}",
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
                        ~allocation_type=Managed,
                        Imm.const(Const_number(Const_number_int(5L))),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  Comp.local_assign(
                    ~allocation_type=Unmanaged(WasmI32),
                    x,
                    Imm.const(Const_number(Const_number_int(6L))),
                  ),
                ),
                Unmanaged(WasmI32),
              ),
            ),
          ),
        ],
        AExp.comp(
          Comp.imm(
            ~allocation_type=Unmanaged(WasmI32),
            Imm.const(Const_void),
          ),
        ),
      );
    },
  );
  assertAnf(
    "test_no_local_mutation_optimization_of_closure_scope_mut.gr",
    "provide let bar = () => { let mut x = 5; let foo = () => x; foo() }",
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
                        ~allocation_type=Managed,
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
                              ~allocation_type=Managed,
                              UnboxBind,
                              Imm.id(x),
                            ),
                          ),
                          Managed,
                        ),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  Comp.app(
                    ~tail=true,
                    ~allocation_type=Managed,
                    (Imm.id(foo), ([], Managed)),
                    [],
                  ),
                ),
                Managed,
              ),
            ),
          ),
        ],
        AExp.comp(
          Comp.imm(
            ~allocation_type=Unmanaged(WasmI32),
            Imm.const(Const_void),
          ),
        ),
      );
    },
  );
  /* All optimizations are needed to work completely on this input */
  assertAnf(
    "test_optimizations_work_together.gr",
    "{\n    let x = 5;\n    let foo = ((y) => {y});\n    let y = (3, 5);\n    foo(3) + x}",
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
            Comp.lambda([(arg, Managed)]) @@
            (
              AExp.comp @@ Comp.imm(~allocation_type=Managed) @@ Imm.id(arg),
              Managed,
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
              ~allocation_type=Managed,
              (Imm.id(foo), ([Managed], Managed)),
              [Imm.const(Const_number(Const_number_int(3L)))],
            ),
          ),
        ],
      ) @@
      AExp.comp @@
      Comp.app(
        ~allocation_type=Managed,
        ~tail=true,
        (Imm.id(plus), ([Managed, Managed], Managed)),
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
    "test_manual_gc_calls_removed.gr",
    ~config_fn=() => {Grain_utils.Config.no_gc := true},
    {|
      from "runtime/unsafe/memory" include Memory
      from "runtime/unsafe/wasmi32" include WasmI32
      @disableGC
      provide let foo = (x, y, z) => {
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
              ~attributes=[
                Grain_parsing.Location.mknoloc(Typedtree.Disable_gc),
              ],
              [(arg, Managed), (arg, Managed), (arg, Managed)],
              (
                AExp.let_(
                  Nonrecursive,
                  [
                    (
                      app,
                      Comp.app(
                        ~allocation_type=Managed,
                        (Imm.id(plus), ([Managed, Managed], Managed)),
                        [Imm.id(arg), Imm.id(arg)],
                      ),
                    ),
                  ],
                  AExp.comp(
                    Comp.app(
                      ~allocation_type=Managed,
                      ~tail=true,
                      (Imm.id(plus), ([Managed, Managed], Managed)),
                      [Imm.id(app), Imm.id(arg)],
                    ),
                  ),
                ),
                Managed,
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=Unmanaged(WasmI32),
          Imm.const(Const_void),
        ),
      );
    },
  );

  // Bulk Memory (memory.fill & memory.copy)
  assertAnf(
    "test_no_bulk_memory_calls.gr",
    ~config_fn=() => {Grain_utils.Config.bulk_memory := false},
    {|
      from "runtime/unsafe/memory" include Memory
      @disableGC
      provide let foo = () => {
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
              ~attributes=[
                Grain_parsing.Location.mknoloc(Typedtree.Disable_gc),
              ],
              [],
              (
                AExp.seq(
                  Comp.app(
                    ~allocation_type=Unmanaged(WasmI32),
                    (
                      Imm.id(fill),
                      (
                        [
                          Unmanaged(WasmI32),
                          Unmanaged(WasmI32),
                          Unmanaged(WasmI32),
                        ],
                        Unmanaged(WasmI32),
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
                      ~tail=true,
                      ~allocation_type=Unmanaged(WasmI32),
                      (
                        Imm.id(copy),
                        (
                          [
                            Unmanaged(WasmI32),
                            Unmanaged(WasmI32),
                            Unmanaged(WasmI32),
                          ],
                          Unmanaged(WasmI32),
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
                Unmanaged(WasmI32),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=Unmanaged(WasmI32),
          Imm.const(Const_void),
        ),
      );
    },
  );
  assertAnf(
    "test_memory_fill_calls_replaced.gr",
    ~config_fn=() => {Grain_utils.Config.bulk_memory := true},
    {|
      from "runtime/unsafe/memory" include Memory
      @disableGC
      provide let foo = () => {
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
              ~attributes=[
                Grain_parsing.Location.mknoloc(Typedtree.Disable_gc),
              ],
              [],
              (
                AExp.seq(
                  Comp.primn(
                    ~allocation_type=Unmanaged(WasmI32),
                    WasmMemoryFill,
                    [
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                      Imm.const(Const_wasmi32(0l)),
                    ],
                  ),
                  AExp.comp(
                    Comp.primn(
                      ~allocation_type=Unmanaged(WasmI32),
                      WasmMemoryCopy,
                      [
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                        Imm.const(Const_wasmi32(0l)),
                      ],
                    ),
                  ),
                ),
                Unmanaged(WasmI32),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        Comp.imm(
          ~allocation_type=Unmanaged(WasmI32),
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
  assertRun(
    "regression_issue_1675",
    {|
      let (+) = (a, b) => toString(a) ++ " plus " ++ toString(b)
      print(1 + 2)
    |},
    "1 plus 2\n",
  );
});
