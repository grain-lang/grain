open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_middle_end.Anftree;
open Grain_middle_end.Anf_helper;
open Grain_utils;
open Grain_parsing;

// Prevent updating stamps for consistent snapshots
let gensym = Grain_typed.Ident.(name => {name, stamp: 0, flags: 0});

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
      ~loc=Location.dummy_loc,
      Comp.imm(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        Imm.const(
          ~loc=Location.dummy_loc,
          Const_number(Const_number_int(4L)),
        ),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_2.gr",
    "{ if (false) {4} else {5} }",
    AExp.comp(
      ~loc=Location.dummy_loc,
      Comp.imm(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        Imm.const(
          ~loc=Location.dummy_loc,
          Const_number(Const_number_int(5L)),
        ),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_3.gr",
    "{ let x = true; if (x) {4} else {5} }",
    AExp.comp(
      ~loc=Location.dummy_loc,
      Comp.imm(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        Imm.const(
          ~loc=Location.dummy_loc,
          Const_number(Const_number_int(4L)),
        ),
      ),
    ),
  );
  assertAnf(
    "test_dead_branch_elimination_4.gr",
    "{let x = if (true) {4} else {5}; x}",
    AExp.comp(
      ~loc=Location.dummy_loc,
      Comp.imm(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        Imm.const(
          ~loc=Location.dummy_loc,
          Const_number(Const_number_int(4L)),
        ),
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
      ~loc=Location.dummy_loc,
      Comp.imm(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        Imm.const(
          ~loc=Location.dummy_loc,
          Const_number(Const_number_int(4L)),
        ),
      ),
    ),
  );
  /* Primarily a constant-propagation test, but DAE removes the let bindings as well */
  assertAnf(
    "test_const_propagation2.gr",
    "((x) => {\n    let x = 4;\n    let y = x;\n    x})",
    {
      open Grain_typed;
      let x = gensym("lambda_arg");
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.lambda(
          ~loc=Location.dummy_loc,
          [(x, GrainValue(GrainI31))],
          (
            AExp.comp(
              ~loc=Location.dummy_loc,
              Comp.imm(
                ~loc=Location.dummy_loc,
                ~allocation_type=GrainValue(GrainI31),
                Imm.const(
                  ~loc=Location.dummy_loc,
                  Const_number(Const_number_int(4L)),
                ),
              ),
            ),
            GrainValue(GrainI31),
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
        ~loc=Location.dummy_loc,
        Comp.app(
          ~loc=Location.dummy_loc,
          ~tail=true,
          ~allocation_type=GrainValue(GrainI31),
          (
            Imm.id(~loc=Location.dummy_loc, Ident.create("+")),
            (
              [GrainValue(GrainI31), GrainValue(GrainI31)],
              GrainValue(GrainI31),
            ),
          ),
          [
            Imm.const(
              ~loc=Location.dummy_loc,
              Const_number(Const_number_int(5L)),
            ),
            Imm.const(
              ~loc=Location.dummy_loc,
              Const_number(Const_number_int(12L)),
            ),
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
      let arg = gensym("lambda_arg");
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.lambda(
          ~loc=Location.dummy_loc,
          [(arg, GrainValue(GrainI31))],
        ) @@
        (
          AExp.comp(
            ~loc=Location.dummy_loc,
            Comp.tuple(
              ~loc=Location.dummy_loc,
              [
                Imm.id(~loc=Location.dummy_loc, arg),
                Imm.const(
                  ~loc=Location.dummy_loc,
                  Const_number(Const_number_int(1L)),
                ),
              ],
            ),
          ),
          GrainValue(GrainI31),
        ),
      );
    },
  );
  assertAnf(
    "test_dae_lambda_unused.gr",
    "((x) => {1})",
    {
      open Grain_typed;
      let x = gensym("lambda_arg");
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.lambda(
          ~loc=Location.dummy_loc,
          [(x, GrainValue(GrainI31))],
          (
            AExp.comp(
              ~loc=Location.dummy_loc,
              Comp.imm(
                ~loc=Location.dummy_loc,
                ~allocation_type=GrainValue(GrainI31),
                Imm.const(
                  ~loc=Location.dummy_loc,
                  Const_number(Const_number_int(1L)),
                ),
              ),
            ),
            GrainValue(GrainI31),
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
      let foo = gensym("foo");
      let x = gensym("x");
      AExp.let_(
        ~loc=Location.dummy_loc,
        Nonrecursive,
        ~global=Global,
        [
          (
            foo,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              ~name="foo",
              [],
              (
                AExp.let_(
                  ~loc=Location.dummy_loc,
                  Nonrecursive,
                  ~mut_flag=Mutable,
                  [
                    (
                      x,
                      Comp.imm(
                        ~loc=Location.dummy_loc,
                        ~allocation_type=GrainValue(GrainI31),
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_number(Const_number_int(5L)),
                        ),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  ~loc=Location.dummy_loc,
                  Comp.local_assign(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=GrainValue(GrainI31),
                    x,
                    Imm.const(
                      ~loc=Location.dummy_loc,
                      Const_number(Const_number_int(6L)),
                    ),
                  ),
                ),
                GrainValue(GrainI31),
              ),
            ),
          ),
        ],
        AExp.comp(
          ~loc=Location.dummy_loc,
          Comp.imm(
            ~loc=Location.dummy_loc,
            ~allocation_type=GrainValue(GrainI31),
            Imm.const(~loc=Location.dummy_loc, Const_void),
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
      let x = gensym("x");
      let bar = gensym("bar");
      let foo = gensym("foo");
      AExp.let_(
        ~loc=Location.dummy_loc,
        Nonrecursive,
        ~global=Global,
        [
          (
            bar,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              ~name="bar",
              [],
              (
                AExp.let_(
                  ~loc=Location.dummy_loc,
                  Nonrecursive,
                  [
                    (
                      x,
                      Comp.prim1(
                        ~loc=Location.dummy_loc,
                        ~allocation_type=GrainValue(GrainI31),
                        BoxBind,
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_number(Const_number_int(5L)),
                        ),
                      ),
                    ),
                  ],
                ) @@
                AExp.let_(
                  ~loc=Location.dummy_loc,
                  Nonrecursive,
                  [
                    (
                      foo,
                      Comp.lambda(
                        ~loc=Location.dummy_loc,
                        [],
                        (
                          AExp.comp(
                            ~loc=Location.dummy_loc,
                            Comp.prim1(
                              ~loc=Location.dummy_loc,
                              ~allocation_type=GrainValue(GrainI31),
                              UnboxBind,
                              Imm.id(~loc=Location.dummy_loc, x),
                            ),
                          ),
                          GrainValue(GrainI31),
                        ),
                      ),
                    ),
                  ],
                ) @@
                AExp.comp(
                  ~loc=Location.dummy_loc,
                  Comp.app(
                    ~loc=Location.dummy_loc,
                    ~tail=true,
                    ~allocation_type=GrainValue(GrainI31),
                    (
                      Imm.id(~loc=Location.dummy_loc, foo),
                      ([], GrainValue(GrainI31)),
                    ),
                    [],
                  ),
                ),
                GrainValue(GrainI31),
              ),
            ),
          ),
        ],
        AExp.comp(
          ~loc=Location.dummy_loc,
          Comp.imm(
            ~loc=Location.dummy_loc,
            ~allocation_type=GrainValue(GrainI31),
            Imm.const(~loc=Location.dummy_loc, Const_void),
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
      let plus = gensym("+");
      let foo = gensym("foo");
      let arg = gensym("lambda_arg");
      let app = gensym("app");
      AExp.let_(
        ~loc=Location.dummy_loc,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              [(arg, GrainValue(GrainI31))],
            ) @@
            (
              AExp.comp(~loc=Location.dummy_loc) @@
              Comp.imm(
                ~loc=Location.dummy_loc,
                ~allocation_type=GrainValue(GrainI31),
              ) @@
              Imm.id(~loc=Location.dummy_loc, arg),
              GrainValue(GrainI31),
            ),
          ),
        ],
      ) @@
      AExp.let_(
        ~loc=Location.dummy_loc,
        Nonrecursive,
        [
          (
            app,
            Comp.app(
              ~loc=Location.dummy_loc,
              ~allocation_type=GrainValue(GrainI31),
              (
                Imm.id(~loc=Location.dummy_loc, foo),
                ([GrainValue(GrainI31)], GrainValue(GrainI31)),
              ),
              [
                Imm.const(
                  ~loc=Location.dummy_loc,
                  Const_number(Const_number_int(3L)),
                ),
              ],
            ),
          ),
        ],
      ) @@
      AExp.comp(~loc=Location.dummy_loc) @@
      Comp.app(
        ~loc=Location.dummy_loc,
        ~allocation_type=GrainValue(GrainI31),
        ~tail=true,
        (
          Imm.id(~loc=Location.dummy_loc, plus),
          (
            [GrainValue(GrainI31), GrainValue(GrainI31)],
            GrainValue(GrainI31),
          ),
        ),
        [
          Imm.id(~loc=Location.dummy_loc, app),
          Imm.const(
            ~loc=Location.dummy_loc,
            Const_number(Const_number_int(5L)),
          ),
        ],
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
    {|
      from "runtime/unsafe/memory" include Memory
      from "runtime/unsafe/wasmi32" include WasmI32
      @unsafe
      provide let foo = (x, y, z) => {
        Memory.incRef(WasmI32.fromGrain((+)))
        Memory.incRef(WasmI32.fromGrain((+)))
        // x, y, and z will get decRef'd by `+`
        x + y + z
      }
    |},
    {
      open Grain_typed;
      let plus = gensym("+");
      let foo = gensym("foo");
      let arg = gensym("lambda_arg");
      let app = gensym("app");
      AExp.let_(
        ~loc=Location.dummy_loc,
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              ~name=Ident.name(foo),
              [
                (arg, GrainValue(GrainI31)),
                (arg, GrainValue(GrainI31)),
                (arg, GrainValue(GrainI31)),
              ],
              (
                AExp.let_(
                  ~loc=Location.dummy_loc,
                  Nonrecursive,
                  [
                    (
                      app,
                      Comp.app(
                        ~loc=Location.dummy_loc,
                        ~allocation_type=GrainValue(GrainI31),
                        (
                          Imm.id(~loc=Location.dummy_loc, plus),
                          (
                            [GrainValue(GrainI31), GrainValue(GrainI31)],
                            GrainValue(GrainI31),
                          ),
                        ),
                        [
                          Imm.id(~loc=Location.dummy_loc, arg),
                          Imm.id(~loc=Location.dummy_loc, arg),
                        ],
                      ),
                    ),
                  ],
                  AExp.comp(
                    ~loc=Location.dummy_loc,
                    Comp.app(
                      ~loc=Location.dummy_loc,
                      ~allocation_type=GrainValue(GrainI31),
                      ~tail=true,
                      (
                        Imm.id(~loc=Location.dummy_loc, plus),
                        (
                          [GrainValue(GrainI31), GrainValue(GrainI31)],
                          GrainValue(GrainI31),
                        ),
                      ),
                      [
                        Imm.id(~loc=Location.dummy_loc, app),
                        Imm.id(~loc=Location.dummy_loc, arg),
                      ],
                    ),
                  ),
                ),
                GrainValue(GrainI31),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.imm(
          ~loc=Location.dummy_loc,
          ~allocation_type=GrainValue(GrainI31),
          Imm.const(~loc=Location.dummy_loc, Const_void),
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
      @unsafe
      provide let foo = () => {
        Memory.fill(0n, 0n, 0n)
        Memory.copy(0n, 0n, 0n)
      }
    |},
    {
      open Grain_typed;
      let foo = gensym("foo");
      let fill = gensym("fill");
      let copy = gensym("copy");
      AExp.let_(
        ~loc=Location.dummy_loc,
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              ~name=Ident.name(foo),
              [],
              (
                AExp.seq(
                  ~loc=Location.dummy_loc,
                  Comp.app(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=GrainValue(GrainI31),
                    (
                      Imm.id(~loc=Location.dummy_loc, fill),
                      (
                        [
                          GrainValue(GrainI31),
                          GrainValue(GrainI31),
                          GrainValue(GrainI31),
                        ],
                        GrainValue(GrainI31),
                      ),
                    ),
                    [
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                    ],
                  ),
                  AExp.comp(
                    ~loc=Location.dummy_loc,
                    Comp.app(
                      ~loc=Location.dummy_loc,
                      ~tail=true,
                      ~allocation_type=GrainValue(GrainI31),
                      (
                        Imm.id(~loc=Location.dummy_loc, copy),
                        (
                          [
                            GrainValue(GrainI31),
                            GrainValue(GrainI31),
                            GrainValue(GrainI31),
                          ],
                          GrainValue(GrainI31),
                        ),
                      ),
                      [
                        Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_wasmi32(0l),
                        ),
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_wasmi32(0l),
                        ),
                      ],
                    ),
                  ),
                ),
                GrainValue(GrainI31),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.imm(
          ~loc=Location.dummy_loc,
          ~allocation_type=GrainValue(GrainI31),
          Imm.const(~loc=Location.dummy_loc, Const_void),
        ),
      );
    },
  );
  assertAnf(
    "test_memory_fill_calls_replaced.gr",
    ~config_fn=() => {Grain_utils.Config.bulk_memory := true},
    {|
      from "runtime/unsafe/memory" include Memory
      @unsafe
      provide let foo = () => {
        Memory.fill(0n, 0n, 0n)
        Memory.copy(0n, 0n, 0n)
      }
    |},
    {
      open Grain_typed;
      let foo = gensym("foo");
      AExp.let_(
        ~loc=Location.dummy_loc,
        ~global=Global,
        Nonrecursive,
        [
          (
            foo,
            Comp.lambda(
              ~loc=Location.dummy_loc,
              ~name=Ident.name(foo),
              [],
              (
                AExp.seq(
                  ~loc=Location.dummy_loc,
                  Comp.primn(
                    ~loc=Location.dummy_loc,
                    ~allocation_type=GrainValue(GrainI31),
                    WasmMemoryFill,
                    [
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                      Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                    ],
                  ),
                  AExp.comp(
                    ~loc=Location.dummy_loc,
                    Comp.primn(
                      ~loc=Location.dummy_loc,
                      ~allocation_type=GrainValue(GrainI31),
                      WasmMemoryCopy,
                      [
                        Imm.const(~loc=Location.dummy_loc, Const_wasmi32(0l)),
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_wasmi32(0l),
                        ),
                        Imm.const(
                          ~loc=Location.dummy_loc,
                          Const_wasmi32(0l),
                        ),
                      ],
                    ),
                  ),
                ),
                GrainValue(GrainI31),
              ),
            ),
          ),
        ],
      ) @@
      AExp.comp(
        ~loc=Location.dummy_loc,
        Comp.imm(
          ~loc=Location.dummy_loc,
          ~allocation_type=GrainValue(GrainI31),
          Imm.const(~loc=Location.dummy_loc, Const_void),
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
