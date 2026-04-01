open Binaryen;
open Comp_utils;
open Grain_utils;

/**
 * Initializes the types for Grains core data representations.
 */
let build_core_data_representations = (wasm_mod: Module.t) => {
  // Helpers for building our core data representations
  let field =
      (~packed_type=Packed_type.not_packed, ~name=?, ~mutable_=false, type_) => (
    name,
    Type_builder.{
      type_,
      packed_type,
      mutable_,
    },
  );
  let build_struct = (~open_=false, ~supertype=?, ~name=?, fields) => {
    let builder = Type_builder.make(1);
    Type_builder.set_struct_type(
      builder,
      0,
      List.map(((_, typ)) => typ, fields),
    );
    switch (supertype) {
    | Some(supertype) => Type_builder.set_sub_type(builder, 0, supertype)
    | None => ()
    };
    if (open_) {
      Type_builder.set_open(builder, 0);
    };
    switch (Type_builder.build_and_dispose(builder)) {
    | Ok([ty]) =>
      switch (name) {
      | Some(name) when Config.profile^ != Some(Release) =>
        Heap_type.set_type_name(wasm_mod, ty, name);
        List.iteri(
          (index, (name, _)) => {
            switch (name) {
            | Some(name) =>
              Heap_type.set_field_name(wasm_mod, ty, index, name)
            | _ => ()
            }
          },
          fields,
        );
      | _ => ()
      };
      ty;
    | _ => assert(false)
    };
  };
  // Core supertype for all Grains values.
  let grain_value =
    build_struct(
      ~open_=true,
      ~name="GrainValue",
      [field(~name="valueTag", Type.int32)],
    );

  // Core supertype for all compound values (tuples, records, variants, closures).
  let grain_compound_value =
    build_struct(
      ~open_=true,
      ~supertype=grain_value,
      ~name="GrainCompoundValue",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
      ],
    );

  // Core data representations for tuples, arrays, records, variants, closures, strings, bytes, and numbers.
  let grain_tuple =
    build_struct(
      ~supertype=grain_compound_value,
      ~name="GrainTuple",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
      ],
    );
  let grain_array =
    build_struct(
      ~supertype=grain_compound_value,
      ~name="GrainArray",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
      ],
    );
  let grain_record =
    build_struct(
      ~supertype=grain_compound_value,
      ~name="GrainRecord",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
        field(~name="typeHash", ref_i31()),
        field(~name="typeId", ref_i31()),
      ],
    );
  let grain_variant =
    build_struct(
      ~supertype=grain_compound_value,
      ~name="GrainVariant",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
        field(~name="typeHash", ref_i31()),
        field(~name="typeId", ref_i31()),
        field(~name="variantTag", ref_i31()),
      ],
    );
  let grain_closure =
    build_struct(
      ~open_=true,
      ~supertype=grain_compound_value,
      ~name="GrainClosure",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
      ],
    );
  let grain_closure_full = functype =>
    build_struct(
      ~supertype=grain_closure,
      [
        field(~name="valueTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="cycleMarker",
          Type.int32,
        ),
        field(~name="data", build_array_type(~mutable_=true, ref_any())),
        field(
          ~mutable_=true,
          ~name="wasmFunc",
          Type.from_heap_type(functype, true),
        ),
      ],
    );
  let grain_string =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainString",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~name="data",
          build_array_type(
            ~mutable_=true,
            ~packed_type=Packed_type.int8,
            Type.int32,
          ),
        ),
      ],
    );
  let grain_bytes =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainBytes",
      [
        field(~name="valueTag", Type.int32),
        field(
          ~name="data",
          build_array_type(
            ~mutable_=true,
            ~packed_type=Packed_type.int8,
            Type.int32,
          ),
        ),
      ],
    );
  let grain_number =
    build_struct(
      ~supertype=grain_value,
      ~open_=true,
      ~name="GrainBoxedNumber",
      [
        field(~name="valueTag", Type.int32),
        field(~name="numberTag", Type.int32),
      ],
    );
  let grain_int64 =
    build_struct(
      ~supertype=grain_number,
      ~name="GrainInt64",
      [
        field(~name="valueTag", Type.int32),
        field(~name="numberTag", Type.int32),
        field(~name="value", Type.int64),
      ],
    );
  let grain_float64 =
    build_struct(
      ~supertype=grain_number,
      ~name="GrainFloat64",
      [
        field(~name="valueTag", Type.int32),
        field(~name="numberTag", Type.int32),
        field(~name="value", Type.float64),
      ],
    );
  let grain_rational =
    build_struct(
      ~supertype=grain_number,
      ~name="GrainRational",
      [
        field(~name="valueTag", Type.int32),
        field(~name="numberTag", Type.int32),
        field(~name="numerator", ref_any()),
        field(~name="denominator", ref_any()),
      ],
    );
  let grain_big_int =
    build_struct(
      ~supertype=grain_number,
      ~name="GrainBigInt",
      [
        field(~name="valueTag", Type.int32),
        field(~name="numberTag", Type.int32),
        field(
          ~mutable_=true,
          ~packed_type=Packed_type.int8,
          ~name="flags",
          Type.int32,
        ),
        field(~name="limbs", build_array_type(~mutable_=true, Type.int64)),
      ],
    );
  let grain_int32 =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainInt32",
      [
        field(~name="valueTag", Type.int32),
        field(~name="value", Type.int32),
      ],
    );
  let grain_float32 =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainFloat32",
      [
        field(~name="valueTag", Type.int32),
        field(~name="value", Type.float32),
      ],
    );
  let grain_uint32 =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainUint32",
      [
        field(~name="valueTag", Type.int32),
        field(~name="value", Type.int32),
      ],
    );
  let grain_uint64 =
    build_struct(
      ~supertype=grain_value,
      ~name="GrainUint64",
      [
        field(~name="valueTag", Type.int32),
        field(~name="value", Type.int64),
      ],
    );

  {
    grain_value: Type.from_heap_type(grain_value, false),
    grain_compound_value: Type.from_heap_type(grain_compound_value, false),
    grain_tuple: Type.from_heap_type(grain_tuple, false),
    grain_array: Type.from_heap_type(grain_array, false),
    grain_record: Type.from_heap_type(grain_record, false),
    grain_variant: Type.from_heap_type(grain_variant, false),
    grain_closure: Type.from_heap_type(grain_closure, false),
    grain_closure_full,
    grain_string: Type.from_heap_type(grain_string, false),
    grain_bytes: Type.from_heap_type(grain_bytes, false),
    grain_number: Type.from_heap_type(grain_number, false),
    grain_int64: Type.from_heap_type(grain_int64, false),
    grain_float64: Type.from_heap_type(grain_float64, false),
    grain_rational: Type.from_heap_type(grain_rational, false),
    grain_big_int: Type.from_heap_type(grain_big_int, false),
    grain_int32: Type.from_heap_type(grain_int32, false),
    grain_float32: Type.from_heap_type(grain_float32, false),
    grain_uint32: Type.from_heap_type(grain_uint32, false),
    grain_uint64: Type.from_heap_type(grain_uint64, false),
    array_mut_any: build_array_type(~mutable_=true, ref_any()),
    array_mut_i8:
      build_array_type(
        ~mutable_=true,
        ~packed_type=Packed_type.int8,
        Type.int32,
      ),
    array_mut_i64: build_array_type(~mutable_=true, Type.int64),
  };
};
