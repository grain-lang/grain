open Grain_parsing;
open Grain_typed;
open Anftree;

module ClearLocationsArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;

  let clear_attribute_locations = attributes => {
    List.map(
      attr => {
        open Typedtree;
        let attr =
          switch (attr.Location.txt) {
          | Disable_gc => Disable_gc
          | Unsafe => Unsafe
          | External_name(name) => External_name(Location.mknoloc(name.txt))
          };
        Location.mknoloc(attr);
      },
      attributes,
    );
  };

  let leave_imm_expression = i => {...i, imm_loc: Location.dummy_loc};

  let leave_comp_expression = c => {
    ...c,
    comp_loc: Location.dummy_loc,
    comp_attributes: clear_attribute_locations(c.comp_attributes),
  };

  let leave_anf_expression = a => {...a, anf_loc: Location.dummy_loc};
};

module ClearLocations = Anf_mapper.MakeMap(ClearLocationsArg);

let clear_locations = ClearLocations.map_anf_program;
