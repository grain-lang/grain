open Grain_diagnostics;
open Grain_parsing;

/** The various error types that can occur during documentation processing. */
type error =
  | /** Indicates a required cli flag is missing. */
    MissingFlag({
      flag: string,
      attr: string,
    })
  | /** Indicates a required labeled parameter type is missing. */
    MissingLabeledParamType({
      name: string,
    })
  | /** Indicates a required unlabeled parameter type is missing. */
    MissingUnlabeledParamType({
      idx: int,
    })
  | /** Indicates that a parameter attribute appears multiple times. */
    ParameterAttributeAppearsMultipleTimes({
      param_name: string,
    })
  | /** Indicates that an attribute that should appear on a function is not on a function. */
    AttributeAppearsOnNonFunction({
      attr: string,
    })
  | /** Indicates that an attribute that can only appear once appears multiple times. */
    AttributeAppearsMultipleTimes({
      attr: string,
    })
  | /** Indicates that an attribute was used in an invalid context. */
    InvalidAttribute({
      name: string,
      attr: string,
    });

/** An exception that represents a documentation error. */
exception Error(Location.t, error);

let report_error = (ppf, err) => {
  switch (err) {
  | MissingFlag({flag, attr}) =>
    Format.fprintf(
      ppf,
      "Must provide %s when generating docs with `%s` attribute.",
      flag,
      attr,
    )
  | MissingLabeledParamType({name}) =>
    Format.fprintf(
      ppf,
      "Unable to find a matching function parameter for %s. Make sure a parameter exists with this label or use `@param <param_index> %s` for unlabeled parameters.",
      name,
      name,
    )
  | MissingUnlabeledParamType({idx}) =>
    Format.fprintf(
      ppf,
      "Unable to find a type for parameter at index %d. Make sure a parameter exists at this index in the parameter list.",
      idx,
    )
  | ParameterAttributeAppearsMultipleTimes({param_name}) =>
    Format.fprintf(
      ppf,
      "Parameter @%s is only allowed to have one @param attribute.",
      param_name,
    )
  | AttributeAppearsOnNonFunction({attr}) =>
    Format.fprintf(ppf, "Attribute @%s is only allowed on functions.", attr)
  | AttributeAppearsMultipleTimes({attr}) =>
    Format.fprintf(ppf, "Attribute @%s is only allowed to appear once.", attr)
  | InvalidAttribute({name, attr}) =>
    Format.fprintf(ppf, "Invalid attribute @%s on %s", attr, name)
  };
};

let () =
  Location.register_error_of_exn(
    fun
    | Error(loc, err) =>
      Some(Location.error_of_printer(loc, report_error, err))
    | _ => None,
  );

// ┌──────────────────────────────────────────────────────────────────────────┐
// │                               ERROR UTILITIES                            │
// └──────────────────────────────────────────────────────────────────────────┘

let get_attr_name = attr => {
  Comment_attributes.(
    switch (attr) {
    | Deprecated(_) => "deprecated"
    | Since(_) => "since"
    | History(_) => "history"
    | Param(_) => "param"
    | Returns(_) => "returns"
    | Throws(_) => "throws"
    | Example(_) => "example"
    }
  );
};

/** Ensures that the given value is not already set by another attribute. */
let only_one_attribute = (attr_value: option('a), attribute) => {
  let {attr, attr_loc}: Comment_attributes.t = attribute;
  switch (attr_value) {
  | Some(_) =>
    raise(
      Error(
        attr_loc,
        AttributeAppearsMultipleTimes({attr: get_attr_name(attr)}),
      ),
    )
  | None => ()
  };
};

/** Indicates that an attribute appears on something other than a function. */
let attribute_appears_on_non_function = attribute => {
  let {attr, attr_loc}: Comment_attributes.t = attribute;
  Error(
    attr_loc,
    AttributeAppearsOnNonFunction({attr: get_attr_name(attr)}),
  );
};

/** Indicates that an attribute is used in an invalid context. */
let invalid_attribute = (~name, attribute) => {
  let {attr, attr_loc}: Comment_attributes.t = attribute;
  Error(
    attr_loc,
    InvalidAttribute({
      name,
      attr: get_attr_name(attr),
    }),
  );
};

/** Checks that no attributes are present. */
let no_attributes_check = (~name, attributes) => {
  switch ((attributes: list(Comment_attributes.t))) {
  | [] => ()
  | [{attr, attr_loc}, ..._] =>
    raise(
      Error(
        attr_loc,
        InvalidAttribute({
          name,
          attr: get_attr_name(attr),
        }),
      ),
    )
  };
};

let get_current_version = (~current_version, ~loc, ~attr) => {
  switch (current_version) {
  | Some(version) => version
  | None =>
    raise(
      Error(
        loc,
        MissingFlag({
          flag: "--current-version",
          attr,
        }),
      ),
    )
  };
};
