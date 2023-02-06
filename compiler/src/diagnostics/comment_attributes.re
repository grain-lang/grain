exception InvalidAttribute(string);
exception MalformedAttribute(string, string);

type attr_name = string;
type attr_desc = string;
// The `attr_type` always starts as `None` and is applied later by something like Graindoc
type attr_type = option(string);
type attr_version = string;

type t =
  | Param({
      attr_name,
      attr_type,
      attr_desc,
    })
  | Returns({
      attr_desc,
      attr_type,
    })
  // Currently only accepts single-line examples
  | Example({attr_desc})
  | Section({
      attr_name,
      attr_desc,
    })
  | Deprecated({attr_desc})
  | Since({attr_version})
  | History({
      attr_version,
      attr_desc,
    })
  | Throws({
      attr_type,
      attr_desc,
    });

type parsed_graindoc = (option(string), list(t));
