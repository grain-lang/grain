%{
open Comment_attributes
open Graindoc_parser_header
%}

%token PARAM SECTION SINCE HISTORY THROWS RETURNS EXAMPLE DEPRECATED COLON EOL EOF
%token <string> TEXT IDENT INT SEMVER CONSTRUCTOR

%right EOL

%start <Comment_attributes.parsed_graindoc> graindoc

%%

eols:
  | EOL
  | eols EOL {}

multiline_text_help:
  | TEXT { [$1] }
  | multiline_text_help EOL TEXT { $3 :: "\n" :: $1 }
  | multiline_text_help eols EOL TEXT { $4 :: "\n\n" :: $1 }

%inline multiline_text:
  | rev(multiline_text_help) { String.concat "" $1 }

description:
  | { None }
  | multiline_text ioption(eols) { Some($1) }

attribute_text:
  | ioption(eols) multiline_text ioption(eols) %prec EOL { $2 }

param_id:
  | IDENT { LabeledParam ($1, (to_loc $loc)) }
  | INT { PositionalParam (int_of_string $1, to_loc $loc) }

attribute:
  | PARAM param_id COLON attribute_text { { attr=Param({ attr_id=$2; attr_desc=$4 }); attr_loc=to_loc $loc } }
  | RETURNS attribute_text { { attr=Returns({attr_desc=$2}); attr_loc=to_loc $loc; } }
  | EXAMPLE attribute_text { { attr=Example({attr_desc=$2}); attr_loc=to_loc $loc; } }
  | DEPRECATED attribute_text { { attr=Deprecated({attr_desc=$2}); attr_loc=to_loc $loc; } }
  | SINCE SEMVER { { attr=Since({attr_version=$2}); attr_loc=to_loc $loc; } }
  | HISTORY SEMVER COLON attribute_text { { attr=History({ attr_version=$2; attr_desc=$4 }); attr_loc=to_loc $loc; } }
  | THROWS CONSTRUCTOR COLON attribute_text { { attr=Throws({ attr_type=$2; attr_desc=$4 }); attr_loc=to_loc $loc; } }

attributes_help:
  | attribute { [$1] }
  | attributes_help option(eols) attribute { $3 :: $1 }

%inline attributes:
  | { [] }
  | rev(attributes_help) ioption(eols) { $1 }

graindoc:
  | option(eols) description attributes EOF { ($2, $3) }
