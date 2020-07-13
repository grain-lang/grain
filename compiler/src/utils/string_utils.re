let explode = string => {
  let get_char = String.get(string);
  List.init(String.length(string), get_char);
};

let slice = (~last=?, string) => {
  let stringLength = String.length(string);
  let last = Option.value(~default=stringLength, last);
  if (last > stringLength) {
    string;
  } else {
    String.sub(string, 0, last);
  };
};
