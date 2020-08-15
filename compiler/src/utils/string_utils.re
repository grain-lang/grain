let explode = string => {
  let get_char = String.get(string);
  List.init(String.length(string), get_char);
};

let slice = (~first=0, ~last=?, string) => {
  let stringLength = String.length(string);
  let last = Option.value(~default=stringLength, last);
  let newLength = last - first;
  if (newLength > stringLength) {
    string;
  } else if (first >= stringLength) {
    "";
  } else {
    String.sub(string, first, newLength);
  };
};
