type color = Pastel.colorName;

let ansi =
    (
      ~reset=?,
      ~bold=?,
      ~dim=?,
      ~italic=?,
      ~underline=?,
      ~inverse=?,
      ~hidden=?,
      ~strikethrough=?,
      ~color=?,
      ~backgroundColor=?,
      text,
    ) => {
  Pastel.createElement(
    ~reset?,
    ~bold?,
    ~dim?,
    ~italic?,
    ~underline?,
    ~inverse?,
    ~hidden?,
    ~strikethrough?,
    ~color?,
    ~backgroundColor?,
    ~children=[text],
    (),
  );
};
