type t = Bigarray.Array1.t(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout);

let create : int => t = size => {
  Bigarray.Array1.create(Bigarray.char, Bigarray.c_layout, size);
}

let to_string : t => string = bs => {
  String.init(Bigarray.Array1.dim(bs), i => bs.{i})
}

let of_string : string => t = str => {
  let bs = create(String.length(str));
  String.iteri((i, c) => bs.{i} = c, str);
  bs
}

let add_bigstring : (t, t, int) => unit = (dest, src, pos) => {
  let src_size = Bigarray.Array1.dim(src);
  let dest_size = Bigarray.Array1.dim(dest);
  if (pos + src_size > dest_size) {
    raise(Invalid_argument("Source bigstring will not fit in the destination bigstring at the specified position"))
  }
  for (i in 0 to src_size - 1) {
    dest.{i + pos} = src.{i}
  };
}

let concat : list(t) => t = (bigstrings) => {
  let rec process = (start, bigstrings, new_bs) => {
    switch (bigstrings) {
      | [bs, ...tl] => {
        let size = Bigarray.Array1.dim(bs);
        for (i in 0 to size - 1) {
          new_bs.{i + start} = bs.{i}
        };
        process(start + size, tl, new_bs);
      }
      | [] => new_bs
    }
  }
  let len = List.fold_left((acc, bs) => acc + Bigarray.Array1.dim(bs), 0, bigstrings);
  process(0, bigstrings, create(len))
}

let input : (in_channel, t, int, int) => unit = (ic, str, pos, len) => {
  for (i in pos to pos + len - 1) {
    str.{i} = input_char(ic)
  };
}

let output : (out_channel, t) => unit = (oc, str) => {
  for (i in 0 to Bigarray.Array1.dim(str) - 1) {
    output_char(oc, str.{i})
  };
}

let set_int32_le : (t, int, int32) => unit = (bs, pos, i) => {
  open Int32;
  let lowest = logand(i, 0x000000ffl);
  let low = shift_right_logical(logand(i, 0x0000ff00l), 8);
  let high = shift_right_logical(logand(i, 0x00ff0000l), 16);
  let highest = shift_right_logical(logand(i, 0xff000000l), 24);
  bs.{pos} = Char.chr(to_int(lowest));
  bs.{pos + 1} = Char.chr(to_int(low));
  bs.{pos + 2} = Char.chr(to_int(high));
  bs.{pos + 3} = Char.chr(to_int(highest));
}
