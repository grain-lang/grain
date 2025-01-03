open Grain_typed;
open Cmi_format;

let construct_type_metadata_table = metas => {
  // Create a hash table mapping "type hashes" unique to each Grain type to
  // offsets in the metadata section

  // Structure:
  // | Hash table: array of (offset into mappings data, bucket size) for each hash (type_hash % num_buckets)
  // | Mappings: arrays of (type_hash, offset into actual metadata table) for each hash table bucket
  // | Metadata: actual metadata content; appropriate offset for type metadata found through table data
  let exceptions_buf = Buffer.create(256);
  let exception_metas = List.map(m => m.ctm_exceptions, metas);
  let exceptions_section_len =
    List.fold_left((len, e) => String.length(e) + len, 0, exception_metas)
    + 4;
  Buffer.add_int32_le(exceptions_buf, Int32.of_int(exceptions_section_len));
  List.iter(Buffer.add_string(exceptions_buf), exception_metas);

  let exception_meta = (Buffer.contents(exceptions_buf), [(0, 0)]);
  let non_exception_metas =
    List.map(m => (m.ctm_metadata, m.ctm_offsets_tbl), metas);
  let metas = [exception_meta, ...non_exception_metas];

  let metadata_buf = Buffer.create(256);
  let curr_offset = ref(0);
  let hash_to_offset =
    List.flatten(
      List.map(
        ((meta, offsets_tbl)) => {
          Buffer.add_string(metadata_buf, meta);
          let padding_bytes = String.length(meta) mod 8;
          for (_ in 1 to padding_bytes) {
            Buffer.add_int8(metadata_buf, 0);
          };
          let hash_to_offset =
            List.map(
              ((hash, offset)) => (hash, offset + curr_offset^),
              offsets_tbl,
            );
          curr_offset := curr_offset^ + String.length(meta) + padding_bytes;
          hash_to_offset;
        },
        metas,
      ),
    );
  let rec next_pow_of_2 = (num, res) =>
    if (num <= 1) {
      1 lsl res;
    } else {
      next_pow_of_2(num lsr 1, res + 1);
    };
  let num_entries = List.length(hash_to_offset);
  // Choose number of buckets to be the greatest power of 2 <= # table entries
  // (limit 4K) as a memory/lookup speed tradeoff (expect 1-2 entries/bucket)
  let num_buckets = min(4096, next_pow_of_2(num_entries, 0));
  let buckets = Array.make(num_buckets, []);
  List.iter(
    ((hash, _) as hash_and_offset) => {
      let hash_hash = hash mod num_buckets;
      buckets[hash_hash] = [hash_and_offset, ...buckets[hash_hash]];
    },
    hash_to_offset,
  );
  for (i in 0 to num_buckets - 1) {
    buckets[i] = List.rev(buckets[i]);
  };

  let tbl_buckets_buf = Buffer.create(num_buckets * 8);
  let tbl_vals_buf = Buffer.create(num_entries * 8);
  // Store the number of buckets at the beginning of the metadata section
  Buffer.add_int32_le(tbl_buckets_buf, Int32.of_int(num_buckets));
  // For 8-byte alignment
  Buffer.add_int32_le(tbl_buckets_buf, 0l);

  // # buckets + buckets + data (2 i32s each)
  let tbl_size = 8 + (num_buckets + num_entries) * 8;
  // Initialize offset to point after the table buckets
  let tbl_data_offset = ref(8 + num_buckets * 8);
  Array.iter(
    hash_to_offset => {
      Buffer.add_int32_le(tbl_buckets_buf, Int32.of_int(tbl_data_offset^));
      Buffer.add_int32_le(
        tbl_buckets_buf,
        Int32.of_int(List.length(hash_to_offset)),
      );
      List.iter(
        ((hash, offset)) => {
          Buffer.add_int32_le(tbl_vals_buf, Int32.of_int(hash));
          Buffer.add_int32_le(tbl_vals_buf, Int32.of_int(offset + tbl_size));
          tbl_data_offset := tbl_data_offset^ + 8;
        },
        hash_to_offset,
      );
    },
    buckets,
  );
  Buffer.add_buffer(tbl_buckets_buf, tbl_vals_buf);
  Buffer.add_buffer(tbl_buckets_buf, metadata_buf);
  Buffer.to_bytes(tbl_buckets_buf);
};
