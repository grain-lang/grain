# Printing in Grain

One of Grain's most developer-focused features is the ability to print any value without having to first define a printer. Some type information is kept around at runtime that allows this to work. As this does take up some extra memory and a few extra CPU cycles, this behavior can be disabled via the `--elide-type-info` flag, which may be useful in a production setting where this functionality is no longer needed.

This document is primarily aimed at describing the data format used to store the names of enum variants and record fields. For more information on information stored with each individual value, see the document on [data representations](./data_representations.md).

## The Master Type Metadata List

Grain reserves a memory segment before the runtime heap for storing available printing information; the metadata information is populated into a `data` section of the linked `.wasm` module when a Grain program is compiled and linked.

The data is structured as a hash table (with the number of buckets chosen based on the total number of types registered by a program at link time), mapping a unique hash value for each Grain type (as described in [data representations](./data_representations.md)) to an offset in the bucket data section containing that type's metadata. More specifically, when initialized into memory, the metadata section will contain (in this order):
- **Buckets:** Hash table buckets, each containing an offset into the following section and the number of items stored in that bucket (a pair of `i32`s).
- **Bucket Data:** Mappings of type IDs to offsets in the metadata section (stored as pairs of `i32`s).
- **Metadata:** A blob of metadata entries for each Grain type known to the program.

Here is the layout for a single entry in the metadata blob corresponding to an enum type:

```plaintext
+0  32-bit size of complete type info block
<start repeated pattern>
+0  32-bit size of constructor name block
+4  32-bit offset to field data (for inline record constructors) or 0 (for tuple-like constructors)
+4  32-bit constructor id
+8  32-bit length of constructor name
+12 n-bit string
  <start repeated pattern> (only for inline record constructors)
  +0  32-bit size of record field name block
  +4  32-bit length of record field name
  +8  n-bit string
  <end repeated pattern>
<end repeated pattern>
```

And here is the layout for a single record type:

```plaintext
+0  32-bit size of complete type info block
<start repeated pattern>
+0  32-bit size of record field name block
+4  32-bit length of record field name
+8  n-bit string
<end repeated pattern>
```

The block sizes are all necessary as padding is added to keep alignment.
