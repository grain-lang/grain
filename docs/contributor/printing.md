# Printing in Grain

One of Grain's most developer-focused features is the ability to print any value without having to first define a printer. Some type information is kept around at runtime that allows this to work. As this does take up some extra memory and a few extra CPU cycles, this behavior can be disabled via the `--elide-type-info` flag, which may be useful in a production setting where this functionality is no longer needed.

This document is primarily aimed at describing the data format used to store the names of enum variants and record fields. For more information on information stored with each individual value, see the document on [data representations](./data_representations.md).

## The Master Type Metadata List

Grain reserves a single 32-bit word in the runtime heap space as a pointer to all available printing information. This is done to allow modules to register their printing information without needing to depend on any other modules, and by extension, it allows them to register their information before printing is available.

The data is structured as a linked list. To register its printing information, a module just needs to cons its information onto the list. Here's what the payload for a single module looks like:

```plaintext
+0  32-bit pointer to next block
+4  32-bit module id
+8  32-bit number of types contained in the block, where a type is an enum or record declaration
+12 n-bit type information
```

Here is the layout for a single enum type:

```plaintext
+0  32-bit size of complete type info block
+4  32-bit type id
<start repeated pattern>
+0  32-bit size of constructor name block
+4  32-bit constructor id
+8  32-bit length of constructor name
+12 n-bit string
<end repeated pattern>
```

And here is the layout for a single record type:

```plaintext
+0  32-bit size of complete type info block
+4  32-bit type id
<start repeated pattern>
+0  32-bit size of record field name block
+4  32-bit length of record field name
+8  n-bit string
<end repeated pattern>
```

The block sizes are all necessary as padding is added to keep alignment.

### Considerations

#### Data Format

You may have looked at the data format and thought that it might be easier to use if all of the strings were allocated individually. This is probably true, but ideally we'll move this information into the module's `data` section. When that happens, it won't be possible to have pointers in the block as the data must be purely static.

#### Efficiency

This format is admittedly inefficient. The worst case lookup on any given type is O(_kn_) where _k_ is the number of modules loaded and _n_ is the number of types defined in the module. Constant-time lookups on module information would be fairly easy to obtain at the expense of memory.
