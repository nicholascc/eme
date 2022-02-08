# c-utils

A set of small single-header-file libraries I use for my C projects.

### integer.h

Definitions for basic signed and unsigned integer types: `u8`, `u16`, `u32`, `u64`, `s8`, `s16`, `s32`, and `s64`.

### darray.h

A generic implementation of a dynamic array, along with some functions for interacting with it. The macro is called using the type of the elements of the array, and a name for the generated struct. For example, `GENERATE_DARRAY_HEADER(u8, darray_byte);` and `GENERATE_DARRAY_CODE(u8, darray_byte);` together define a struct `darray_byte` (representing a dynamic array with elements of type `u8`) and its associated functions. The implementation provides the following definitions:

```c
typedef struct DARRAY_NAME {
  u32 reserved; // Amount of space reserved (in terms of elements, not bytes)
  u32 length; // Amount of elements in array (not size in bytes)
  TYPE *data;
} DARRAY_NAME;

DARRAY_NAME initialize_DARRAY_NAME(u32 reserved); // Initializes the array

u64 DARRAY_NAME_size(DARRAY_NAME arr); // Gets the size of the array (length * sizeof(TYPE))

void DARRAY_NAME_push(DARRAY_NAME *arr, TYPE push_value); // Pushes a value to the end of the array.

TYPE DARRAY_NAME_push(DARRAY_NAME *arr); // Removes an element from the end of the array and returns its value.
```

`GENERATE_DARRAY_HEADER` generates the struct definition and function headers, while `GENERATE_DARRAY_CODE` generates the C code for the functions. It is recommended to place the first macro in the header file, and the second macro in the corresponding C code file.

###### Note: `darray.h` includes `integer.h`.

### hash.h

Provides functions for hashing strings for use in hash tables. `hash_string` hashes a string with a given length, and `hash_string_nullterm` hashes a null-terminated string without including the null byte (so that you are able to call `hash_string` and `hash_string_nullterm` on the same underlying string and get the same hash).

### table.h

Provides a generic implementation of a basic hash table which borrows from Crafting Interpreters' design. Keys are must be passed to `table.h` pre-hashed as unsigned 64-bit integers; you should use `table.h` along with `hash.h` or your own hash function specific to your data structures.

The macro is called usig the type of the value stored in the table, and a name for the generated struct. For example `GENERATE_TABLE_HEADER(char *, String_Table);` and `GENERATE_TABLE_CODE(u8, String_Table);` together define a table `String_Table` which uses `char *`s as values. It also defines a struct `String_Table_Entry` which will be used internally. The implementation provides the following functions:

```c
typedef struct TABLE_NAME {
  u32 count; // The amount of elements currently stored in the table.
  u32 capacity; // The current capacity of the table. Must always be a power of two.
                // After initialization this will be 0, since memory is only
                // allocated once the table has elements.
  TABLE_NAME_Entry *entries;
} TABLE_NAME;

TABLE_NAME init_TABLE_NAME(); // Makes a new table.
void free_TABLE_NAME(TABLE_NAME *table); // Frees a table.
void expand_TABLE_NAME(TABLE_NAME *table, u32 capacity); // Expands a table to a new, larger capacity. capacity must be a power of two.
void allocate_TABLE_NAME(TABLE_NAME *table, u32 capacity); // Actually allocates the table. capacity must be a power of two.
bool get_TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE *value); // Gets a value from the table, and returns false if no value is found.
bool set_TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE value); // Sets a value in the table, and returns true if the value has not been set before.
VALUE_TYPE *put_and_get_ptr_TABLE_NAME(TABLE_NAME *table, u64 key); // Adds an entry with key 'key' to the table, and returns a pointer to the value associated with that entry.
void add_all_TABLE_NAME(TABLE_NAME *from, TABLE_NAME *to); // Copies all data from 'from' into 'to'.
```
