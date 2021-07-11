# c-utils

A set of small single-header-file libraries I use for my C projects.

### integer.h

Definitions for basic signed and unsigned integer types: `u8`, `u16`, `u32`, `u64`, `s8`, `s16`, `s32`, and `s64`.

### darray.h

A generic implementation of a dynamic array, along with some functions for interacting with it. The macro is called using the type of the elements of the array, and a name for the generated struct. For example, `GENERATE_DARRAY_HEADER(u8, darray_byte);` and `GENERATE_DARRAY_FUNCTIONS(u8, darray_byte);` together define a struct `darray_byte` (representing a dynamic array with elements of type `u8`) and its associated functions. The implementation provides the following definitions:

```c
typedef struct DARRAY_NAME {
  u32 reserved; // Amount of space reserved (in terms of elements, not bytes)
  u32 length; // Amount of elements in array (not size in bytes)
  TYPE *data;
} DARRAY_NAME;

darray_TYPE initialize_DARRAY_NAME(u32 reserved); // Initializes the array

u64 DARRAY_NAME_size(DARRAY_NAME arr); // Gets the size of the array (length * sizeof(TYPE))

void DARRAY_NAME_push(DARRAY_NAME *arr, TYPE push_value); // Pushes a value to the end of the array.

TYPE DARRAY_NAME_push(DARRAY_NAME *arr); // Removes an element from the end of the array and returns its value.
```

`GENERATE_DARRAY_HEADER` generates the struct definition and function headers, while `GENERATE_DARRAY_FUNCTIONS` generates the C code for the functions.

###### Note: `darray.h` includes upon `integer.h`.
