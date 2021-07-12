#ifndef C_UTILS_DARRAY_H
#define C_UTILS_DARRAY_H

#include <stdlib.h>
#include <assert.h>

#include "integer.h"

#define GENERATE_DARRAY_HEADER(TYPE,ARRAY_NAME) \
typedef struct ARRAY_NAME { \
  u32 reserved; \
  u32 length; \
  TYPE *data; \
} ARRAY_NAME; \
ARRAY_NAME init_##ARRAY_NAME(u32 reserved); \
u64 ARRAY_NAME##_size(ARRAY_NAME arr); \
void ARRAY_NAME##_push(ARRAY_NAME *arr, TYPE push_value); \
TYPE ARRAY_NAME##_pop(ARRAY_NAME *arr)


#define GENERATE_DARRAY_CODE(TYPE, ARRAY_NAME) \
 \
ARRAY_NAME init_##ARRAY_NAME(u32 reserved) { \
  assert(reserved > 1 && "reserve size must be at least 2"); \
  ARRAY_NAME r; \
  r.reserved = reserved; \
  r.length = 0; \
  r.data = malloc(r.reserved * sizeof(TYPE)); \
  assert(r.data != NULL && "initial dynamic array malloc failed"); \
  return r; \
} \
 \
u64 ARRAY_NAME##_size(ARRAY_NAME arr) { \
  return arr.length * sizeof(TYPE); \
} \
 \
void ARRAY_NAME##_push(ARRAY_NAME *arr, TYPE push_value) { \
  if(arr->length == arr->reserved) { \
    arr->reserved += arr->reserved / 2; \
    arr->data = realloc(arr->data, arr->reserved * sizeof(TYPE)); \
    assert(arr->data != NULL && "realloc to expand dynamic array failed"); \
  } \
  arr->data[arr->length] = push_value; \
  arr->length++; \
} \
 \
TYPE ARRAY_NAME##_pop(ARRAY_NAME *arr) { \
  assert(arr->length > 0 && "failed to pop; dynamic array has 0 elements"); \
  arr->length--; \
  return arr->data[arr->length]; \
}



#endif
