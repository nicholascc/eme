#ifndef C_UTILS_HASH_H
#define C_UTILS_HASH_H
#include "integer.h"

// uses FNV

#define FNV_PRIME 1099511628211ull
#define FNV_BASIS 14695981039346656037ull

static u64 hash_string(const char *key, int length) {
  u64 hash = FNV_BASIS;
  for (int i = 0; i < length; i++) {
    hash ^= (u8)key[i];
    hash *= FNV_PRIME;
  }
  return hash;
}

static u64 hash_string_nullterm(const char *key) {
  u64 hash = FNV_BASIS;
  u8 c;
  while(c = *key++) {
    hash ^= c;
    hash *= FNV_PRIME;
  }
  return hash;
}

#undef FNV_PRIME
#undef FNV_BASIS

#endif
