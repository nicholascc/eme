#ifndef C_UTILS_HASH_H
#define C_UTILS_HASH_H
#include "integer.h"

// uses FNV-1a
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function#FNV-1a_hash
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

// equivalent to boost::hash_combine
// https://www.boost.org/doc/libs/1_55_0/doc/html/hash/reference.html#boost.hash_combine
static u64 hash_combine(u64 a, u64 b) {
  a ^= b + 0x9e3779b9 + (a << 6) + (a >> 2);
  return a;
}

// based on splitmix64
// https://xorshift.di.unimi.it/splitmix64.c
static u64 hash_u64(u64 x) {
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    x = x ^ (x >> 31);
    return x;
}


#endif
