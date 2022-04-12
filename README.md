# Eme

Eme is a low-level, strongly typed, and non-managed programming language with a compiler written in C. It's both compiled and interpreted, and is designed to support 'high-level' features such as advanced polymorphism, iterators, and operator overloading.

'Eme' (<img src="http://psd.museum.upenn.edu/epsd/psl/img/popup/Oajg.png" height="20" />) means 'language' in Sumerian, the earliest known written language.

Here's a simple code example which recursively computes the factorial of an integer x.
```
fac :: fn (x: int) -> int {
  if(1 < x) return x*fac(x-1);
  else return x;
}
```

### Installation

Currently, in order to simplify development, Eme only supports x64 Windows. Look in [releases](https://github.com/nicholascc/eme/releases) for an x64 Windows executable, or compile the compiler yourself by linking and including LLVM-C. The downloaded executable can either interpret or compile your program, adjustable through command line options.


### Usage

To interpret a program, run:
```
eme source_file -i
```
This will run the program and output the integer value that the main function returns.

To compile a program, run:
```
eme source_file -o obj_file [asm_file] [llvm_ir_file]
```
This will output an object file, along with optionally an x64 assembly file and an LLVM bytecode file. Eme can't run on it's own yet, so you have to use the outputted object file like a library for a short supporting C program. So to run the object file, compile it with a short C program which calls the main function (`eme`), and outputs its result.

For example, I compile my Eme programs with this file:

```c
#include <stdio.h>
#include <stdint.h>

int64_t eme();

int main() {
  printf("Result: %i\n", eme());
  return 0;
}
```

I use the following MSVC compiler command to compile the above file with the compiler's outputted object file:
```
cl.exe /nologo /Fo"obj/" /Fe"out.exe" support.c /link "out.obj"
```

This produces an output executable (`out.exe`), which is your compiled Eme program!


### Example code

The following is a ground-up implementation of an array data type in Eme, along with a small example of its use. The lack of iterators in the programming language right now make the usage of this data type a little bit difficult, but that will soon change.

```
// We're using the foreign function malloc to allocate data.
malloc :: foreign fn(int) -> int

// Allocates a block of memory of size x * size_of(T).
allocn :: fn #inline ($T, x: int) -> ^T {
  return bit_cast(^T, malloc(size_of(T)*x));
}

// This is a polymorphic struct which can represent an array of any type T.
array :: struct(T: type) {
  length: int;
  data: ^T;
}

// The array subscript operator, which returns a pointer to the nth element of
// a given array a. The #operator tag means you can call this function
// subscript(arr, n) with the syntax arr[n].
subscript :: fn #inline #operator (a: array($T), n: int) -> ^T {
  return bit_cast(^T, size_of(T)*n + bit_cast(int, a.data));
}

// The array allocation/initialization function. Other code can
// override the identifier 'alloc' with a different first argument, so you can
// specialize the 'alloc' function to support any data structure you define.
alloc :: fn #inline (array($T), length: int) -> array(T) {
  r: array(T);
  r.length = length;
  r.data = allocn(T, length);
  return r;
}

// This is the current name of the main function.
eme :: fn () -> int {
  // Allocate an array
  arr := alloc(array(int), 10);

  // Use a loop to fill its contents.
  // This is very clumsy right now, since the language doesn't have iterators.
  // Those will be added soon!
  i := 0;
  while(i < 10) {
    arr[i] = i;
    i = i + 1;
  }

  // Return the fifth element (which should be 5)
  return arr[5];
}
```
