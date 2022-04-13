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

The following is a ground-up implementation of an array data type in Eme, along with a small example of its use. The implementation includes a subscript operator overload and an iterator implementation, so that the data structure is easily used in user programs.

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

  // Use a loop to fill its contents, by calculating squares from 0-9.
  // This uses an iterator, implemented for the array datatype below this function.
  each(arr) it = it_index*it_index;

  // Return the sixth element (which should be 25)
  return arr[5];
}




// This is the struct representing the iterator type.
array_iter :: struct(T: type) {
  arr: array(T);
  index: int;
}

// Creates a new iterator from an array.
iterator_make :: fn #inline #operator (a: array($T)) -> array_iter(T) {
  r: array_iter(T);
  r.arr = a;
  r.index = 0;
  return r;
}

// Gets element the iterator is currently at.
iterator_element :: fn #inline #operator (a: array_iter($T)) -> ^T {
  return a.arr^[a.index]; // This operator returns the pointer to the element in
                          // the array, rather than the element itself. This is
                          // automatically defined with the subscript operator.
}

// Gets the index of the element the iterator is currently at.
iterator_index :: fn #inline #operator (a: array_iter($T)) -> int {
  return a.index;
}

// Gets whether the iterator is finished going through the whole array.
iterator_done :: fn #inline #operator (a: array_iter($T)) -> bool {
  return a.index >= a.arr.length;
}

// Moves the iterator to the next element.
iterator_next :: fn #inline #operator (a: ^array_iter($T)) {
  a.index = a.index + 1;
}

```
