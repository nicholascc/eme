#ifndef ERROR_H
#define ERROR_H

#include <stdbool.h>

typedef struct Location {
  int file_id;
  int line;
  int character;
} Location;

Location NULL_LOCATION;

bool should_exit_after_parsing; // indicates whether an error has occurred during parsing which would cause the later stages of compilation to fail.
bool should_exit_after_type_inference; // same as above but for type inference phase

// Given an error location, prints out the error with a nice display of the line in which it occurred.
// Then the function caller must print the actual error message after the prelude given here.
// If any of the 3 values in the Location are negative, their values are assumed to not be known.
// The error message adapts to display correctly for any amount of knowledge.
// (for example, it only displays the line and not the character if the character is negative, or it says "unknown location" if the file_id is invalid)
void print_error(Location l);

// Prints the error followed by the string message if message is non-null.
void print_error_message(char *message, Location l);


// Given a file id and a line number, prints that line out in the below format:

// [In file: $filename]
// $line | $line_contents

// If line is negative, just the [In file: $filename] is printed.
// If file_id is negative, the function will cause an error.
void print_one_line_in_file(int file_id, int line);

// Same behavior as print_one_line_in_file, but will print `lines_to_print` lines before the indicated line if it can.
void print_lines_in_file(int file_id, int line, int lines_to_print);

#endif /* end of include guard: ERROR_H */
