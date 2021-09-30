#ifndef ERROR_H
#define ERROR_H

#include <stdbool.h>

bool should_exit_after_parsing; // indicates whether an error has occurred during parsing which would cause the later stages of compilation to fail.
bool should_exit_after_type_inference; // same as above but for type inference phase

// Given an error message, line, character, and file, prints out the error with a nice display of the line in which it occurred.
// If any of the last 3 values are negative, their values are assumed to not be known the error message adapts to display correctly. (for example, it only displays the file if the line is negative, or it says "unknown location" if the file_id is invalid)
void print_error_message(char *message, int file_id, int line, int character);


// Given a file id and a line number, prints that line out in the below format:

// [In file: $filename]
// $line | $line_contents

// If line is negative, just the [In file: $filename] is printed.
// If file_id is negative, the function will cause an error.
void print_one_line_in_file(int file_id, int line);

// Same behavior as print_one_line_in_file, but will print `lines_to_print` lines before the indicated line if it can.
void print_lines_in_file(int file_id, int line, int lines_to_print);

#endif /* end of include guard: ERROR_H */
