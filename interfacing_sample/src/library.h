
#ifndef LIBRARY_H
#define LIBRARY_H

// Function that adds two integers.
int sum(int x, int y);

// Function that prints an integer.
void print_int(int value);

// This example demonstrates using a structure and a C-style string.
struct IntFormat
{
    int  number;
    char formatted[128];
};

// Updates value->formatted with a formatted version of value->number
void format_integer(struct IntFormat *value);

#endif
