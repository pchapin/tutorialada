
#include <stdio.h>
#include "library.h"

int sum( int x, int y )
{
    return x + y;
}


void print_int( int value )
{
    printf( "%d\n", value );
}


// The formatting is silly and just for demonstration purposes.
void format_integer( struct IntFormat *value )
{
    sprintf( value->formatted, "(* %04d *)", value->number );
}
