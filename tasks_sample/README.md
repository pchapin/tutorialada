README
======

This sample demonstrates Ada tasking. It is intended to do something interesting with the
producer consumer pattern, but for now it just computes prime numbers. It prints a dot every
second while it is checking a value for primality. However, the check is done so quickly, that
typically no dots appear!

The program could be improved, as a demonstration at least, by using extended precision integers
so that very large numbers could be (inefficiently) checked, taking a long enough time to see
the dots.