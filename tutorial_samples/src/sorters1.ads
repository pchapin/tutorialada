---------------------------------------------------------------------------
-- FILE    : sorters.ads
-- SUBJECT : Package providing various sorting procedures
--
-- This package is largely illustrative. A real package like this would use an unconstrained
-- array type and be generic. A version showing these things should probably be added to this
-- collection of samples.
---------------------------------------------------------------------------

package Sorters1 is

   -- It's easier to complete the proofs if we deal with fixed sized arrays.
   Block_Size : constant := 16;  -- ... or whatever the application needs.
   subtype Count_Type is Natural  range 0 .. Block_Size;
   subtype Index_Type is Positive range 1 .. Block_Size;
   type Array_Type is array(Index_Type) of Integer;  -- ... or whatever the application needs.

   -- A simple, but inefficient O(n^2) sorting algorithm.
   procedure Bubble_Sort(Values : in out Array_Type)
     with
       Post => (for all I in Values'First .. Values'Last - 1 => (Values(I) <= Values(I + 1)));

   -- Sorts the first part of array Values from Values'First to Limit.
   procedure Selection_Sort(Values : in out Array_Type; Limit : in Index_Type)
     with
       Post => (for all I in Values'First .. Limit - 1 => (Values(I) <= Values(I + 1)));

   -- An efficient O(n log(n)) sorting algorithm.
   procedure Merge_Sort(Values : in out Array_Type)
     with
       Post => (for all I in Values'First .. Values'Last - 1 => (Values(I) <= Values(I + 1)));

end Sorters1;
