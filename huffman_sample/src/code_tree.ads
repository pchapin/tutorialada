---------------------------------------------------------------------------
-- FILE   : code_tree.ads
-- SUBJECT: Interface to the Huffman code tree.
--
---------------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Interfaces;

package Code_Tree is

   type Fraction_Type is delta 0.001 range 0.0 .. 1.0;

   type Byte_Information is
      record
         Value    : Interfaces.Unsigned_8;
         Count    : Natural;
         Fraction : Fraction_Type;
         Code     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   -- Reads the file with the given name and counts the number of times each byte occurs.
   procedure Read_File(Name : in String);

   -- Build the Huffman code tree based on information gathered by a previous call to
   -- Read_File.
   --
   procedure Build_Tree;

   -- Assign Huffman codes based on the tree computed by a previous call to Build_Tree.
   procedure Assign_Codes;

   -- Returns information about a particular byte value. For complete information, only call
   -- this function after Assign_Codes has done its work. Otherwise some of the fields in the
   -- returned Byte_Information record may be invalid.
   --
   function Get_Byte(Byte : Interfaces.Unsigned_8) return Byte_Information;

end Code_Tree;
