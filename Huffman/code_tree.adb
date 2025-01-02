---------------------------------------------------------------------------
-- FILE    : code_tree.adb
-- SUBJECT : Implementation of the Huffman code tree.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
with Ada.Sequential_IO;

package body Code_Tree is

   -- Is this really the best way to do this?
   package Byte_IO is new Ada.Sequential_IO(Interfaces.Unsigned_8);

   type Tree_Node;
   type Tree_Node_Pointer is access Tree_Node;
   type Tree_Node is
      record
         Count  : Natural;
         Bit    : Natural range 0 .. 1;
         Parent : Tree_Node_Pointer;
      end record;

   subtype Extended_Index_Type is Integer range -1 .. 255;    -- Allow special value for empty.
   subtype Index_Type is Extended_Index_Type range 0 .. 255;
   type    Node_Array is array(Index_Type) of Tree_Node_Pointer;

   Base_Counts : Node_Array;
   Total_Count : Natural := 0;
   Codes       : array(Index_Type) of Ada.Strings.Unbounded.Unbounded_String;


   procedure Read_File(Name : String) is
      Input_File : Byte_IO.File_Type;
      Byte       : Interfaces.Unsigned_8;
   begin
      Byte_IO.Open(Input_File, Byte_IO.In_File, Name);
      while not Byte_IO.End_Of_File(Input_File) loop
         Byte_IO.Read(Input_File, Byte);
         Base_Counts(Index_Type(Byte)).Count := Base_Counts(Index_Type(Byte)).Count + 1;
      end loop;
      Byte_IO.Close(Input_File);

      for I in Base_Counts'Range loop
         Total_Count := Total_Count + Base_Counts(I).Count;
      end loop;
   end Read_File;


   function Get_Byte(Byte : Interfaces.Unsigned_8) return Byte_Information is
      Result : Byte_Information;
   begin
      Result.Value := Byte;
      Result.Count := Base_Counts(Index_Type(Byte)).Count;
      Result.Fraction := Fraction_Type(Float(Result.Count) / Float(Total_Count));
      Result.Code  := Codes(Index_Type(Byte));
      return Result;
   end Get_Byte;


   procedure Build_Tree is
      Active_List : Node_Array;
      Top_Index   : Extended_Index_Type := Index_Type'Last;

      procedure Find_Minimums(Index_Of_Smallest, Index_Of_Next_Smallest : out Index_Type) is
         Index_Of_Lowest : Index_Type := Index_Type'First;  -- Index of the smallest count.
         Index_Of_Low    : Index_Type := Index_Type'First;  -- Index of the next smallest count.
         Lowest_Count : Natural := Natural'Last;         -- Count associated with lowest index.
         Low_Count    : Natural := Natural'Last;         -- Count associated with low index.
      begin
         for I in Index_Type'First .. Top_Index loop
            if Active_List(I).Count < Lowest_Count then
               Index_Of_Low    := Index_Of_Lowest;
               Low_Count       := Lowest_Count;
               Index_Of_Lowest := I;
               Lowest_Count    := Active_List(I).Count;
            elsif Active_List(I).Count < Low_Count then
               Index_Of_Low    := I;
               Low_Count       := Active_List(I).Count;
            end if;
         end loop;
         Index_Of_Smallest      := Index_Of_Lowest;
         Index_Of_Next_Smallest := Index_Of_Low;
      end Find_Minimums;


      procedure Purge_Nodes(A, B : in Index_Type) is
         High_Index : Index_Type := A;  -- This will be the highest of the two indicies.
         Low_Index  : Index_Type := B;  -- This will be the lowest of the two indicies.
         Temp_Index : Index_Type;

         -- This procedure purges one node only.
         procedure Compress_List(Index_To_Remove : in Index_Type) is
         begin
            for I in Index_To_Remove .. Top_Index - 1 loop
               Active_List(I) := Active_List(I + 1);
            end loop;
            Active_List(Top_Index) := null;
            Top_Index := Top_Index - 1;
         end Compress_List;

      begin -- Purge_Nodes
         -- Exchange the indicies if necessary.
         if Low_Index > High_Index then
            Temp_Index := Low_Index;
            Low_Index  := High_Index;
            High_Index := Temp_Index;
         end if;

         -- Purge the high index first (so low index is not affected).
         Compress_List(High_Index);
         Compress_List(Low_Index);
      end Purge_Nodes;

      Index_Of_Smallest      : Index_Type;
      Index_Of_Next_Smallest : Index_Type;
      New_Node : Tree_Node_Pointer;

   begin -- Build_Tree
      Active_List := Base_Counts;
      while Top_Index > 0 loop
         Find_Minimums
            (Index_Of_Smallest      => Index_Of_Smallest,
             Index_Of_Next_Smallest => Index_Of_Next_Smallest);

         New_Node := new Tree_Node;
         Active_List(Index_Of_Smallest).Parent      := New_Node;
         Active_List(Index_Of_Next_Smallest).Parent := New_Node;
         Active_List(Index_Of_Smallest).Bit      := 0;
         Active_List(Index_Of_Next_Smallest).Bit := 1;
         New_Node.Count :=
            Active_List(Index_Of_Smallest).Count + Active_List(Index_Of_Next_Smallest).Count;
         Purge_Nodes(Index_Of_Smallest, Index_Of_Next_Smallest);
         Top_Index := Top_Index + 1;
         Active_List(Top_Index) := New_Node;
      end loop;
   end Build_Tree;


   procedure Assign_Codes is
      use Ada.Strings.Unbounded;

      Current_Node : Tree_Node_Pointer;

      procedure Reverse_String(S : in out Ada.Strings.Unbounded.Unbounded_String) is
         Temp : Character;
      begin
         for I in 1 .. Length(S) / 2 loop
            Temp := Element(Source => S, Index => I);
            Replace_Element(Source => S, Index => I, By => Element(S, Length(S) + 1 - I));
            Replace_Element(Source => S, Index => Length(S) + 1 - I, By => Temp);
         end loop;
      end Reverse_String;

   begin -- Assign_Codes
      for I in Index_Type loop
         Current_Node := Base_Counts(I);
         while Current_Node.parent /= null loop
            if Current_Node.Bit = 1 then
               Append(Codes(I), '1');
            else
               Append(Codes(I), '0');
            end if;
            Current_Node := Current_Node.parent;
         end loop;
         Reverse_String(Codes(I));
      end loop;
   end Assign_Codes;


begin
   for I in Index_Type loop
      Base_Counts(I) := new Tree_Node'(Count => 0, Bit => 0, Parent => null);
   end loop;
end Code_Tree;
