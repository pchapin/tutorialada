---------------------------------------------------------------------------
-- FILE         : single_list.ads
-- SUBJECT      : Specification of a singly linked list package.
---------------------------------------------------------------------------

with Ada.Finalization;

generic
   type Item_Type is private;
package Single_List is

   type List_Type is limited new Ada.Finalization.Limited_Controlled with private;
   type Iterator is private;

   -- Adds (a copy of) the new item to the end of the list.
   procedure Append(L : in out List_Type; Item : in Item_Type);

   -- Adds (a copy of) the new item to the front of the list.
   procedure Prepend(L : in out List_Type; Item : in Item_Type);

   -- The Iterator interface.
   function  Start_Iteration(L : List_Type) return Iterator;
   function  Is_Null(It : Iterator) return Boolean;
   function  Get_Item(It : Iterator) return Item_Type;
   procedure Next_Item(It : in out Iterator);

private

   type Node;
   type Node_Pointer is access Node;
   type Node is
      record
         Data : Item_Type;
         Next : Node_Pointer;
      end record;

   type List_Type is new Ada.Finalization.Limited_Controlled with
      record
         Head : Node_Pointer := null;
         Tail : Node_Pointer := null;
      end record;

   procedure Finalize(L : in out List_Type);

   type Iterator is new Node_Pointer;

end Single_List;
