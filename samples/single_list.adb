---------------------------------------------------------------------------
-- FILE         : single_list.adb
-- SUBJECT      : Implementation of a singly linked list package.
---------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Single_List is


   -- Adds (a copy of) the new item to the end of the list.
   procedure Append(L : in out List_Type; Item : in Item_Type) is
      New_Node : Node_Pointer := new Node'(Data => Item, Next => null);
   begin
      if L.Tail /= null then
         L.Tail.Next := New_Node;
         L.Tail := New_Node;
      else
         L.Head := New_Node;
         L.Tail := New_Node;
      end if;
   end Append;


   -- Adds (a copy of) the new item to the front of the list.
   procedure Prepend(L : in out List_Type; Item : in Item_Type) is
      New_Node : Node_Pointer := new Node'(Data => Item, Next => null);
   begin
      if L.Head /= null then
         New_Node.Next := L.Head;
         L.Head := New_Node;
      else
         L.Head := New_Node;
         L.Tail := New_Node;
      end if;
   end Prepend;


   -- The Iterator interface.
   function Start_Iteration(L : List_Type) return Iterator is
   begin
      return Iterator(L.Head);
   end Start_Iteration;


   function Is_Null(It : Iterator) return Boolean is
   begin
      return It = null;
   end Is_Null;


   function Get_Item(It : Iterator) return Item_Type is
   begin
      return It.Data;
   end Get_Item;


   procedure Next_Item(It : in out Iterator) is
   begin
      It := Iterator(It.Next);
   end Next_Item;


   procedure Finalize(L : in out List_Type) is
      procedure Free_Node is new Ada.Unchecked_Deallocation(Node, Node_Pointer);

      Current    : Node_Pointer := L.Head;
      Destroy_Me : Node_Pointer;

   begin
      while Current /= null loop
         Destroy_Me := Current;
         Current    := Current.Next;
         Free_Node(Destroy_Me);
      end loop;
   end Finalize;

end Single_List;
