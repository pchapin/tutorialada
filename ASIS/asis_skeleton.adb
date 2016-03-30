with Ada.Wide_Text_IO;        use Ada.Wide_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

--  ASIS-specific context clauses:
with Asis;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Exceptions;
with Asis.Errors;

procedure ASIS_Skeleton is
   My_Context : Asis.Context;
   --  ASIS Context is an abstraction of an Ada compilation environment,
   --  it defines a set of ASIS Compilation Units available through ASIS
   --  queries

begin
   --  first, by initializing an ASIS implementation, we make it ready
   --  for work
   Asis.Implementation.Initialize ("-ws");
   --  The "-ws" parameter of the Initialize procedure means "turn off
   --  all the ASIS warnings"

   --  then we define our Context by making an association with the
   --  "physical" environment:
   Asis.Ada_Environments.Associate
     (My_Context, "My Asis Context", "-CA");
   --  "-CA" as a Context parameter means "consider all the tree files
   --  in the current directory" See ASIS-for-GNAT Reference Manual for
   --  the description of the parameters of the Associate query, see
   --  also chapter "ASIS Context" for the description of different
   --  kinds of ASIS Context in case of ASIS-for-GNAT

   --  by opening a Context we make it ready for processing by ASIS
   --  queries
   Asis.Ada_Environments.Open (My_Context);

Processing_Units: declare
   Next_Unit : Asis.Compilation_Unit;
   --  ASIS Compilation_Unit is the abstraction to represent Ada
   --  compilation units as described in RM 95

   All_Units : Asis.Compilation_Unit_List :=
     --  ASIS lists are one-dimensional unconstrained arrays. Therefore,
     --  when declaring an object of an ASIS list type, we have to
     --  provide either a constraint or explicit initialization
     --  expression:

     Asis.Compilation_Units.Compilation_Units (My_Context);
   --  The Compilation_Units query returns a list of all the units
   --  contained in an ASIS Context
begin
   Put_Line
     ("A Context contains the following compilation units:");
   New_Line;
   for I in All_Units'Range loop
      Next_Unit := All_Units (I);
      Put ("   ");

      --  to get a unit name, we just need a Unit_Full_Name query. ASIS
      --  uses Wide_String as a string type, that is why we are using
      --  Ada.Wide_Text_IO

      Put (Asis.Compilation_Units.Unit_Full_Name (Next_Unit));

      --  to get more info about a unit, we ask about unit class and
      --  about unit origin

      case Asis.Compilation_Units.Unit_Kind (Next_Unit) is
         when Asis.A_Library_Unit_Body =>
            Put (" (body)");
         when Asis.A_Subunit =>
            Put (" (subunit)");
         when others =>
            Put (" (spec)");
      end case;

      case Asis.Compilation_Units.Unit_Origin (Next_Unit) is
         when Asis.An_Application_Unit =>
            Put_Line (" - user-defined unit");
         when Asis.An_Implementation_Unit =>
            Put_Line (" - implementation-specific unit");
         when Asis.A_Predefined_Unit =>
            Put_Line (" - Ada predefined unit");
         when Asis.Not_An_Origin =>
            Put_Line
              (" - unit does not actually exist in a Context");
      end case;

   end loop;
end Processing_Units;

--  Cleaning up: we have to close out the Context, break its association
--  with the external environment and finalize our ASIS implementation
--  to release all the resources used:
Asis.Ada_Environments.Close (My_Context);
Asis.Ada_Environments.Dissociate (My_Context);
Asis.Implementation.Finalize;

exception
   when Asis.Exceptions.ASIS_Inappropriate_Context |
     Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
     Asis.Exceptions.ASIS_Failed =>

      --  we check not for all the ASIS-defined exceptions, but only
      --  those of them which can actually be raised in our ASIS
      --  application.
      --
      --  If an ASIS exception is raised, we output the ASIS error
      --  status and the ASIS diagnosis string:

      Put_Line ("ASIS exception is raised:");
      Put_Line ("ASIS diagnosis is:");
      Put_Line (Asis.Implementation.Diagnosis);
      Put      ("ASIS error status is: ");
      Put_Line
        (Asis.Errors.Error_Kinds'Wide_Image
         (Asis.Implementation.Status));
end ASIS_Skeleton;
