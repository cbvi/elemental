with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Elemental.Index is

   Index_Error : exception;

   package UB renames Ada.Strings.Unbounded;

   package Page_Vector is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural,
      Element_Type => UB.Unbounded_String,
      "=" => UB."=");

   subtype List is Page_Vector.Vector;

   procedure Get_Pages (Pages_Index : String; Pages_List : in out List);

end Elemental.Index;
