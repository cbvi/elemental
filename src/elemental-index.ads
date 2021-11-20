with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Elemental.Index is

   Index_Error : exception;
   --  Raised to indicate a problem with an Index file.

   package UB renames Ada.Strings.Unbounded;

   package Page_Vector is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural,
      Element_Type => UB.Unbounded_String,
      "=" => UB."=");
   --  List of strings representing paths to page files.

   subtype List is Page_Vector.Vector;

   procedure Get_Pages (Pages_Index : String; Pages_List : in out List);
   --  Populates the list of page files from a path to an index file.
end Elemental.Index;
