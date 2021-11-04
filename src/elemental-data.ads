with Sax.Readers;
with Ada.Strings.Unbounded;
with Elemental.PageReader;

package Elemental.Data is
   package UB renames Ada.Strings.Unbounded;

   function Get_Title
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
       return UB.Unbounded_String;

   procedure Process_Fragment
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List);
end Elemental.Data;
