with Sax.Readers;
with Ada.Strings.Unbounded;
with Elemental.Page;

package Elemental.PageReader.Utils is
   package UB renames Ada.Strings.Unbounded;

   procedure Get_Attribute_By_Name
     (Handler : in out Elemental.PageReader.Reader;
      Atts    :        Sax.Readers.Sax_Attribute_List;
      Name    :        String;
      Value   : in out UB.Unbounded_String;
      Found   : in out Boolean);

   function Get_Title
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
       return UB.Unbounded_String;

   function Get_Date
     (Handler    : in out Elemental.PageReader.Reader;
      Atts       : Sax.Readers.Sax_Attribute_List)
      return Elemental.Page.Some_Date;

   function Get_Sub
     (Handler    : in out Elemental.PageReader.Reader;
      Atts       : Sax.Readers.Sax_Attribute_List)
      return UB.Unbounded_String;

   procedure Process_Fragment
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List);
end Elemental.PageReader.Utils;
