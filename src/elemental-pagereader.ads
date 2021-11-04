with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;
with Ada.Strings.Unbounded;
with Elemental.Page;

package Elemental.PageReader is

   package UB renames Ada.Strings.Unbounded;

   type Reader is new Sax.Readers.Sax_Reader with record
      In_Content  : Boolean;
      Page        : Elemental.Page.Page;
   end record;

   procedure Start_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List);

   procedure Characters
      (Handler    : in out Reader;
       Ch         : Unicode.CES.Byte_Sequence);

   procedure End_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol);

end Elemental.PageReader;
