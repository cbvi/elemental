with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;
with Ada.Strings.Unbounded;
with Elemental.Page;

package Elemental.PageReader is

   package UB renames Ada.Strings.Unbounded;

   Page_Error : exception;
   --  Raised to indicate a problem with a page file.

   type Reader is new Sax.Readers.Sax_Reader with record
      In_Page     : Boolean := False;
      In_Content  : Boolean := False;
      In_Text     : Boolean := False;
      Page        : Elemental.Page.Page;
   end record;

private
   overriding
   procedure Start_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List);

   overriding
   procedure Characters
      (Handler    : in out Reader;
       Ch         : Unicode.CES.Byte_Sequence);

   overriding
   procedure End_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol);
end Elemental.PageReader;
