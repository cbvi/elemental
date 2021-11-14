with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Ada.Strings.Unbounded;
with Unicode.CES;

package Elemental.IndexReader is

   Index_Error : exception;

   package UB renames Ada.Strings.Unbounded;
   NUB : UB.Unbounded_String renames UB.Null_Unbounded_String;

   type Reader is new Sax.Readers.Sax_Reader with record
      Dummy : Boolean;
   end record;

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

end Elemental.IndexReader;
