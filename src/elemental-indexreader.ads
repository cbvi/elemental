with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;
with Elemental.Index;

package Elemental.IndexReader is

   type Reader is new Sax.Readers.Sax_Reader with record
      Pages    : Elemental.Index.List;
      In_Pages : Boolean := False;
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

end Elemental.IndexReader;
