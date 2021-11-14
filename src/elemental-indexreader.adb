with Sax.Symbols; use Sax.Symbols;

package body Elemental.IndexReader is

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      null;
   end Start_Element;

   overriding
   procedure Characters
     (Handler    : in out Reader;
      Ch         : Unicode.CES.Byte_Sequence)
   is
   begin
      null;
   end Characters;

   overriding
   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol)
   is
   begin
      null;
   end End_Element;

end Elemental.IndexReader;
