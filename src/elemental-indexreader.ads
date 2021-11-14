with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Ada.Strings.Unbounded;
with Unicode.CES;
with Ada.Containers.Indefinite_Vectors;

package Elemental.IndexReader is

   Index_Error : exception;

   package UB renames Ada.Strings.Unbounded;
   NUB : UB.Unbounded_String renames UB.Null_Unbounded_String;

   package Page_Vector is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
       Element_Type => UB.Unbounded_String,
       "=" => UB."=");

   type Reader is new Sax.Readers.Sax_Reader with record
      Pages    : Page_Vector.Vector;
      In_Pages : Boolean := False;
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
