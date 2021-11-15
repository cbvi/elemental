with Sax.Symbols; use Sax.Symbols;
with Ada.Strings.Unbounded;

package body Elemental.IndexReader is

   package UB renames Ada.Strings.Unbounded;

   procedure Process_Page
     (Handler : in out Elemental.IndexReader.Reader;
      Atts    : Sax.Readers.Sax_Attribute_List);

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      if Local_Name = "Pages" then
         Handler.In_Pages := True;
      elsif Local_Name = "Page" then
         if Handler.In_Pages then
            Process_Page (Handler, Atts);
         else
            raise Elemental.Index.Index_Error with "<Page> outside of <Pages>";
         end if;
      else
         raise Elemental.Index.Index_Error with "Unexpected tag";
      end if;
   end Start_Element;

   overriding
   procedure Characters
     (Handler    : in out Reader;
      Ch         : Unicode.CES.Byte_Sequence)
   is
   begin
      raise Elemental.Index.Index_Error with "Characters not expected in index";
   end Characters;

   overriding
   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol)
   is
   begin
      if Local_Name = "Pages" then
         Handler.In_Pages := False;
      end if;
   end End_Element;

   procedure Process_Page
     (Handler : in out Elemental.IndexReader.Reader;
      Atts    : Sax.Readers.Sax_Attribute_List)
   is
      Index  : Integer;
      Source : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "Source");

      if Index /= -1 then
         Source := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
         Handler.Pages.Append (Source);
      else
         raise Elemental.Index.Index_Error with "Index Page must have a Source";
      end if;
   end Process_Page;

end Elemental.IndexReader;
