with Sax.Symbols; use Sax.Symbols;
with Sax.Attributes;
with Ada.Text_IO;
with Elemental.Data;
with Ada.Text_IO.Unbounded_IO;

package body Elemental.PageReader is
   overriding
   procedure Start_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      if Local_Name = "Page" then
         Handler.Page.Title := Elemental.Data.Get_Title (Handler, Atts);
      elsif Local_Name = "Content" then
         Handler.In_Content := True;
      elsif Local_Name = "Fragment" then
         Elemental.Data.Process_Fragment (Handler, Atts);
      end if;
   end Start_Element;

   overriding
   procedure Characters
      (Handler    : in out Reader;
       Ch         : Unicode.CES.Byte_Sequence)
   is
      Fragment    : Elemental.Page.Fragment (Elemental.Page.Raw);
   begin
      Fragment.Content := UB.To_Unbounded_String (Ch);

      Handler.Page.Fragments.Append (Fragment);
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

   overriding
   procedure End_Document
      (Handler    : in out Reader)
   is
      --  Html : UB.Unbounded_String;
   begin
      --  Html := Elemental.Page.To_Html (Handler.Page);
      null;
   end End_Document;

begin
      null;
end Elemental.PageReader;
