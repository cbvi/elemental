with Sax.Symbols; use Sax.Symbols;
with Elemental.Data;
with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

package body Elemental.PageReader is
   package L1 renames Ada.Characters.Latin_1;

   overriding
   procedure Start_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      case Handler.In_Page is
         when True =>
            if Local_Name = "Content" then
               Handler.In_Content := True;
            elsif Local_Name = "Text" then
               if not Handler.In_Content then
                  raise Page_Error with "Text must be in <Content>";
               end if;
               Handler.In_Text := True;
            elsif Local_Name = "Fragment" then
               if not Handler.In_Content then
                  raise Page_Error with "Fragment must be in <Content>";
               end if;
               Elemental.Data.Process_Fragment (Handler, Atts);
            elsif Local_Name = "Page" then
               raise Page_Error with "Must have only one <Page> element";
            end if;
         when False =>
            if Local_Name = "Page" then
               Handler.Page.Title := Elemental.Data.Get_Title (Handler, Atts);
               Handler.In_Page := True;
            else
               raise Page_Error with "Root element must be <Page>";
            end if;
      end case;
   end Start_Element;

   overriding
   procedure Characters
      (Handler    : in out Reader;
       Ch         : Unicode.CES.Byte_Sequence)
   is
      Fragment    : Elemental.Page.Fragment (Elemental.Page.Raw);
      Set         : constant Ada.Strings.Maps.Character_Set :=
                     Ada.Strings.Maps.To_Set (L1.Space & L1.VT & L1.LF & L1.CR);
   begin
      if not Handler.In_Text then
         raise Page_Error with "Characters outside of <Text>";
      end if;

      Fragment.Content := UB.Trim (UB.To_Unbounded_String (Ch), Set, Set);

      Handler.Page.Fragments.Append (Fragment);
   end Characters;

   overriding
   procedure End_Element
      (Handler    : in out Reader;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol)
   is
   begin
      if Local_Name = "Text" then
         Handler.In_Text := False;
      elsif Local_Name = "Content" then
         Handler.In_Content := False;
      elsif Local_Name = "Page" then
         Handler.In_Page := False;
      end if;
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
