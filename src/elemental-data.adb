with Sax.Symbols;
with Elemental.Page;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Elemental.Data is

   function Get_Title
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
       return UB.Unbounded_String
   is
      Index : Integer;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "title");

      if Index /= -1 then
         return UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
      else
         raise Elemental.PageReader.Page_Error with "Page must have title";
      end if;
   end Get_Title;

   procedure Process_Fragment
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
      Index    : Integer;
      Name     : UB.Unbounded_String;
      Fragment : Elemental.Page.Fragment (Elemental.Page.External);
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "source");
      if Index /= -1 then
         Name := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
         Fragment.Source := Name;
      else
         raise Elemental.PageReader.Page_Error with "Fragment must have source";
      end if;

      Index := Sax.Readers.Get_Index (Handler, Atts, "", "type");
      if Index /= -1 then
         Name := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);

         if Name = "code" then
            Fragment.What := Elemental.Page.Code;
         elsif Name = "html" then
            Fragment.What := Elemental.Page.Html;
         else
            Fragment.What := Elemental.Page.Text;
         end if;
      else
         Fragment.What := Elemental.Page.Text;
      end if;

      Handler.Page.Fragments.Append (Fragment);
   end Process_Fragment;
end Elemental.Data;
