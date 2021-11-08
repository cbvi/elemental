with Sax.Symbols;
with Elemental.Page;

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
      Fragment : Elemental.Page.Fragment (Elemental.Page.Text);
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "source");
      if Index /= -1 then
         Name := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
         Fragment.Source := Name;
         Handler.Page.Fragments.Append (Fragment);
      else
         raise Elemental.PageReader.Page_Error with "Fragment must have source";
      end if;
   end Process_Fragment;
end Elemental.Data;
