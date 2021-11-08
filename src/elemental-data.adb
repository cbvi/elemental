with Sax.Symbols;
with Elemental.Page;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Elemental.Data is

   procedure Get_Attribute_By_Name
     (Handler : in out Elemental.PageReader.Reader;
      Atts    :        Sax.Readers.Sax_Attribute_List;
      Name    :        String;
      Value   : in out UB.Unbounded_String;
      Found   : in out Boolean)
   is
      Index : Integer;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", Name);

      if Index /= -1 then
         Value := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
         Found := True;
      else
         Found := False;
      end if;
   end Get_Attribute_By_Name;

   function Get_Title
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
       return UB.Unbounded_String
   is
      Title : UB.Unbounded_String;
      Found : Boolean := False;
   begin
      Get_Attribute_By_Name (Handler, Atts, "title", Title, Found);

      if Found then
         return Title;
      else
         raise Elemental.PageReader.Page_Error with "Page must have title";
      end if;
   end Get_Title;

   procedure Process_Fragment
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
      Source   : UB.Unbounded_String;
      What     : UB.Unbounded_String;
      Found    : Boolean := False;
      Fragment : Elemental.Page.Fragment (Elemental.Page.External);
   begin
      Get_Attribute_By_Name (Handler, Atts, "source", Source, Found);

      if Found then
         Fragment.Source := Source;
      else
         raise Elemental.PageReader.Page_Error with "Fragment must have source";
      end if;

      Found := False;
      Get_Attribute_By_Name (Handler, Atts, "type", What, Found);

      if Found then
         if What = "code" then
            Fragment.What := Elemental.Page.Code;
         elsif What = "html" then
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
