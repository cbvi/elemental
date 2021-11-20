with Sax.Symbols;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;

package body Elemental.PageReader.Utils is

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

   function Get_Date
     (Handler    : in out Elemental.PageReader.Reader;
      Atts       : Sax.Readers.Sax_Attribute_List)
       return Elemental.Page.Some_Date
   is
      Time   : constant String := " 12:00:00"; --  time is unused
      String : UB.Unbounded_String;
      Found  : Boolean := False;
      Date   : Elemental.Page.Some_Date (True);
      None   : Elemental.Page.Some_Date;
   begin
      Get_Attribute_By_Name (Handler, Atts, "date", String, Found);

      if Found then
         Date.Value := Ada.Calendar.Formatting.Value
           (UB.To_String (String) & Time,
            Ada.Calendar.Time_Zones.UTC_Time_Offset);
         return Date;
      else
         return None;
      end if;
   end Get_Date;

   function Get_Sub
     (Handler    : in out Elemental.PageReader.Reader;
      Atts       : Sax.Readers.Sax_Attribute_List)
      return UB.Unbounded_String
   is
      Found  : Boolean := False;
      Sub    : UB.Unbounded_String;
   begin
      Get_Attribute_By_Name (Handler, Atts, "sub", Sub, Found);

      if Found then
         return Sub;
      else
         return UB.Null_Unbounded_String;
      end if;
   end Get_Sub;

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
end Elemental.PageReader.Utils;
