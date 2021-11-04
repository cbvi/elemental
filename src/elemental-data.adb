with Sax.Readers;
with Sax.Symbols;
with Sax.Attributes;
with Elemental.PageReader;
with Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Latin_1;

package body Elemental.Data is
   package WO renames Ada.Text_IO;
   package EI renames Ada.IO_Exceptions;

   function Get_Title
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
       return UB.Unbounded_String
   is
      Index : Integer;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "title");
      return UB.To_Unbounded_String
         (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
   end Get_Title;

   procedure Process_Fragment
      (Handler    : in out Elemental.PageReader.Reader;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
      Index : Integer;
      Name  : UB.Unbounded_String;
      File  : WO.File_Type;
      Line  : String (1..512);
      Last  : Natural;
      Dat   : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "source");
      Name := UB.To_Unbounded_String
         (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
      Ada.Text_IO.Unbounded_IO.Put_Line (Name);
      WO.Open (File, WO.In_File, UB.To_String (Name), "WCEM=8");
      loop
         begin
            WO.Get_Line (File, Line, Last);
            UB.Append (Dat, Line (1 .. Last));
            UB.Append (Dat, Ada.Characters.Latin_1.LF);
         exception
            when EI.End_Error => exit;
         end;
      end loop;
      UB.Append (Handler.Page.Content, Dat);
      Ada.Text_IO.Unbounded_IO.Put_Line (Handler.Page.Content);
   end Process_Fragment;
end Elemental.Data;
