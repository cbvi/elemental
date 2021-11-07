with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;

package body Elemental.Page is
   package IO renames Ada.Text_IO;
   package UI renames Ada.Text_IO.Unbounded_IO;
   package EI renames Ada.IO_Exceptions;

   Header : constant String := ""      &
      "<!doctype html>"                &
      "<html><head>"                   &
      "<meta charset=""UTF-8"" />";

   function To_Html (Page : Elemental.Page.Page) return UB.Unbounded_String
   is
      Buffer : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      UB.Append (Buffer, Header);
      UB.Append (Buffer, "<title>");
      UB.Append (Buffer, Page.Title);
      UB.Append (Buffer, "</title>");
      UB.Append (Buffer, "</head><body>");

      for I of Page.Fragments loop
         UB.Append (Buffer, Fragment_To_String (I));
      end loop;

      UB.Append (Buffer, "</body></html>");
      return Buffer;
   end To_Html;

   function Fragment_To_String (Frag : Fragment) return UB.Unbounded_String
   is
      File     : IO.File_Type;
      Line     : UB.Unbounded_String := UB.Null_Unbounded_String;
      Result   : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      if Frag.What = Text then
         return Frag.Content;
      end if;
      IO.Open (File, IO.In_File, UB.To_String (Frag.Source), "WCEM=8");
      loop
         begin
            Line := UI.Get_Line (File);
            UB.Append (Result, Line);
            UB.Append (Result, Ada.Characters.Latin_1.LF);
         exception
            when EI.End_Error => exit;
         end;
      end loop;
      IO.Close (File);
      return Result;
   end Fragment_To_String;
end Elemental.Page;
