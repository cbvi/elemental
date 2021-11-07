with Elemental.Page;
with Elemental.PageReader;
with Input_Sources.File;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;

procedure Test is
   package IO renames Ada.Text_IO;
   package UB renames Ada.Strings.Unbounded;
   package UI renames Ada.Text_IO.Unbounded_IO;
   package EI renames Ada.IO_Exceptions;

   procedure Do_Test (Xml : String; Html : String);
   function Get_Expected (Name : String) return UB.Unbounded_String;

   function Get_Expected (Name : String) return UB.Unbounded_String
   is
      File     : IO.File_Type;
      Line     : UB.Unbounded_String := UB.Null_Unbounded_String;
      Result   : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      IO.Open (File, IO.In_File, Name, "WCEM=8");
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
   end Get_Expected;

   procedure Do_Test (Xml : String; Html : String)
   is
      Reader   : Elemental.PageReader.Reader;
      File     : Input_Sources.File.File_Input;
      Output   : UB.Unbounded_String;
      Expects  : UB.Unbounded_String;
   begin
      Input_Sources.File.Open (Xml, File);
      Elemental.PageReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      Output := Elemental.Page.To_Html (Reader.Page);
      Expects := Get_Expected (Html);

      if Output /= Expects then
         IO.Put_Line ("TEST FAIL: " & Xml);
         IO.Put_Line
            ("GOT: " & Ada.Characters.Latin_1.LF &
            "{{{" & To_String (Output) & "}}}");
         IO.Put_Line
            ("EXPECTED: " & Ada.Characters.Latin_1.LF &
            "{{{" & To_String (Expects) & "}}}");
      end if;

      pragma Assert (Output = Expects);
   end Do_Test;
begin
   Do_Test ("test/basic.xml", "test/expects/basic.html");
   Do_Test ("test/transclude.xml", "test/expects/transclude.html");
end Test;
