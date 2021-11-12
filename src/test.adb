with Elemental.Page;
with Elemental.PageReader;
with Input_Sources.File;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Command_Line;

procedure Test is
   package IO renames Ada.Text_IO;
   package UB renames Ada.Strings.Unbounded;
   package UI renames Ada.Text_IO.Unbounded_IO;
   package EI renames Ada.IO_Exceptions;
   package EX renames Ada.Exceptions;

   Started : Integer := 0;
   Finished : Integer := 0;

   procedure Do_Test (Xml : String; Html : String; Template : String);
   function Get_Expected (Name : String) return UB.Unbounded_String;
   procedure Dies_Ok (Xml : String; Message : String);
   procedure Start_Test;
   procedure End_Test;

   procedure Start_Test is
   begin
      Started := Started + 1;
      IO.Put (Integer'Image (Started) & " ... ");
   end Start_Test;

   procedure End_Test is
   begin
      Finished := Finished + 1;
      IO.Put_Line ("ok");
   end End_Test;

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

   procedure Do_Test (Xml : String; Html : String; Template : String)
   is
      Reader   : Elemental.PageReader.Reader;
      File     : Input_Sources.File.File_Input;
      Output   : UB.Unbounded_String;
      Expects  : UB.Unbounded_String;
   begin
      Start_Test;

      Input_Sources.File.Open (Xml, File);
      Elemental.PageReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      Output := Elemental.Page.To_Html (Reader.Page, Template);
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

      End_Test;
   end Do_Test;

   procedure Dies_Ok (Xml : String; Message : String)
   is
      Reader   : Elemental.PageReader.Reader;
      File     : Input_Sources.File.File_Input;
      Died     : Boolean := False;
   begin
      Start_Test;
      Input_Sources.File.Open (Xml, File);

      begin
         Elemental.PageReader.Parse (Reader, File);
      exception
         when E : others =>
            if EX.Exception_Message (E) = Message then
               Died := True;
            else
               EX.Reraise_Occurrence (E);
            end if;
      end;
      Input_Sources.File.Close (File);

      pragma Assert (Died);
      End_Test;
   end Dies_Ok;

   T1 : constant String := "test/outset/template.html";
   TB : constant String := "test/badhtml/badtemplate.html";
begin
   Do_Test ("test/outset/basic.xml", "test/outset/expects/basic.html", T1);
   Do_Test ("test/outset/transclude.xml",
            "test/outset/expects/transclude.html", T1);
   Do_Test ("test/outset/mixed.xml", "test/outset/expects/mixed.html", T1);
   Do_Test ("test/outset/fragment-type.xml",
            "test/outset/expects/fragment-type.html", T1);

   Do_Test ("test/outset/basic.xml",
            "test/badhtml/expects/badtemplate.html", TB);

   Dies_Ok ("test/outset/outside.xml", "Characters outside of <Text>");
   Dies_Ok ("test/outset/stray-text.xml", "Text must be in <Content>");
   Dies_Ok ("test/outset/stray-fragment.xml", "Fragment must be in <Content>");
   Dies_Ok ("test/outset/stray-page.xml", "Must have only one <Page> element");
   Dies_Ok ("test/outset/not-page.xml", "Root element must be <Page>");
   Dies_Ok ("test/outset/notitle.xml", "Page must have title");
   Dies_Ok ("test/outset/nosource.xml", "Fragment must have source");

   if Finished = Started then
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test;
