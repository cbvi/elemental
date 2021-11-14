with Elemental.Page;
with Elemental.PageReader;
with Elemental.SettingsReader;
with Elemental.Settings;
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
   procedure Dies_Html (Template : String; Message : String);
   procedure Do_Settings (Xml : String; Settings : Elemental.Settings.Settings);
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

   procedure Dies_Html (Template : String; Message : String)
   is
      Reader   : Elemental.PageReader.Reader;
      File     : Input_Sources.File.File_Input;
      Output   : UB.Unbounded_String := UB.Null_Unbounded_String;
      Died     : Boolean := False;
   begin
      Start_Test;

      Input_Sources.File.Open ("test/badhtml/tests.xml", File);
      Elemental.PageReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      begin
         Output := Elemental.Page.To_Html (Reader.Page, Template);
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
      pragma Assert (Output = UB.Null_Unbounded_String);

      End_Test;
   end Dies_Html;

   procedure Do_Settings (Xml : String; Settings : Elemental.Settings.Settings)
   is
      Reader   : Elemental.SettingsReader.Reader;
      File     : Input_Sources.File.File_Input;
   begin
      Start_Test;

      Input_Sources.File.Open (Xml, File);
      Elemental.SettingsReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      pragma Assert (Settings.Template = Reader.Settings.Template);
      pragma Assert (Settings.Author = Reader.Settings.Author);
      pragma Assert (Settings.Pages = Reader.Settings.Pages);

      End_Test;
   end Do_Settings;

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
   Set1 : Elemental.Settings.Settings;
begin
   Do_Test ("test/outset/basic.xml", "test/outset/expects/basic.html", T1);
   Do_Test ("test/outset/transclude.xml",
            "test/outset/expects/transclude.html", T1);
   Do_Test ("test/outset/mixed.xml", "test/outset/expects/mixed.html", T1);
   Do_Test ("test/outset/unicode.xml", "test/outset/expects/unicode.html", T1);
   Do_Test ("test/outset/fragment-type.xml",
            "test/outset/expects/fragment-type.html", T1);

   Dies_Html ("test/badhtml/badtemplate.html",
              "opening {{@@ has no closing @@}}");
   Dies_Html ("test/badhtml/badtag.html",
              "opening {{@@ has no closing @@}}");
   Dies_Html ("test/badhtml/noexist.html",
              "nonexistent tag: {{@@ NOTREAL @@}}");
   Dies_Html ("test/badhtml/mismatch.html",
              "runaway tag: TITLE @@}</title></head><body>{{@@ CONTENT");

   Dies_Ok ("test/outset/outside.xml", "Characters outside of <Text>");
   Dies_Ok ("test/outset/stray-text.xml", "Text must be in <Content>");
   Dies_Ok ("test/outset/stray-fragment.xml", "Fragment must be in <Content>");
   Dies_Ok ("test/outset/stray-page.xml", "Must have only one <Page> element");
   Dies_Ok ("test/outset/not-page.xml", "Root element must be <Page>");
   Dies_Ok ("test/outset/notitle.xml", "Page must have title");
   Dies_Ok ("test/outset/nosource.xml", "Fragment must have source");

   Do_Test ("test/inset/date.xml",
            "test/inset/expects/date.html",
            "test/inset/date.html");

   Do_Test ("test/inset/date.xml",
            "test/inset/expects/beside.html",
            "test/inset/beside.html");

   Set1.Template := UB.To_Unbounded_String ("test/setset/template.html");
   Set1.Author := UB.To_Unbounded_String ("Justin Example");
   Set1.Pages := UB.To_Unbounded_String ("test/setset/pages.xml");
   Do_Settings ("test/setset/settings.xml", Set1);

   if Finished = Started then
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test;
