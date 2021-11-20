with Ada.Directories;
with Elemental.Page;
with Elemental.Settings;
with Elemental.Index;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Direct_IO;

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
   procedure Do_Index
     (Xml : String;
      Pages : Elemental.Index.List);
   procedure Do_Transmute
     (Xml     : String;
      Output  : String;
      Expects : String;
      List    : Elemental.Index.List);
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
      Output   : UB.Unbounded_String;
      Expects  : UB.Unbounded_String;
      Page     : Elemental.Page.Page;
   begin
      Start_Test;

      Page := Elemental.Page.Get_Page (Xml);

      Output := Elemental.Page.To_Html (Page, Template);
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
      Output   : UB.Unbounded_String := UB.Null_Unbounded_String;
      Died     : Boolean := False;
      Page     : Elemental.Page.Page;
   begin
      Start_Test;

      Page := Elemental.Page.Get_Page ("test/badhtml/tests.xml");

      begin
         Output := Elemental.Page.To_Html (Page, Template);
      exception
         when E : others =>
            if EX.Exception_Message (E) = Message then
               Died := True;
            else
               EX.Reraise_Occurrence (E);
            end if;
      end;

      pragma Assert (Died);
      pragma Assert (Output = UB.Null_Unbounded_String);

      End_Test;
   end Dies_Html;

   procedure Do_Settings (Xml : String; Settings : Elemental.Settings.Settings)
   is
      Settings1 : Elemental.Settings.Settings;
   begin
      Start_Test;

      Elemental.Settings.Process_Settings (Xml, Settings1);

      pragma Assert (Settings.Template = Settings1.Template);
      pragma Assert (Settings.Author = Settings1.Author);
      pragma Assert (Settings.Pages = Settings1.Pages);

      End_Test;
   end Do_Settings;

   procedure Do_Index
     (Xml   : String;
      Pages : Elemental.Index.List)
   is
      Pages2   : Elemental.Index.List;
      use Elemental.Index.Page_Vector;
   begin
      Start_Test;

      Elemental.Index.Get_Pages (Xml, Pages2);

      pragma Assert (Pages = Pages2);

      End_Test;
   end Do_Index;

   procedure Dies_Ok (Xml : String; Message : String)
   is
      Died     : Boolean := False;
      Page     : Elemental.Page.Page;
   begin
      Start_Test;
      begin
         Page := Elemental.Page.Get_Page (Xml);
      exception
         when E : others =>
            if EX.Exception_Message (E) = Message then
               Died := True;
            else
               EX.Reraise_Occurrence (E);
            end if;
      end;
      pragma Assert (Died);
      End_Test;
   end Dies_Ok;

   procedure Do_Transmute
     (Xml     : String;
      Output  : String;
      Expects : String;
      List    : Elemental.Index.List)
   is
      Settings : Elemental.Settings.Settings;
      Index    : Elemental.Index.List;
      Page     : Elemental.Page.Page;
   begin
      Start_Test;
      Elemental.Settings.Process_Settings (Xml, Settings);
      Ada.Directories.Delete_Tree (UB.To_String (Settings.Output));
      Elemental.Index.Get_Pages (UB.To_String (Settings.Pages), Index);

      for S of Index loop
         Page := Elemental.Page.Get_Page (UB.To_String (S));
         Elemental.Page.Transmute_Page (Page, Settings);
      end loop;

      for S of List loop
         declare
            Out_File : constant String := Output  & "/" & UB.To_String (S);
            Exp_File : constant String := Expects & "/" & UB.To_String (S);
            Size : constant Natural :=
              Natural (Ada.Directories.Size (Out_File));
            subtype File_String is String (1 .. Size);
            package FIO is new Ada.Direct_IO (File_String);

            Content1 : File_String;
            File1    : FIO.File_Type;

            Content2 : File_String;
            File2    : FIO.File_Type;
         begin
            FIO.Open (File1, FIO.In_File, Out_File);
            FIO.Read (File1, Content1);
            FIO.Close (File1);

            FIO.Open (File2, FIO.In_File, Exp_File);
            FIO.Read (File2, Content2);
            FIO.Close (File2);

            pragma Assert (Content1 = Content2);
         end;
      end loop;

      End_Test;
   end Do_Transmute;

   T1 : constant String := "test/outset/template.html";
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

   declare
      Set1 : constant Elemental.Settings.Settings :=
        (Template => UB.To_Unbounded_String ("test/setset/template.html"),
         Author   => UB.To_Unbounded_String ("Justin Example"),
         Pages    => UB.To_Unbounded_String ("test/setset/pages.xml"),
         Output   => UB.To_Unbounded_String ("test/setset/output/"));
   begin
      Do_Settings ("test/setset/settings.xml", Set1);
   end;

   declare
      Pages : Elemental.Index.List;
   begin
      Pages.Append (UB.To_Unbounded_String ("test/setset/page1.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/page2.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/page3.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/page4.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/subdir/page1.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/subdir/page2.xml"));
      Pages.Append (UB.To_Unbounded_String ("test/setset/subdir/page9.xml"));
      Do_Index ("test/setset/pages.xml", Pages);
   end;

   declare
      List : Elemental.Index.List;
   begin
      List.Append (UB.To_Unbounded_String ("page1"));
      List.Append (UB.To_Unbounded_String ("page2"));
      List.Append (UB.To_Unbounded_String ("page3"));
      List.Append (UB.To_Unbounded_String ("page9"));
      List.Append (UB.To_Unbounded_String ("aaa/page1"));
      List.Append (UB.To_Unbounded_String ("aaa/bbb/page2"));
      List.Append (UB.To_Unbounded_String ("ccc/page4"));
      Do_Transmute
        ("test/setset/settings.xml",
         "test/setset/output",
         "test/setset/expects/output",
         List);
   end;

   if Finished = Started then
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test;
