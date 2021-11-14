with Elemental.SettingsReader;
with Elemental.IndexReader;
with Elemental.PageReader;
with Elemental.Page;
with Input_Sources.File;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Main is
   package UB renames Ada.Strings.Unbounded;
   function TS (S : UB.Unbounded_String)
                return String renames UB.To_String;

   Settings_File   : Input_Sources.File.File_Input;
   Settings_Reader : Elemental.SettingsReader.Reader;

   Index_File   : Input_Sources.File.File_Input;
   Index_Reader : Elemental.IndexReader.Reader;

   Page_File   : Input_Sources.File.File_Input;
   Page_Reader : Elemental.PageReader.Reader;
begin
   Input_Sources.File.Open ("test/setset/settings.xml", Settings_File);
   Elemental.SettingsReader.Parse (Settings_Reader, Settings_File);
   Input_Sources.File.Close (Settings_File);

   Input_Sources.File.Open (TS (Settings_Reader.Settings.Pages), Index_File);
   Elemental.IndexReader.Parse (Index_Reader, Index_File);
   Input_Sources.File.Close (Index_File);

   for S of Index_Reader.Pages loop
      Input_Sources.File.Open (TS (S), Page_File);
      Elemental.PageReader.Parse (Page_Reader, Page_File);
      Input_Sources.File.Close (Page_File);

      Ada.Text_IO.Put_Line (TS (Page_Reader.Page.Title));

      declare
         New_File : Ada.Text_IO.File_Type;
         New_Html : UB.Unbounded_String;
      begin
         Ada.Text_IO.Create
           (New_File,
            Ada.Text_IO.Out_File,
            TS (Settings_Reader.Settings.Output) & TS (S) & ".html",
            "WCEM=8");
         New_Html := Elemental.Page.To_Html
           (Page_Reader.Page, TS (Settings_Reader.Settings.Template));
         Ada.Text_IO.Put (New_File, TS (New_Html));
         Ada.Text_IO.Close (New_File);
      end;

      Page_Reader.Page.Fragments.Clear;
   end loop;
end Main;
