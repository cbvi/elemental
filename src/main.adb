with Elemental.Index;
with Elemental.Settings;
with Elemental.Page;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Main is
   package UB renames Ada.Strings.Unbounded;
   function TS (S : UB.Unbounded_String)
                return String renames UB.To_String;

   Settings    : Elemental.Settings.Settings;
   Index       : Elemental.Index.List;
   Page        : Elemental.Page.Page;
begin
   Elemental.Settings.Process_Settings ("test/setset/settings.xml", Settings);

   Elemental.Index.Get_Pages (TS (Settings.Pages), Index);

   for S of Index loop
      Page := Elemental.Page.Get_Page (TS (S));

      Ada.Text_IO.Put_Line (TS (Page.Title));

      Elemental.Page.Transmute_Page (Page, Settings);
   end loop;
end Main;
