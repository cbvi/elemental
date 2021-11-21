with Elemental.Index;
with Elemental.Settings;
with Elemental.Page;
with Ada.Strings;
with Ada.Strings.Unbounded;
--  with Ada.Text_IO;

procedure Main is

   package UB renames Ada.Strings.Unbounded;

   function TS (S : UB.Unbounded_String)
                return String renames UB.To_String;

   task type Transmute_Task is
      entry Start (P : UB.Unbounded_String; S : Elemental.Settings.Settings);
   end Transmute_Task;

   task body Transmute_Task is
      Page : Elemental.Page.Page;
   begin
      accept Start (P : UB.Unbounded_String; S : Elemental.Settings.Settings) do
         Page := Elemental.Page.Get_Page (TS (P));
         Elemental.Page.Transmute_Page (Page, S);
      end Start;
   end Transmute_Task;

   Settings    : Elemental.Settings.Settings;
   Index       : Elemental.Index.List;
begin
   Elemental.Settings.Process_Settings ("settings.xml", Settings);

   Elemental.Index.Get_Pages (TS (Settings.Pages), Index);

   declare
      Task_Array : array (1 .. Natural (Index.Length)) of Transmute_Task;
      Task_Id    : Natural := 1;
   begin
      for S of Index loop
         Task_Array (Task_Id).Start (S, Settings);
         Task_Id := Task_Id + 1;

         --  Page := Elemental.Page.Get_Page (TS (S));
         --
         --  Ada.Text_IO.Put_Line (TS (Page.Title));
         --
         --  Elemental.Page.Transmute_Page (Page, Settings);
      end loop;
   end;
end Main;
