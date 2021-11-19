with Elemental.IndexReader;
with Input_Sources.File;
with Ada.Directories;

package body Elemental.Index is

   package AD renames Ada.Directories;

   procedure Get_Pages (Pages_Index : String; Pages_List : in out List)
   is
      File   : Input_Sources.File.File_Input;
      Reader : Elemental.IndexReader.Reader;
   begin
      Input_Sources.File.Open (Pages_Index, File);
      Elemental.IndexReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      Pages_List := Reader.Pages;
   end Get_Pages;

   function Get_Target_Name (Page_Path : String) return String
   is
   begin
      return AD.Base_Name (Page_Path) & ".html";
   end Get_Target_Name;

end Elemental.Index;
