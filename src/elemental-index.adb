with Elemental.IndexReader;
with Input_Sources.File;

package body Elemental.Index is

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
end Elemental.Index;
