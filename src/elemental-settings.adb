with Elemental.SettingsReader;
with Input_Sources.File;

package body Elemental.Settings is

   procedure Process_Settings
     (Settings_Path :        String;
      Settings      : in out Elemental.Settings.Settings)
   is
      File   : Input_Sources.File.File_Input;
      Reader : Elemental.SettingsReader.Reader;
   begin
      Input_Sources.File.Open (Settings_Path, File);
      Elemental.SettingsReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      Settings := Reader.Settings;
   end Process_Settings;

end Elemental.Settings;
