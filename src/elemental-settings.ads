with Ada.Strings.Unbounded;

package Elemental.Settings is

   package UB renames Ada.Strings.Unbounded;
   NUB : UB.Unbounded_String renames UB.Null_Unbounded_String;

   type Settings is record
      Template : UB.Unbounded_String := NUB;
      Author   : UB.Unbounded_String := NUB;
      Pages    : UB.Unbounded_String := NUB;
      Output   : UB.Unbounded_String := NUB;
   end record;

   procedure Process_Settings
     (Settings_Path :        String;
      Settings      : in out Elemental.Settings.Settings);

end Elemental.Settings;
