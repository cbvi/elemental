with Ada.Strings.Unbounded;

package Elemental.Settings is

   package UB renames Ada.Strings.Unbounded;

   type Settings is record
      Template : UB.Unbounded_String;
      Author   : UB.Unbounded_String;
      Pages    : UB.Unbounded_String;
   end record;

   procedure Dummy;

end Elemental.Settings;
