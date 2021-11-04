with Sax.Readers;
with Ada.Strings.Unbounded;

package Elemental.Page is
   package UB renames Ada.Strings.Unbounded;

   type Page is record
      Title    : UB.Unbounded_String;
      Content  : UB.Unbounded_String;
   end record;
end Elemental.Page;
