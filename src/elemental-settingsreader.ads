with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Elemental.Settings;
with Ada.Strings.Unbounded;

package Elemental.SettingsReader is

   package UB renames Ada.Strings.Unbounded;

   type Reader is new Sax.Readers.Sax_Reader with record
      Settings        : Elemental.Settings.Settings;
      In_Settings     : Boolean := False;
      In_Setting      : Boolean := False;
      Current_Setting : UB.Unbounded_String := UB.Null_Unbounded_String;
   end record;

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List);

end Elemental.SettingsReader;
