with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Elemental.Settings;
with Ada.Strings.Unbounded;
with Unicode.CES;

package Elemental.SettingsReader is

   Settings_Error : exception;

   package UB renames Ada.Strings.Unbounded;
   NUB : UB.Unbounded_String renames UB.Null_Unbounded_String;

   type Reader is new Sax.Readers.Sax_Reader with record
      Settings        : Elemental.Settings.Settings;
      In_Settings     : Boolean := False;
      In_Setting      : Boolean := False;
      Current_Setting : UB.Unbounded_String := NUB;
      Current_Value   : UB.Unbounded_String := NUB;
   end record;

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List);

   overriding
   procedure Characters
     (Handler    : in out Reader;
      Ch         : Unicode.CES.Byte_Sequence);

   overriding
   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol);

end Elemental.SettingsReader;
