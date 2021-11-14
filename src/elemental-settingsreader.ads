with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Elemental.Settings;

package Elemental.SettingsReader is

   type Reader is new Sax.Readers.Sax_Reader with record
      Settings    : Elemental.Settings.Settings;
   end record;

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List);

end Elemental.SettingsReader;
