package body Elemental.SettingsReader is

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      null;
   end Start_Element;

end Elemental.SettingsReader;
