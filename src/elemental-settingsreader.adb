with Sax.Symbols; use Sax.Symbols;
with Elemental.SettingsReader.Utils;

package body Elemental.SettingsReader is

   Settings_Error : exception;

   overriding
   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List)
   is
   begin
      if Handler.In_Settings then
         if Local_Name = "Setting" then
            Elemental.SettingsReader.Utils.Process_Setting (Handler, Atts);
            Handler.In_Setting := True;
         else
            raise Settings_Error with "Unknown element in Settings";
         end if;
      else
         if Local_Name = "Settings" then
            Handler.In_Settings := True;
         else
            raise Settings_Error with "Root element must be <Settings>";
         end if;
      end if;
   end Start_Element;

end Elemental.SettingsReader;
