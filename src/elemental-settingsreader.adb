with Sax.Symbols; use Sax.Symbols;
with Elemental.SettingsReader.Utils;

package body Elemental.SettingsReader is

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

   overriding
   procedure Characters
     (Handler    : in out Reader;
      Ch         : Unicode.CES.Byte_Sequence)
   is
   begin
      if Handler.In_Setting then
         UB.Append (Handler.Current_Value, Ch);
      else
         raise Settings_Error with "Characters outside Setting";
      end if;
   end Characters;

   overriding
   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol)
   is
      use UB;
   begin
      if Local_Name = "Setting" then
         if Handler.Current_Setting = "template" then
            Handler.Settings.Template := Handler.Current_Value;
         elsif Handler.Current_Setting = "pages" then
            Handler.Settings.Pages := Handler.Current_Value;
         elsif Handler.Current_Setting = "author" then
            Handler.Settings.Author := Handler.Current_Value;
         else
            raise Settings_Error with
              "Unknown setting name: " & UB.To_String (Handler.Current_Setting);
         end if;
         Handler.In_Setting := False;
         Handler.Current_Setting := NUB;
         Handler.Current_Value := NUB;
      elsif Local_Name = "Settings" then
         Handler.In_Settings := False;
      end if;
   end End_Element;

end Elemental.SettingsReader;
