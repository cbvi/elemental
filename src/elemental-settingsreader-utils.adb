package body Elemental.SettingsReader.Utils is

   procedure Process_Setting
     (Handler    : in out Elemental.SettingsReader.Reader;
      Atts       : Sax.Readers.Sax_Attribute_List)
   is
      Index : Integer;
   begin
      Index := Sax.Readers.Get_Index (Handler, Atts, "", "name");

      if Index /= -1 then
         Handler.Current_Setting := UB.To_Unbounded_String
           (Sax.Symbols.Get (Sax.Readers.Get_Value (Atts, Index)).all);
      end if;
   end Process_Setting;

end Elemental.SettingsReader.Utils;
