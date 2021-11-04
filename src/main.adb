with Elemental.PageReader;
with Input_Sources.File;

procedure Main is
   My_Reader : Elemental.PageReader.Reader;
   Input     : Input_Sources.File.File_Input;
begin
   Input_Sources.File.Open("test/basic.xml", Input);

   Elemental.PageReader.Parse (My_Reader, Input);

   Input_Sources.File.Close (Input);

   Input_Sources.File.Open("test/transclude.xml", Input);
   Elemental.PageReader.Parse (My_Reader, Input);
   Input_Sources.File.Close (Input);

   Input_Sources.File.Open("test/mixed.xml", Input);
   Elemental.PageReader.Parse (My_Reader, Input);
   Input_Sources.File.Close (Input);
end Main;
