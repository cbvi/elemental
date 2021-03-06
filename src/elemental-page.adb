with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Directories;
with Elemental.PageReader;
with Input_Sources.File;

package body Elemental.Page is
   package IO renames Ada.Text_IO;
   package UI renames Ada.Text_IO.Unbounded_IO;
   package EI renames Ada.IO_Exceptions;
   package SF renames Ada.Strings.Fixed;
   package AD renames Ada.Directories;

   Prefix : constant String := "{{@@";
   Suffix : constant String := "@@}}";

   function Date_To_String (Date : Ada.Calendar.Time) return String;

   procedure Transmute_Page
     (Page : Elemental.Page.Page;
      Settings : Elemental.Settings.Settings)
   is
      File  : IO.File_Type;
      Path  : constant String := Elemental.Page.Get_Target (Page, Settings);
      Html  : UB.Unbounded_String;
   begin
      AD.Create_Path (Get_Target_Directory (Page, Settings));
      IO.Create (File, IO.Out_File, Path, "WCEM=8");
      Html := Elemental.Page.To_Html (Page, UB.To_String (Settings.Template));
      Ada.Text_IO.Put (File, UB.To_String (Html));
      Ada.Text_IO.Close (File);
   end Transmute_Page;

   function Get_Page (Page_Path : String) return Elemental.Page.Page
   is
      File   : Input_Sources.File.File_Input;
      Reader : Elemental.PageReader.Reader;
   begin
      Input_Sources.File.Open (Page_Path, File);
      Elemental.PageReader.Parse (Reader, File);
      Input_Sources.File.Close (File);

      Reader.Page.Path := UB.To_Unbounded_String (Page_Path);

      return Reader.Page;
   end Get_Page;

   function Read_Template (Template_File : String) return UB.Unbounded_String
   is
      Buffer : UB.Unbounded_String := UB.Null_Unbounded_String;
      File   : IO.File_Type;
      Line   : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      IO.Open (File, IO.In_File, Template_File, "WCEM=8");

      loop
         begin
            Line := UI.Get_Line (File);
            UB.Append (Buffer, Line);
            UB.Append (Buffer, Ada.Characters.Latin_1.LF);
         exception
            when EI.End_Error =>
               exit;
         end;
      end loop;
      IO.Close (File);
      return Buffer;
   end Read_Template;

   function Extract_Tag
     (Buffer : UB.Unbounded_String;
      Start  : Positive;
      Stop   : Positive)
      return String
   is
      Result : constant String :=
        SF.Trim (UB.Slice (Buffer, Start, Stop), Ada.Strings.Both);
   begin
      return Result;
   end Extract_Tag;

   procedure Replace_Tags
     (Buffer : in out UB.Unbounded_String;
      Map    :        Page_Hash.Map)
   is
      Start : Natural;
      Stop  : Natural;
      From  : Positive := 1;
   begin
      loop
         exit when From >= UB.Length (Buffer);
         Start := UB.Index
           (Source => Buffer,
            Pattern => Prefix,
            From => From);
         exit when Start = 0;

         Stop := UB.Index
           (Source => Buffer,
            Pattern => Suffix,
            From => Start + Prefix'Length);
         if Stop = 0 then
            raise Template_Error with "opening {{@@ has no closing @@}}";
         end if;

         declare
            Tag : constant String := Extract_Tag
              (Buffer,
               Start + Prefix'Length + 1,
               Stop - 1);
            Item : Page_Hash.Cursor;
         begin
            Item := Map.Find (Tag);
            if Page_Hash."/=" (Item, Page_Hash.No_Element) then
               UB.Replace_Slice
                 (Source => Buffer,
                  Low => Start,
                  High => Stop + Suffix'Length - 1,
                  By => Map (Item));
               From := Start + String'(Map (Item))'Length;
            else
               if SF.Index (Source => Tag, Pattern => Prefix) /= 0 then
                  raise Template_Error with "runaway tag: " & Tag;
               else
                  raise Template_Error
                    with "nonexistent tag: {{@@ " & Tag & " @@}}";
               end if;
            end if;
         end;
      end loop;
   end Replace_Tags;

   function Format_Content
     (Content : UB.Unbounded_String;
      What : Elemental.Page.Fragment_Type)
      return UB.Unbounded_String
   is
      use Ada.Strings.Unbounded; --  need & operator
   begin
      case What is
         when Code =>
            return "<code><pre>" & Content & "</pre></code>";
         when Html =>
            return "<div>" & Content & "</div>";
         when Text =>
            return Content;
      end case;
   end Format_Content;

   function To_Html
     (Page : Elemental.Page.Page;
      Template_File : String)
      return UB.Unbounded_String
   is
      Buffer   : UB.Unbounded_String := UB.Null_Unbounded_String;
      Template : UB.Unbounded_String := Read_Template (Template_File);
      Map      : Page_Hash.Map;
   begin
      Map.Include ("TITLE", UB.To_String (Page.Title));

      for I of Page.Fragments loop
         UB.Append (Buffer, Fragment_To_String (I));
         UB.Append (Buffer, Ada.Characters.Latin_1.LF);
      end loop;

      Map.Include ("CONTENT", UB.To_String (Buffer));

      if Page.Date.Valid then
         Map.Include ("DATE", Date_To_String (Page.Date.Value));
      end if;

      Replace_Tags (Template, Map);

      return Template;
   end To_Html;

   function Get_Target
     (Page     : Elemental.Page.Page;
      Settings : Elemental.Settings.Settings)
      return String
   is
      Location : constant String := Get_Target_Directory (Page, Settings) &
        AD.Base_Name (UB.To_String (Page.Path));
   begin
      return Location;
   end Get_Target;

   function Get_Target_Directory
     (Page     : Elemental.Page.Page;
      Settings : Elemental.Settings.Settings)
      return String
   is
      Directory : constant String := UB.To_String (Settings.Output) &
        "/" & UB.To_String (Page.Sub) & "/";
   begin
      return Directory;
   end Get_Target_Directory;

   function Fragment_To_String (Frag : Fragment) return UB.Unbounded_String
   is
   begin
      case Frag.Where is
         when Local =>
            return Format_Content (Frag.Content, Frag.What);
         when External =>
            return Format_Content (Get_External_Fragment (Frag), Frag.What);
      end case;
   end Fragment_To_String;

   function Date_To_String (Date : Ada.Calendar.Time) return String
   is
      package AC renames Ada.Calendar;
      package CF renames Ada.Calendar.Formatting;
      package TZ renames Ada.Calendar.Time_Zones;

      function Month_To_String (Month : AC.Month_Number) return String;
      function Month_To_String (Month : AC.Month_Number) return String
      is
      begin
         case Month is
            when 01 => return "January";
            when 02 => return "February";
            when 03 => return "March";
            when 04 => return "April";
            when 05 => return "May";
            when 06 => return "June";
            when 07 => return "July";
            when 08 => return "August";
            when 09 => return "September";
            when 10 => return "October";
            when 11 => return "November";
            when 12 => return "December";
         end case;
      end Month_To_String;

      UTC : TZ.Time_Offset renames TZ.UTC_Time_Offset;
      Month_String  : constant String := Month_To_String (CF.Month (Date, UTC));
   begin
      return
        SF.Trim (AC.Day_Number'Image (CF.Day (Date, UTC)), Ada.Strings.Both) &
        " " &
        Month_String &
        AC.Year_Number'Image (CF.Year (Date, UTC));
   end Date_To_String;

   function Get_External_Fragment (Frag : Fragment) return UB.Unbounded_String
   is
      File     : IO.File_Type;
      Line     : UB.Unbounded_String := UB.Null_Unbounded_String;
      Result   : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      IO.Open (File, IO.In_File, UB.To_String (Frag.Source), "WCEM=8");
      loop
         begin
            Line := UI.Get_Line (File);
            UB.Append (Result, Line);
            UB.Append (Result, Ada.Characters.Latin_1.LF);
         exception
            when EI.End_Error => exit;
         end;
      end loop;
      IO.Close (File);
      return Result;
   end Get_External_Fragment;
end Elemental.Page;
