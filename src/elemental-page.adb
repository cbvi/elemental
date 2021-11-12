with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Elemental.Page is
   package IO renames Ada.Text_IO;
   package UI renames Ada.Text_IO.Unbounded_IO;
   package EI renames Ada.IO_Exceptions;
   package SF renames Ada.Strings.Fixed;

   Prefix : constant String := "{{@@";
   Suffix : constant String := "@@}}";

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

      Replace_Tags (Template, Map);

      return Template;
   end To_Html;

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
