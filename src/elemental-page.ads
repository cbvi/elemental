with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Calendar;

package Elemental.Page is
   package UB renames Ada.Strings.Unbounded;

   type Fragment_Type is (Text, Code, Html);
   type Fragment_Place is (Local, External);

   Template_Error : exception;

   type Fragment (Where : Fragment_Place) is record
      What   : Fragment_Type := Text;
      case Where is
         when External =>
            Source   : UB.Unbounded_String;
         when Local =>
            Content  : UB.Unbounded_String;
      end case;
   end record;

   package Fragment_Vector is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
       Element_Type => Fragment);

   package Page_Hash is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String,
        Element_Type => String,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   type Some_Date (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Value : Ada.Calendar.Time;
         when False =>
            null;
      end case;
   end record;

   type Page is record
      Title       : UB.Unbounded_String;
      Date        : Some_Date;
      Fragments   : Fragment_Vector.Vector;
   end record;

   function Read_Template
     (Template_File : String)
      return UB.Unbounded_String;

   function Extract_Tag
     (Buffer : UB.Unbounded_String;
      Start  : Positive;
      Stop   : Positive)
      return String;

   procedure Replace_Tags
     (Buffer : in out UB.Unbounded_String;
      Map    :        Page_Hash.Map);

   function Format_Content
     (Content : UB.Unbounded_String;
      What : Elemental.Page.Fragment_Type)
      return UB.Unbounded_String;

   function To_Html
     (Page : Elemental.Page.Page; Template_File : String)
      return UB.Unbounded_String;

   function Fragment_To_String
      (Frag : Fragment) return UB.Unbounded_String;

   function Get_External_Fragment
      (Frag : Fragment) return UB.Unbounded_String;
end Elemental.Page;
