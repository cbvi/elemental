with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Calendar;

package Elemental.Page is
   package UB renames Ada.Strings.Unbounded;

   type Fragment_Type is (Text, Code, Html);
   --  What kind of resource the fragment contains.
   --  The type impacts how the fragment is processed and formatted.

   type Fragment_Place is (Local, External);
   --  Specifies if the fragment is contained in the source of the page
   --  or will require fetching an external file.

   Template_Error : exception;
   --  Raised if there is a problem with the composition of a template file.

   type Fragment (Where : Fragment_Place) is record
      What   : Fragment_Type := Text;
      case Where is
         when External =>
            Source   : UB.Unbounded_String;
         when Local =>
            Content  : UB.Unbounded_String;
      end case;
   end record;
   --  Type representing a fragment from a page. A local fragment contains
   --  the content itself. An external fragment contains a location where
   --  the content can be found.

   package Fragment_Vector is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
       Element_Type => Fragment);
   --  List of fragments.

   package Page_Hash is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String,
        Element_Type => String,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   --  A hash of tag names mapped to the content value.

   type Some_Date (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Value : Ada.Calendar.Time;
         when False =>
            null;
      end case;
   end record;
   --  Optional type that may contain a date value.

   type Page is record
      Title       : UB.Unbounded_String;
      Date        : Some_Date;
      Fragments   : Fragment_Vector.Vector;
   end record;
   --  Type representing a page.

   function To_Html
     (Page : Elemental.Page.Page; Template_File : String)
      return UB.Unbounded_String;
   --  Given a page object and a path to a template file, replaces the tags in
   --  the template file based on data from the page object and returns the
   --  the resulting HTML as a string.

private
   function Read_Template
     (Template_File : String)
      return UB.Unbounded_String;
   --  Reads in and returns all content in the specified file.

   function Extract_Tag
     (Buffer : UB.Unbounded_String;
      Start  : Positive;
      Stop   : Positive)
      return String;
   --  Returns the text data between the delimiters found in a template string.

   procedure Replace_Tags
     (Buffer : in out UB.Unbounded_String;
      Map    :        Page_Hash.Map);
   --  Replaces tags in a template string with the values found in the hash.

   function Format_Content
     (Content : UB.Unbounded_String;
      What : Elemental.Page.Fragment_Type)
      return UB.Unbounded_String;
   --  Dresses the markup based on the type of the fragment.

   function Fragment_To_String
     (Frag : Fragment) return UB.Unbounded_String;
   --  Returns the string value of a fragment, fetching the content if required.

   function Get_External_Fragment
     (Frag : Fragment) return UB.Unbounded_String;
   --  Fetches the content of an external fragment and returns the string value.
end Elemental.Page;
