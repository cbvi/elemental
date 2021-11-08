with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Elemental.Page is
   package UB renames Ada.Strings.Unbounded;

   type Fragment_Type is (Text, Code, Html);
   type Fragment_Place is (Local, External);

   type Fragment (Where : Fragment_Place) is record
      What : Fragment_Type;
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

   type Page is record
      Title       : UB.Unbounded_String;
      Fragments   : Fragment_Vector.Vector;
   end record;

   function To_Html
      (Page : Elemental.Page.Page) return UB.Unbounded_String;

   function Fragment_To_String
      (Frag : Fragment) return UB.Unbounded_String;

   function Get_External_Fragment
      (Frag : Fragment) return UB.Unbounded_String;
end Elemental.Page;
