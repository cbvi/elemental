with Ada.Strings.Unbounded;

package body Elemental.Page is
   Header : constant String := ""      &
      "<!doctype html>"                &
      "<html><head>"                   &
      "<meta charset=""UTF-8"" />";

   function To_Html (Page : Elemental.Page.Page) return UB.Unbounded_String
   is
      Buffer : UB.Unbounded_String := UB.Null_Unbounded_String;
   begin
      UB.Append (Buffer, Header);
      UB.Append (Buffer, "<title>");
      UB.Append (Buffer, Page.Title);
      UB.Append (Buffer, "</title>");
      UB.Append (Buffer, "</head><body>");
      UB.Append (Buffer, Page.Content);
      UB.Append (Buffer, "</body></html>");
      return Buffer;
   end To_Html;
end Elemental.Page;
