module mod_color
   private
   character, parameter :: color_escape = CHAR(27)
   public print_colored
contains
   subroutine print_colored(background, foreground, character)
      character, dimension(4), intent(in) :: background, foreground
      character, intent(in) :: character
      write (*, '(A, A, A)', advance="no") color_escape//background//color_escape//foreground, character, color_escape//'[0m'
   end subroutine
end module mod_color
