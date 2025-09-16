module mod_color
   private
   character, parameter :: color_escape = CHAR(27)

   ! Styles
   character(len=*), parameter, public :: STYLE_RESET = '[0m'
   character(len=*), parameter, public :: STYLE_BOLD = '[1m'
   character(len=*), parameter, public :: STYLE_DIM = '[2m'
   character(len=*), parameter, public :: STYLE_ITALIC = '[3m'
   character(len=*), parameter, public :: STYLE_UNDERLINE = '[4m'
   character(len=*), parameter, public :: STYLE_BLINK = '[5m'
   character(len=*), parameter, public :: STYLE_REVERSE = '[7m'
   character(len=*), parameter, public :: STYLE_STRIKETHROUGH = '[9m'

   ! Foreground colors
   character(len=*), parameter, public :: FG_BLACK = '[30m'
   character(len=*), parameter, public :: FG_RED = '[31m'
   character(len=*), parameter, public :: FG_GREEN = '[32m'
   character(len=*), parameter, public :: FG_YELLOW = '[33m'
   character(len=*), parameter, public :: FG_BLUE = '[34m'
   character(len=*), parameter, public :: FG_MAGENTA = '[35m'
   character(len=*), parameter, public :: FG_CYAN = '[36m'
   character(len=*), parameter, public :: FG_WHITE = '[37m'
   character(len=*), parameter, public :: FG_DEFAULT = '[39m'

   ! Bright foreground colors
   character(len=*), parameter, public :: FG_BRIGHT_BLACK = '[90m'
   character(len=*), parameter, public :: FG_BRIGHT_RED = '[91m'
   character(len=*), parameter, public :: FG_BRIGHT_GREEN = '[92m'
   character(len=*), parameter, public :: FG_BRIGHT_YELLOW = '[93m'
   character(len=*), parameter, public :: FG_BRIGHT_BLUE = '[94m'
   character(len=*), parameter, public :: FG_BRIGHT_MAGENTA = '[95m'
   character(len=*), parameter, public :: FG_BRIGHT_CYAN = '[96m'
   character(len=*), parameter, public :: FG_BRIGHT_WHITE = '[97m'

   ! Background colors
   character(len=*), parameter, public :: BG_BLACK = '[40m'
   character(len=*), parameter, public :: BG_RED = '[41m'
   character(len=*), parameter, public :: BG_GREEN = '[42m'
   character(len=*), parameter, public :: BG_YELLOW = '[43m'
   character(len=*), parameter, public :: BG_BLUE = '[44m'
   character(len=*), parameter, public :: BG_MAGENTA = '[45m'
   character(len=*), parameter, public :: BG_CYAN = '[46m'
   character(len=*), parameter, public :: BG_WHITE = '[47m'
   character(len=*), parameter, public :: BG_DEFAULT = '[49m'

   ! Bright background colors
   character(len=*), parameter, public :: BG_BRIGHT_BLACK = '[100m'
   character(len=*), parameter, public :: BG_BRIGHT_RED = '[101m'
   character(len=*), parameter, public :: BG_BRIGHT_GREEN = '[102m'
   character(len=*), parameter, public :: BG_BRIGHT_YELLOW = '[103m'
   character(len=*), parameter, public :: BG_BRIGHT_BLUE = '[104m'
   character(len=*), parameter, public :: BG_BRIGHT_MAGENTA = '[105m'
   character(len=*), parameter, public :: BG_BRIGHT_CYAN = '[106m'
   character(len=*), parameter, public :: BG_BRIGHT_WHITE = '[107m'

   public print_colored, print_colored_bold

contains
   subroutine print_colored(background, foreground, character)
      character(len=*), intent(in) :: background, foreground
      character, intent(in) :: character
      write (*, '(A)', advance="no") color_escape//background//color_escape//foreground
      write (*, '(A)', advance="no") character
      write (*, '(A)', advance="no") color_escape//STYLE_RESET
   end subroutine print_colored

   subroutine print_colored_bold(background, foreground, character)
      character(len=*), intent(in) :: background, foreground
      character, intent(in) :: character
      write (*, '(A)', advance="no") color_escape//background//color_escape//foreground
      write (*, '(A)', advance="no") color_escape//STYLE_BOLD//character
      write (*, '(A)', advance="no") color_escape//STYLE_RESET
   end subroutine print_colored_bold
end module mod_color
