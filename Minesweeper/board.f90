module mod_board
   use mod_color
   ! Make boards private
   private
   character, allocatable :: visible_board(:, :)
   character, allocatable :: bomb_board(:, :)
   complex :: cursor
   ! Make functions accessable
   public initialize_boards, print_board
contains
   subroutine initialize_boards()
      ! Boards will be NxN, so we will figure out what sizes to use
      integer :: n
      ! Iterators for the initialization
      integer :: i, j

      read *, n

      allocate (visible_board(n, 2*n))
      ! * for blank/not clicked
      ! _ (space) for nothing there
      ! 1-8 for bomb numbers
      ! # for bomb
      do i = 1, n
         do j = 1, 2*n
            visible_board(i, j) = ' '
         end do
      end do

      visible_board(2, 2) = '#'

      ! We will not populate the bomb board until the user has selected a spot.
      allocate (bomb_board(n, 2*n))

   end subroutine initialize_boards

   subroutine print_board()
      integer :: i, j
      integer :: n

      ! Get the number of rows in the board, which
      ! should also be the same as the number of columns
      n = SIZE(visible_board, 1)

      do i = 1, n
         ! Write all characters/cells in the line
         do j = 1, 2*n
            ! Don't advance so we don't create a ton of newlines
            ! * for blank/not clicked
            ! _ (space) for nothing there
            ! 1-8 for bomb numbers
            ! # for bomb
            if (visible_board(i, j) .EQ. '#') then
               call print_colored(BG_RED, FG_RED, ' ')
            else if (visible_board(i, j) .EQ. '*') then
               call print_colored(BG_WHITE, FG_BLACK, '*')
            else if (visible_board(i, j) .EQ. ' ') then
               call print_colored(BG_WHITE, FG_BLACK, '$')
            else
               write (*, '(A, "[47m", " ", A, "[0m")', advance="no") color_escape, color_escape
            end if
         end do
         ! newline
         print *
      end do

   end subroutine print_board
end module mod_board
