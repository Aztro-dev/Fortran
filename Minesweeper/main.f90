module mod_board
   ! Make boards private
   private
   character, allocatable :: visible_board(:, :)
   character, allocatable :: bomb_board(:, :)
   ! Make functions accessable
   public initialize_boards, print_board
contains
   subroutine initialize_boards()
      ! Boards will be NxN, so we will figure out what sizes to use
      integer :: n
      ! Iterators for the initialization
      integer :: i, j

      read *, n

      allocate (visible_board(n, n))
      do i = 1, n
         do j = 1, n
            visible_board(i, j) = '*'
         end do
      end do

      ! We will not populate the bomb board until the user has selected a spot.
      allocate (bomb_board(n, n))

   end subroutine initialize_boards

   subroutine print_board()
      integer :: i, j
      integer :: n
      character :: color_escape

      color_escape = CHAR(27)

      ! Get the number of rows in the board, which
      ! should also be the same as the number of columns
      n = SIZE(visible_board, 1)

      do i = 1, n
         ! Write all characters/cells in the line
         do j = 1, n
            ! Don't advance so we don't create a ton of newlines
       write (*, '(A, "[107m", A, "[30m", A, A, "[0m")', advance="no") color_escape, color_escape, visible_board(i, j), color_escape
         end do
         ! newline
         print *
      end do

   end subroutine print_board
end module mod_board

program minesweeper
   use mod_board
   implicit none

   ! Will read the size from the user and create the NxN boards
   call initialize_boards()

   call print_board()

end program minesweeper
