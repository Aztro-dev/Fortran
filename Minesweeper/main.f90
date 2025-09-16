
program minesweeper
   use mod_board
   implicit none

   ! Will read the size from the user and create the NxN boards
   call initialize_boards()

   do while (.TRUE.)
      call print_board()
      call move_cursor()
   end do

end program minesweeper
