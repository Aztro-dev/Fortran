
program minesweeper
   use mod_board
   implicit none

   ! Will read the size from the user and create the NxN boards
   call initialize_boards()

   call print_board()

end program minesweeper
