
program minesweeper
   use mod_board
   implicit none
   character :: input

   ! Will read the size from the user and create the NxN boards
   call initialize_boards()

   do while (.TRUE.)
      call print_board()

      read *, input
      if (input .EQ. '.') then
      else if (input .EQ. 'f' .OR. input .EQ. 'F') then
      else
         call move_cursor(input)
      end if

   end do

end program minesweeper
