
program minesweeper
   use mod_board
   implicit none
   character :: input
   character(len=3) :: n_str ! max 3 digit number
   integer :: n

   ! Check to see if we should read the number from the user or from ARGV
   if (COMMAND_ARGUMENT_COUNT() .GT. 0) then
      call GET_COMMAND_ARGUMENT(1, n_str) ! Get the first non-command argument
      ! iostat (error code) and iomsg (error message) are optional, so I maybe do that later.
      read (unit=n_str, fmt=*) n
   else
      read *, n
   end if

   ! Will read the size from the user and create the NxN boards
   call initialize_board(n)

   do while (.TRUE.)
      call print_board()

      read *, input
      if (input .EQ. '.') then
         call reveal_current_cell()
      else if (input .EQ. 'f' .OR. input .EQ. 'F') then
      else
         call move_cursor(input)
      end if

   end do

end program minesweeper
