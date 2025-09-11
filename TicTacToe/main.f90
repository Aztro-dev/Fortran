program tictactoe
   implicit none
   character, dimension(3, 3) :: board
   logical :: turn
   integer :: win_status
   integer :: i, j

   ! Initialize board with spaces
   do i = 1, 3
      do j = 1, 3
         board(i, j) = ' '
      end do
   end do

   turn = .true.  ! Initialize turn

   do while (.true.)
      win_status = check_win(board)
      if (win_status /= 0) then
         exit  ! Exit the loop when game is won
      end if

      exit
   end do

contains
   function check_win(board_arr) result(win_result)
      character, dimension(3, 3), intent(in) :: board_arr
      integer :: win_result
      integer :: iter ! iterator for rows/columns

      do i = 1, 1
         ! if ()
      end do

   end function check_win
end program tictactoe
