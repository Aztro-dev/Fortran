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

   call print_board(board)

   do while (.true.)
      win_status = check_win(board)
      if (win_status /= 0) then
         exit
      end if

      read (*, *) i, j

      if (turn) then
         board(i, j) = 'X'
      else
         board(i, j) = 'O'
      end if

      turn = .not. turn

      call print_board(board)
   end do

   if (win_status .EQ. 1) then
      print *, 'X Wins'
   else if (win_status .EQ. 2) then
      print *, 'O Wins'
   end if

contains
   function check_win(board) result(win_status)
      character, dimension(3, 3), intent(in) :: board
      integer :: win_status
      integer :: i ! iterator for rows/columns

      ! 'X' & 1 = 0
      ! 'O' & 1 = 1
      ! ICHAR(character) -> integer
      ! IAND(integer, integer) -> integer & integer

      do i = 1, 3
         if (board(i, 1) .ne. ' ' .and. board(i, 1) .eq. board(i, 2) .and. board(i, 2) .eq. board(i, 3)) then
            win_status = IAND(ICHAR(board(i, 1)), 1) + 1
            return
         end if
      end do

      do j = 1, 3
         if (board(1, j) .ne. ' ' .and. board(1, j) .eq. board(2, j) .and. board(2, j) .eq. board(3, j)) then
            win_status = IAND(ICHAR(board(1, j)), 1) + 1
            return
         end if
      end do

      if (board(1, 1) .ne. ' ' .and. board(1, 1) .eq. board(2, 2) .and. board(2, 2) .eq. board(3, 3)) then
         win_status = IAND(ICHAR(board(1, 1)), 1) + 1
         return
      end if

      if (board(1, 3) .ne. ' ' .and. board(1, 3) .eq. board(2, 2) .and. board(2, 2) .eq. board(3, 1)) then
         win_status = IAND(ICHAR(board(1, 3)), 1) + 1
         return
      end if

      win_status = 0

   end function check_win

   subroutine print_board(board)
      character, dimension(3, 3), intent(in) :: board
      integer :: i, j

      do i = 1, 3
         write (*, '(a)', advance='yes') '-------'
         print '("|", A, "|", A, "|", A, "|")', board(i, 1), board(i, 2), board(i, 3)
      end do
      write (*, '(a)', advance='yes') '-------'
   end subroutine print_board
end program tictactoe
