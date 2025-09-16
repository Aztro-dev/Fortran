module mod_board
   use mod_color
   ! Make boards private
   private
   character, allocatable :: visible_board(:, :)
   character, allocatable :: bomb_board(:, :)
   complex :: cursor
   ! Make functions accessable
   public initialize_boards, print_board, move_cursor
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
      ! F for flag
      do i = 1, n
         do j = 1, 2*n
            visible_board(i, j) = ' '
         end do
      end do

      ! TODO make non-hardcoded
      ! Hardcoded values for testing
      visible_board(2, 2) = 'F'
      visible_board(2, 3) = ' '
      visible_board(2, 4) = '3'
      visible_board(2, 5) = '*'
      cursor = (3.0, 2.0)

      ! We will not populate the bomb board until the user has selected a spot.
      allocate (bomb_board(n, 2*n))

   end subroutine initialize_boards

   subroutine print_board()
      integer :: i, j
      integer :: n

      ! Get the number of rows in the board
      n = SIZE(visible_board, 1)

      do i = 1, n
         ! Write all characters/cells in the line
         do j = 1, 2*n
            ! Don't advance so we don't create a ton of newlines

            ! If the cursor is currently on the cell, we gray it out a lil bit
            if (int(cursor%RE) .EQ. i .AND. int(cursor%IM) .eq. j) then
               if (visible_board(i, j) .EQ. 'F') then
                  call print_colored(BG_CYAN, FG_BRIGHT_RED, visible_board(i, j))
               else
                  call print_colored_bold(BG_CYAN, FG_BLACK, visible_board(i, j))
               end if
               cycle
            end if

            ! * for blank/not clicked
            ! _ (space) for nothing there
            ! 1-8 for bomb numbers
            ! F for flag
            if (visible_board(i, j) .EQ. 'F') then
               call print_colored_bold(BG_RED, FG_BLACK, 'F')
            else if (visible_board(i, j) .EQ. '*') then
               call print_colored(BG_WHITE, FG_BLACK, '*')
            else if (visible_board(i, j) .EQ. ' ') then
               call print_colored(BG_BRIGHT_BLACK, FG_BLACK, ' ')
            else
               ! Do the colors for each type of cell
               select case (visible_board(i, j))
               case ('1')
                  call print_colored_bold(BG_BLACK, FG_BRIGHT_BLUE, '1')
               case ('2')
                  call print_colored_bold(BG_BLACK, FG_GREEN, '2')
               case ('3')
                  call print_colored_bold(BG_BLACK, FG_BRIGHT_RED, '3')
               case ('4')
                  call print_colored_bold(BG_BLACK, FG_BLUE, '4')
               case ('5')
                  call print_colored_bold(BG_BLACK, FG_RED, '5')
               case ('6')
                  call print_colored_bold(BG_BLACK, FG_CYAN, '6')
               case ('7')
                  call print_colored_bold(BG_BLACK, FG_BLACK, '7')
               case ('8')
                  call print_colored_bold(BG_BLACK, FG_BRIGHT_BLACK, '8')
               case default
                  call print_colored_bold(BG_BLACK, FG_WHITE, visible_board(i, j))
               end select
            end if
         end do
         ! newline
         print *
      end do

   end subroutine print_board

   subroutine move_cursor()
      character :: char
      integer :: n

      ! Get the number of rows in the board
      n = SIZE(visible_board, 1)

      read *, char

      if (char .EQ. 'q' .OR. char .EQ. 'Q') then
         stop
      end if

      if (char .EQ. 'w' .OR. char .EQ. 'W') then
         cursor%RE = cursor%RE - 1
      else if (char .EQ. 's' .OR. char .EQ. 'S') then
         cursor%RE = cursor%RE + 1
      end if

      if (char .EQ. 'a' .OR. char .EQ. 'A') then
         cursor%IM = cursor%IM - 1
      else if (char .EQ. 'd' .OR. char .EQ. 'D') then
         cursor%IM = cursor%IM + 1
      end if

      if (INT(cursor%RE) .EQ. 0) then
         cursor%RE = n
      else if (INT(cursor%RE) .GT. n) then
         cursor%RE = 1
      end if
      if (INT(cursor%IM) .EQ. 0) then
         cursor%IM = 2*n
      else if (INT(cursor%IM) .GT. 2*n) then
         cursor%IM = 1
      end if

   end subroutine move_cursor
end module mod_board
