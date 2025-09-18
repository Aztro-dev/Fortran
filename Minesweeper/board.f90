module mod_board
   use mod_color
   ! Make boards private
   private
   real, parameter :: BOMB_CHANCE = 0.20
   ! Number of rows, half the number of columns
   integer :: N
   character, allocatable :: visible_board(:, :)
   character, allocatable :: bomb_board(:, :)
   complex :: cursor
   ! Make functions accessable
   public reveal_current_cell, initialize_board, print_board, move_cursor
contains
   subroutine reveal_current_cell()
      integer :: x, y
      x = INT(cursor%RE)
      y = INT(cursor%IM)
      if (visible_board(x, y) .NE. ' ') then
         ! Skip mines that have already been revealed
         ! Automatically skips flags, numbers, and clicked-blank cells.
         return
      end if

      if (.NOT. ALLOCATED(bomb_board)) then
         call initialize_bomb_board()
      end if

      if (bomb_board(x, y) .EQ. '#') then
         visible_board(x, y) = '#'
         call print_board()
         print *, "Uncovered bomb. You Lost!"
         DEALLOCATE (bomb_board)
         DEALLOCATE (visible_board)
         stop
      end if
   end subroutine reveal_current_cell

   subroutine initialize_board(rows)
      ! Boards will be NxN, so we will figure out what sizes to use
      integer, intent(in):: rows
      ! Iterators for the initialization
      integer :: i, j

      N = rows

      allocate (visible_board(N, 2*N))
      ! _ (space) for blank/not clicked
      ! * for nothing there
      ! 1-8 for bomb numbers
      ! F for flag
      ! # for bomb
      do i = 1, N
         do j = 1, 2*N
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
   end subroutine initialize_board

   subroutine initialize_bomb_board()
      real :: rand
      integer :: i, j
      integer, allocatable :: seed(:)
      allocate (bomb_board(N, 2*N))

      call random_seed(size=N)
      allocate (seed(n))
      call random_seed(get=seed)

      do i = 1, N
         do j = 1, 2*N
            call RANDOM_NUMBER(rand)
            if (rand .LE. BOMB_CHANCE) then
               bomb_board(i, j) = '#'
            else
               bomb_board(i, j) = '.'
            end if
         end do
      end do
   end subroutine initialize_bomb_board

   subroutine print_board()
      integer :: i, j

      do i = 1, N
         ! Write all characters/cells in the line
         do j = 1, 2*N
            ! If the cursor is currently on the cell, we make it CYAN
            if (int(cursor%RE) .EQ. i .AND. int(cursor%IM) .eq. j) then
               if (visible_board(i, j) .EQ. 'F') then
                  call print_colored(BG_CYAN, FG_BRIGHT_RED, visible_board(i, j))
               else
                  call print_colored_bold(BG_CYAN, FG_BLACK, visible_board(i, j))
               end if
               cycle
            end if

            ! _ (space) blank/not clicked
            ! * for nothing there
            ! 1-8 for bomb numbers
            ! F for flag
            ! # for bomb
            if (visible_board(i, j) .EQ. 'F') then
               call print_colored_bold(BG_BRIGHT_RED, FG_BLACK, 'F')
            else if (visible_board(i, j) .EQ. '#') then
               call print_colored_bold(BG_RED, FG_BLACK, '#')
            else if (visible_board(i, j) .EQ. '*') then
               call print_colored(BG_WHITE, FG_BLACK, ' ')
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

   subroutine move_cursor(char)
      character, intent(in) :: char

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
         cursor%RE = N
      else if (INT(cursor%RE) .GT. n) then
         cursor%RE = 1
      end if
      if (INT(cursor%IM) .EQ. 0) then
         cursor%IM = 2*N
      else if (INT(cursor%IM) .GT. 2*n) then
         cursor%IM = 1
      end if

   end subroutine move_cursor
end module mod_board
