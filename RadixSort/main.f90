module m_radix
   implicit none
   private
   integer, parameter :: BITS = 16 ! The radix defines the MSB of the RADIX
   integer, parameter :: RADIX = ISHFT(1, BITS) ! The radix defines the amount of buckets we will be using.
   public radix_sort
contains
   subroutine radix_sort(array, N)
      integer, intent(in) :: N
      integer(kind=8), dimension(N), intent(inout)  :: array
      integer(kind=8), dimension(N)  :: temp_array
      ! Start 0-indexed so we can use bitwise operations
      integer, dimension(0:RADIX - 1) :: count
      integer :: i, temp
      integer(kind=8) :: max = 0
      integer(kind=8) :: exponent = 1

      ! Get the max of the array for the upper bound of the outer for loop
      do i = 1, N
         if (array(i) .GT. max) then
            max = array(i)
         end if
      end do

      do while (max/exponent .GT. 0)
         count(:) = 0

         do i = 1, N
            ! temp = (array(i) / exponent) % RADIX
            ! temp = (array(i) / exponent) & (RADIX - 1)
            temp = array(i)/exponent
            temp = IAND(temp, RADIX - 1)
            count(temp) = count(temp) + 1
         end do

         do i = 1, RADIX - 1
            count(i) = count(i) + count(i - 1)
         end do

         do i = N, 1, -1
            ! arr(i) / exponent % RADIX
            ! arr(i) / exponent & (RADIX - 1)
            temp = array(i)/exponent
            temp = IAND(temp, RADIX - 1)
            temp_array(count(temp)) = array(i)
            count(temp) = count(temp) - 1
         end do

         array = temp_array

         ! exponent = exponent * 256
         ! exponent = exponent << BITS
         exponent = ISHFT(exponent, BITS)
      end do
   end subroutine

end module m_radix

program main
   use m_radix
   implicit none
   integer, parameter :: N = ISHFT(1, 24) ! N = 1 << 24
   integer(kind=8), dimension(N)  :: array
   real :: start_time, end_time ! used for timing the radix_sort function

   call initialize_array(array)

   call CPU_TIME(start_time)
   call radix_sort(array, N)
   call CPU_TIME(end_time)

   call verify_sorted(array)

   ! The time is normally in seconds, so we have to multiply by E6 to get to microseconds.
   print *, "Elapsed CPU time: ", (end_time - start_time)*10**3, " milliseconds"

contains
   subroutine initialize_array(arr)
      integer(kind=8), dimension(N), intent(inout)  :: arr
      real, dimension(N)   :: real_array
      integer(kind=8) :: i = 1

      call RANDOM_SEED()
      call RANDOM_NUMBER(real_array)

      ! Numbers up to 2^64 - 1
      arr = FLOOR(real_array*ISHFT(i, 64), kind=8)
   end subroutine initialize_array

   subroutine verify_sorted(arr)
      integer(kind=8), dimension(N), intent(inout)  :: arr
      integer :: i
      do i = 1, N - 1
         if (arr(i) .GT. arr(i + 1)) then
            print *, 'Failed Sort'
            error stop
         end if
      end do

      print *, 'Successful Sort'
   end subroutine verify_sorted
end program main
