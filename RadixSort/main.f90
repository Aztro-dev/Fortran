module m_radix
   implicit none
   private
   integer, parameter :: RADIX = 256 ! The radix defines the amount of buckets we will be using.
   public radix_sort
contains
   subroutine radix_sort(array, N)
      integer, intent(in) :: N
      integer(kind=8), dimension(N), intent(inout)  :: array
   end subroutine
end module m_radix

program main
   use m_radix
   implicit none
   integer, parameter :: N = ISHFT(1, 16) ! N = 1 << 16
   integer(kind=8), dimension(N)  :: array
   real :: start_time, end_time ! used for timing the radix_sort function

   call initialize_array(array)

   call CPU_TIME(start_time)
   call radix_sort(array, N)
   call CPU_TIME(end_time)

   ! The time is normally in seconds, so we have to multiply by E6 to get to microseconds.
   print *, "Elapsed CPU time: ", (end_time - start_time)*10**6, " microseconds"

contains
   subroutine initialize_array(arr)
      integer(kind=8), dimension(N), intent(inout)  :: arr
      real, dimension(N)   :: real_array
      integer :: i

      call RANDOM_SEED()
      call RANDOM_NUMBER(real_array)

      arr = FLOOR(real_array*HUGE(arr(1)), kind=8)
   end subroutine initialize_array
end program main
