module mod_weather_station
   implicit none
   private

   type, public :: weather_station
      character(len=128) :: name ! Max 128 bytes for the name
      integer(kind=1) :: min, max ! [-99.9, 99.9], so it fits inside a int8_t
      integer :: sum ! Sum of all (used for average)
      integer :: N ! Number of entries (used for average)
   end type
end module
program naive
   use :: mod_weather_station
   implicit none
   ! Array to store all of the weather stations
   ! In the challenge, there are 413 unique weather stations
   ! And I'm not competing in the 10K keyset challenge, so we'll just use 413.
   type(weather_station) :: stations(413)

   integer :: fd ! File descriptor for the measurements file
   logical :: file_exists ! To make sure that the file exists before segfaulting lol

   inquire (file="measurements.txt", exist=file_exists)
   if (.NOT. file_exists) then
      print *, "You need to generate the measurements.txt and place it in the directory."
      print *, "Read the instructions in the README.md file for more information."
      stop
   end if

   open (newunit=fd, file="measurements.txt", status="old", action="read")
   parse: block
      type(weather_station) :: temp_station
      temp_station%name = "Erm what the sigma"
      temp_station%min = 0
      temp_station%max = 0
      temp_station%sum = 0
      temp_station%N = 0
      stations(1) = temp_station
   end block parse

   print *, stations(1)%name
   close (fd)

end program naive
