module mod_weather_station
   implicit none
   private

   type, public :: weather_station
      character(len=128) :: name ! Max 128 bytes for the name
      integer(kind=2) :: min, max ! [-99.9, 99.9], so it fits inside a int8_t
      integer(kind=8) :: sum ! Sum of all (used for average)
      integer :: N ! Number of entries (used for average)
   end type
end module

program naive
   use :: mod_weather_station
   implicit none
   ! Array to store all of the weather stations
   ! In the challenge, there are 413 unique weather stations
   ! And I'm not competing in the 10K keyset challenge, so we'll just use 413.
   integer, parameter :: NUM_STATIONS = 413
   type(weather_station) :: stations(NUM_STATIONS)

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
      character(len=256) :: line ! Next line to parse
      integer :: iostat ! To see if we should stop or not
      integer :: split_pos ! Stores the index of the split (semicolon or dot) which we can use for a substring
      character(len=10) :: temp_str ! To temporarily store the substring of the line for number parsing
      integer :: station_size ! To store the station size so we don't keep doing sizes
      integer :: i ! Iterator

      station_size = 0

      read (fd, '(A)', iostat=iostat) line ! Read line; error code is in iostat
      do while (iostat .EQ. 0)
         split_pos = INDEX(line, ';') ! Index of splitter is in split_pos
         if (split_pos .EQ. 0) then
            write (*, '("A", "A")') "Error! Could not split line: ", line
            stop ! Stop program
         end if

         temp_station%name = TRIM(line(1:split_pos - 1)) ! Name part of the station
         temp_str = TRIM(line(split_pos + 1:)) ! Get what is after the semicolon, but trimmed for whitespace
         split_pos = INDEX(temp_str, '.') !
         temp_str = temp_str(1:split_pos - 1)//temp_str(split_pos + 1:) ! -24.5 => -245, 10.2 => 102
         read (temp_str, *) temp_station%sum
         temp_station%min = temp_station%sum
         temp_station%max = temp_station%sum
         temp_station%N = 1

         i = 1
         do while (i .LE. station_size .AND. stations(i)%name .NE. temp_station%name)
            i = i + 1
         end do

         if (i .GT. station_size) then
            stations(i) = temp_station
            station_size = station_size + 1
         else
            stations(i)%min = MIN(stations(i)%min, temp_station%sum)
            stations(i)%max = MAX(stations(i)%max, temp_station%sum)
            stations(i)%sum = stations(i)%sum + temp_station%sum
            stations(i)%N = stations(i)%N + 1
         end if
         ! Read next line
         read (fd, '(A)', iostat=iostat) line
      end do

      do i = 1, station_size
         print_block: block
            real :: min, max, avg
            min = stations(i)%min/10.0
            max = stations(i)%max/10.0
            avg = stations(i)%sum/10.0/stations(i)%N
            write (*, '(A, ":")') TRIM(stations(i)%name)
            write (*, '("MIN: ", F5.1, " MAX: ", F5.1, " AVG: ", F5.1)') min, max, avg
         end block print_block
      end do
   end block parse

   close (fd)

end program naive
