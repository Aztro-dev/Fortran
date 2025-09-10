program calculator
   implicit none
   real :: a, b
   character :: operation

   print *, 'Enter the first operand:'
   read *, a
   print *, 'Enter the second operand:'
   read *, b
   print *, 'Enter the operation'
   read *, operation

   if (operation .EQ. '+') then
      a = a + b
   else if (operation .EQ. '-') then
      a = a - b
   else if (operation .EQ. '*') then
      a = a*b
   else if (operation .EQ. '/') then
      a = a/b
   else if (operation .EQ. '^') then
      a = a**b
   end if

   print *, a

end program calculator
