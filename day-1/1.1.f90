program d1p1

integer :: inc, p, d, r

inc = 0

read *, p
do while (1 .eq. 1)
read (*, *, IOSTAT=r) d
  if(r < 0) then
    exit
  end if
  if (d > p) then
    inc = inc + 1
  end if
  p = d
end do

print *, inc

end program d1p1
