# random_tree2
choose a random tree

program chooose_random_tree

    character(len=15) :: c3
    integer :: i
    real*8:: a2

     call random_seed()
     call random_number(a2)
     i= int(a2*10+1)  ! i is the number of file 
     call rand_tree_name(i,c3)  ! this subroutine create the name 'i.tree'

        open(unit=11,file=c3)  ! open the flie and read it 
        read(11,*) i2
        read(11,*)i5
        print*,i2,i5
 !   enddo
    

end program


subroutine rand_tree_name(i,c3)

integer,intent(in):: i 
character(len=10) :: fmt
character(len=15) :: filename,c3

    if (i .lt. 10) then   
        fmt = "(I1,A5)"  
     end if 
     if (i .ge. 10 .and. i .lt. 100)then  
        fmt = "(I2,A5)" 
     endif 
     if (i .ge. 100 .and. i .lt. 1000) then
          fmt = "(I3,A5)" 
      endif 
     if (i .ge. 1000 .and. i .lt. 10000)then  
        fmt = "(I4,A5)"  
      endif
     if (i .eq. 10000)  then 
         fmt = "(I5,A5)" 
      endif
      
       write (filename,fmt) i,".tree"
        c3=trim(filename)
 
return 
end 
