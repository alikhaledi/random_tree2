
program chooose_random_tree

    character(len=15) :: tree_file
    integer :: i,i2,i5,node,nh,i1
    real*8:: a2
    integer,allocatable, dimension(:):: nlink,hemind
    integer,allocatable, dimension(:,:)::link,adj
    
     call random_seed()
     call random_number(a2)
     node=0
     nh=0
     
     i1= int(a2*5+1)  ! i is the number of file 
     call rand_tree_name(i1,tree_file)  ! this subroutine create the name 'i.tree'

     open(unit=11,file=tree_file)  ! open the flie and read it 
     read(11,*) node
     read(11,*)nh
     allocate(nlink(node),link(node,node),hemind(nh),adj(node,node))
     
     do i=1,node
        do j=1, node
           read(11,*) adj(j,i)
        end do 
     end do   
     print*,i1,node,nh
     
     call adj_locations(nh,node,adj,link,nlink,hemind)
     deallocate(nlink,link,hemind,adj)
     
end program


subroutine rand_tree_name(i,tree_file)
 implicit none
 integer,intent(in):: i 
 character(len=10) :: fmt
 character(len=15) :: filename,tree_file

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
      tree_file=trim(filename)
 
return 
end 

 subroutine adj_locations(nh,node,adj,link,nlink,hemind)
    implicit none 
    integer,intent(in):: nh,node
    integer,intent(in):: adj(node,node)
    integer,intent(out):: nlink(node),link(node,node),hemind(nh)
    integer:: f,i,j,k,k3
    nlink=0
    link=0
    hemind=0
    
     k3=0
     do i=node,2,-1 ! look for the location of the peripherals start from the last node and go backward until the 2, 
                                                      !to aviod confusing the central one as heminode in case it has the branching=1.  
         if(sum(adj(i,1:node))==1) then ! the sum of adj is equl to 1 then we have a heminode
             k3=k3+1
             hemind(k3)=node-k3+1  !number the heminodes
         end if  
     enddo

       link=0
       do i=1,node
          k=0
          do j=1,node
             if(adj(i,j).eq.1) then
                k=k+1
                link(i,k)=j
             endif
          enddo
          nlink(i)=k
       enddo
    return 
    end 


 
