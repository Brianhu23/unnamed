      program gengeo
      implicit none
      real(8) :: Transvec1(3),Transvec2(3),Transvec3(3)
      real(8) :: r(3,4),coord(3)
      integer :: np,nb,nx,ny
      integer :: ip,ib,iv1,iv2
      Character(4) :: conc1,conc2
!
!***********************************************     
!
      Transvec1=(/1.0d0,0.0d0,0.0d0/)
      Transvec2=(/0.0d0,1.414213562d0,0.0d0/)
      Transvec3=(/0.0d0,0.0d0,2.000000d0/)
!
!
      r(:,1)=(/0.0d0,0.0d0,0.0d0/)
!
      r(:,2)=(/0.0d0,0.0d0,1.00d0/)
!
      r(:,3)=(/0.50d0,0.7071067812d0,0.50d0/)
!
      r(:,4)=(/0.50d0,0.7071067812d0,1.5d0/)
!
      ny=7
      np=15
      conc1='0.50'
      conc2='0.50'
!     
      nb=4*ny
!
!*************************************************************
!
      PRINT*,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      PRINT*,'##@DGEO_GEO'
!
      write(*,"(F16.9,F16.9)") Transvec1(1:2)
      write(*,"(F16.9,F16.9)") Transvec2(1:2)*ny
      write(*,"(F16.9,F16.9,F16.9)") -Transvec3
      write(*,"(F16.9,F16.9,F16.9)") Transvec3
      write(*,"(4x,I3,8x,I3)") np, nb
!       
      do ip=0,np+1
        if(ip.eq.0) PRINT*,'    LEFT_LEAD'
        if(ip.eq.1) PRINT*,'    CENTRAL_LAYERS'
        if(ip.eq.np+1) PRINT*,'    RIGHT_LEAD'
      do iv2=0,ny-1
      do ib=1,4
        print*,''
        coord=r(:,ib)+iv2*Transvec2(:)+ip*Transvec3(:)
        if(iv2.eq.0.and.ib.eq.1) then
          write(*,"(F16.9,F16.9,F16.9,'    !    PL',I3)") coord, IP
        else
          write(*,"(F16.9,F16.9,F16.9)") coord
        end if
        write(*,"(F16.9,F16.9,F16.9)") Transvec1
!
      if (ip.GE.6 .AND. ip.LE.np-5) then
!
        if ( iv2.le.1 .or. iv2.ge.ny-2  ) then
!
          if ( (iv2.eq.1 .and. ib.ge.3) ) then
            print*,'    2'
            print*,'    Vac           ',conc2
            print*,'    Cu            ',conc1
          else if (iv2.eq.ny-2 .and. ib.le.2)  then
            print*,'    2'
            print*,'    Vac           ',conc1
            print*,'    Cu            ',conc2
          else
            print*,'    1'
            print*,'    Vac           1.00'
          end if
!
        else
            print*,'    1'
            print*,'    Cu          1.00'
        end if
!
      else
        if ( iv2.le.1 .or. iv2.ge.ny-2 )  then
          print*,'    1'
          print*,'    Vac          1.00'
        else
          print*,'    1'
          print*,'    Cu           1.00'
        end if
      end if
!
      end do
      end do
      end do
!
      PRINT*,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      PRINT*,'##@DKSMP_GEO'
      PRINT*,'AXYZ0    60        1                 AXEQ  AYEQ' 
      PRINT*,'AXYZ1    120       1                 AXNEQ AYNEQ'
      PRINT*,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
!
      END PROGRAM
