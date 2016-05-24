
program density
  implicit none
  integer, parameter:: m=6240, n=35485
  real,parameter:: e=0.01, beta=1, epsilon3=1          
  integer,parameter:: hit=100            
  real g0(n),dd(m),g1(n),g(n),d0(m),g2(n),z(m)
  real,allocatable:: gg(:,:)
  real,allocatable:: sn(:,:)
  real,parameter:: damp=0.2         !����ϵ��
  integer i,j,k,nit
  real f                      ! Ŀ�꺯��ֵ
  allocate (gg(n,m)) 
  allocate (sn(m,n))
  open (11,file='matrixg0.dat')            ! ����G�ļ�
  open (12,file='resgeoid.dat')     ! �۲������쳣�ļ�
  !open (13,file='density0.dat')       ! ��ʼ�ܶ��ļ�
  open (14,file='gridz.dat')
  open (15,file='density-LS.dat')
 

!//������G�ļ�
  do i=1,n
    read(11,100) (gg(i,j),j=1,m)
  end do
100 format(6240f16.7)                              !change
   close(11)
!//���۲��쳣�ļ�

  do i=1,n
    read(12,*) g0(i)
  end do

  do i=1,m
    read(14,*) z(i)
  end do

  !//����ʼ�ܶ��ļ�
 ! do j=1,m
   ! read(13,*) dd(j)
 ! end do
  dd=0.d0
  call invg(m,n,g,z,epsilon3,beta,damp,sn) 

  nit=0
600 nit=nit+1
  write(*,*) '�������� nit=',nit
  if ( nit>=hit ) then
    write(*,*)    '�������������ޣ��������ܶ�ֵ'
    write(*,*)     '���յ�������', nit
    write(*,*)     '���չ۲�ֵ�����ֵ������', f
    write(15,*)   dd   
    stop
  end if
  call matmul (gg,dd,n,m,1,g1) 
  g=g1-g0
  g2=g*g       
  f=0.0
  do i=1,n
    f=f+g2(i)
  end do
  f=f/n
  f=(f**0.5)
  write(*,*)     '�۲�ֵ�����ֵ������', f  
  if ( f<=e ) then
    write(*,*)    '������ϣ��������ܶ�ֵ'
    write(*,*)     '���յ�������', nit
    write(*,*)     '���չ۲�ֵ�����ֵ������', f
    write(15,*)   dd	
	stop
  else
    call invert (m,n,sn,g,dd)
	go to 600 
  end if 
  deallocate (gg)
  deallocate (sn)
end program density



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!���ݼ���
subroutine invert (m,n,sn,g,dd)
  implicit none
  integer i,j,m,n
  real g(n),dd(m),d0(m),sn(m,n)  
  call matmul  (sn,g,m,n,1,d0)  
  dd=dd-d0                   !ע��˴���+����-d0
end subroutine invert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!����inv(V*S'*S*V'+ee)*V*S'*U'=inv(G'G+II)*G'
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine invg(m,n,g,z,epsilon3,beta,damp,sn)  !gai����
  implicit none
  integer i,j,m,n,epsilon3,beta
  real g(n,m),sn(m,n),damp,ee(n,n),z(m)
  real gt(m,n)
  real,allocatable:: wz(:,:)
  real,allocatable:: wzt(: ,:)
  real,allocatable:: wztwz(:,:)
  real,allocatable:: iwztwz(:,:)
  real,allocatable:: iwztwzgt(:,:)
  real,allocatable:: giwztwzgt(:,:)
  real,allocatable:: gtge(:,:)
  real,allocatable:: giwztwz(:,:)
  allocate (wz(m,m)) 
  allocate (wzt(m,m))
  allocate (wztwz(m,m)) 
  allocate (iwztwz(m,m))
  allocate (iwztwzgt(m,n)) 
  allocate (giwztwzgt(n,n))
  allocate (gtge(n,n)) 
  allocate (giwztwz(n,m))
 
  ee=0.0
  do i=1,n
    do j=1,n
      if (i==j) then
	    ee(i,j)=damp !������,��Ȩ��������Ϣ	
	  end if
    end do
  end do


  wz=0.0
  do i=1,m
    do j=1,m
      if (i==j) then
	    wz(i,j)=(z(i)+epsilon3)**(-beta) !������,��Ȩ��������Ϣ	
	  end if
    end do
  end do

  call transpose (m,m,wz,wzt) 
  call matmul    (wzt,wz,m,m,m,wztwz)  
  call imatrix   (wztwz,iwztwz,m)
  call transpose (n,m,g,gt)      !s' 
  call matmul    (iwztwz,gt,m,m,n,iwztwzgt)  
  call matmul    (g,iwztwz,n,m,m,giwztwz)  
  call matmul    (giwztwz,gt,n,m,n,giwztwzgt)   !v*s' 
  gtge=giwztwzgt+ee
  call matmul    (iwztwzgt,gtge,m,n,n,sn) 
  return
 
end subroutine invg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!��������ӳ���
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine matmul(a,b,ar,ac,bc,c) !����c=a*b
  implicit none
  integer ar,ac,bc,i,j,k
  real a(ar,ac),b(ac,bc),c(ar,bc)  
  
    do i=1,ar
      do j=1,bc
	    c(i,j)=0.d0
	    do k=1,ac
	      c(i,j)=c(i,j)+a(i,k)*b(k,j)
		end do
	  end do
     end do   
	 return
end subroutine matmul



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!����ת���ӳ���
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine transpose (ar,ac,a,at)
  implicit none 
  integer ar,ac,i,j
  real a(ar,ac),at(ac,ar)
  do i=1,ar
    do j=1,ac
      at(j,i)=a(i,j)
    end do
   end do
   return
end subroutine transpose

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!����Ϊ��������ģ��,
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module LinearAlgebra
  implicit none
contains
! �������
subroutine inverse(A,IA)
  implicit none
  real :: A(:,:), IA(:,:)
  real, allocatable :: B(:,:)
  integer :: i,j,N
  N = size(A,1)  
  allocate(B(N,N))
  ! �Ȱ�IA�趨�ɵ�λ����
  forall(i=1:N,j=1:N,i==j) IA(i,j)=1.0
  forall(i=1:N,j=1:N,i/=j) IA(i,j)=0.0
  ! ����ԭ�ȵľ���A, ʹ��B������
  B=A 
  ! ��B���ɶԽ��߾���(���˶Խ�����,��Ϊ0)
  call Upper(B,IA,N) ! �Ȱ�B���������Ǿ���
  call Lower(B,IA,N) ! �ٰ�B���������Ǿ���       
  ! ���
  forall(i=1:N) IA(i,:)=IA(i,:)/B(i,i) 
  return
  deallocate(B)
end subroutine


! �������Ǿ�����ӳ���
subroutine Upper(M,S,N)
  implicit none
  integer :: N
  real :: M(N,N)
  real :: S(N,N)
  integer :: I,J
  real :: E
  do I=1,N-1
    do J=I+1,N              
      E=M(J,I)/M(I,I)
      M(J,I:N)=M(J,I:N)-M(I,I:N)*E
      S(J,:)=S(J,:)-S(I,:)*E
    end do
  end do
  return
end subroutine Upper
! �������Ǿ�����ӳ���
subroutine Lower(M,S,N)
  implicit none
  integer :: N
  real  :: M(N,N)
  real  :: S(N,N)
  integer :: I,J
  real :: E
  do I=N,2,-1
    do J=I-1,1,-1           
      E=M(J,I)/M(I,I)
      M(J,1:N)=M(J,1:N)-M(I,1:N)*E
      S(J,:)=S(J,:)-S(I,:)*E
    end do
  end do
  return
end subroutine Lower
end module

subroutine imatrix(A,IA,N)
  use LinearAlgebra
  implicit none
  integer N,I
  real A(N,N),IA(N,N)   
  call inverse(A,IA) 
  return
end 
