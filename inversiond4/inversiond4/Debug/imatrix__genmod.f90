        !COMPILER-GENERATED INTERFACE MODULE: Tue May 24 15:58:55 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE IMATRIX__genmod
          INTERFACE 
            SUBROUTINE IMATRIX(A,IA,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: A(N,N)
              REAL(KIND=4) :: IA(N,N)
            END SUBROUTINE IMATRIX
          END INTERFACE 
        END MODULE IMATRIX__genmod
