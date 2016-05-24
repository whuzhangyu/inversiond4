        !COMPILER-GENERATED INTERFACE MODULE: Tue May 24 16:09:44 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INVERT__genmod
          INTERFACE 
            SUBROUTINE INVERT(M,N,SN,G,DD)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: SN(M,N)
              REAL(KIND=4) :: G(N)
              REAL(KIND=4) :: DD(M)
            END SUBROUTINE INVERT
          END INTERFACE 
        END MODULE INVERT__genmod
