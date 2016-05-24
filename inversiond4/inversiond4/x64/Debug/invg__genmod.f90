        !COMPILER-GENERATED INTERFACE MODULE: Tue May 24 16:09:44 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INVG__genmod
          INTERFACE 
            SUBROUTINE INVG(M,N,G,Z,EPSILON3,BETA,DAMP,SN)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: G(N,M)
              REAL(KIND=4) :: Z(M)
              INTEGER(KIND=4) :: EPSILON3
              INTEGER(KIND=4) :: BETA
              REAL(KIND=4) :: DAMP
              REAL(KIND=4) :: SN(M,N)
            END SUBROUTINE INVG
          END INTERFACE 
        END MODULE INVG__genmod
