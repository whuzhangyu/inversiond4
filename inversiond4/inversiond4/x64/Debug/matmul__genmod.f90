        !COMPILER-GENERATED INTERFACE MODULE: Tue May 24 16:09:44 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MATMUL__genmod
          INTERFACE 
            SUBROUTINE MATMUL(A,B,AR,AC,BC,C)
              INTEGER(KIND=4) :: BC
              INTEGER(KIND=4) :: AC
              INTEGER(KIND=4) :: AR
              REAL(KIND=4) :: A(AR,AC)
              REAL(KIND=4) :: B(AC,BC)
              REAL(KIND=4) :: C(AR,BC)
            END SUBROUTINE MATMUL
          END INTERFACE 
        END MODULE MATMUL__genmod
