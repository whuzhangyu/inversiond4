        !COMPILER-GENERATED INTERFACE MODULE: Tue May 24 15:58:55 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TRANSPOSE__genmod
          INTERFACE 
            SUBROUTINE TRANSPOSE(AR,AC,A,AT)
              INTEGER(KIND=4) :: AC
              INTEGER(KIND=4) :: AR
              REAL(KIND=4) :: A(AR,AC)
              REAL(KIND=4) :: AT(AC,AR)
            END SUBROUTINE TRANSPOSE
          END INTERFACE 
        END MODULE TRANSPOSE__genmod
