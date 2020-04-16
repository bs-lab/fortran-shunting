MODULE STACK_MOD
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: mli=64   ! max length of item on string stack

  TYPE, PUBLIC :: StackType
    CHARACTER(LEN=mli), DIMENSION(:), ALLOCATABLE :: data
    INTEGER :: sz    ! size of data on stack
    INTEGER :: cap   ! capacity (allocated size)

  CONTAINS
    PROCEDURE :: print => printx
    PROCEDURE :: push
    PROCEDURE :: pop
    PROCEDURE :: top
    PROCEDURE :: initialize
    PROCEDURE :: size => sizex
  END TYPE StackType

CONTAINS
  SUBROUTINE initialize(this)
    CLASS (StackType) :: this
    INTENT(OUT) :: this
    this%sz = 0
    this%cap = 8
    ALLOCATE(this%data(this%cap))
  END SUBROUTINE initialize

  SUBROUTINE printx(this)
    CLASS(StackType), INTENT(IN) :: this
    print '(1X,A)', this%data(1:this%sz)
  END SUBROUTINE printx

  SUBROUTINE push(this, val)
    CLASS(StackType), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: val
    CHARACTER(LEN=mli), DIMENSION(:), ALLOCATABLE :: data_copy

    ! in case 'this' was not initialized previously
    IF (.NOT.ALLOCATED(this%data))  CALL this%initialize()

    ! need to reallocated memory if already at capacity
    IF (this%sz == this%cap) THEN
      ! make a new array, twice as big
      ALLOCATE(data_copy(this%cap*2))
      ! copy old contents over
      data_copy(1:this%cap) = this%data(1:this%cap)
      ! replace old with new
      DEALLOCATE(this%data)
      ALLOCATE(this%data(this%cap*2))
      this%data = data_copy
      this%cap = this%cap * 2
    END IF

    ! add to end of stack
    this%data(this%sz+1) = val
    this%sz = this%sz + 1
  END SUBROUTINE push

  SUBROUTINE pop(this)
    CLASS(StackType), INTENT(INOUT)  :: this
    IF (this%sz == 0) THEN
      WRITE(0,*)"ERROR -- unable to pop from empty stack"
      STOP
    END IF
    this%data(this%sz) = ""
    this%sz = this%sz - 1
  END SUBROUTINE pop    

  FUNCTION top(this) RESULT(topx)
    CLASS(StackType), INTENT(IN) :: this
    CHARACTER(LEN=mli) :: topx
    ! in case 'this' was not initialized previously
    IF (.NOT.ALLOCATED(this%data))  CALL this%initialize()
    topx = this%data(this%sz)
  END FUNCTION top

  FUNCTION sizex(this) RESULT(sz)
    CLASS(StackType), INTENT(IN) :: this
    INTEGER :: sz
    ! in case 'this' was not initialized previously
    IF (.NOT.ALLOCATED(this%data))  CALL this%initialize()
    sz = this%sz
  END FUNCTION sizex

END MODULE STACK_MOD
