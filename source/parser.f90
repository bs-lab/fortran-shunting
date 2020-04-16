MODULE PARSER_MOD

PRIVATE
PUBLIC mss, mlc, mli, TokenType, TOKENIZER, CREATE_STACK, PRINT_TOKENS, PRINT_STACK

INTEGER, PARAMETER :: mss=5000    ! max size of stack
INTEGER, PARAMETER :: mlc=1024    ! max length of a record (equation)
INTEGER, PARAMETER :: mli=64      ! max length of item on stack
INTEGER, PARAMETER :: num_opers=13
CHARACTER(LEN=2), DIMENSION(num_opers), PARAMETER :: operators = ['**', '^ ', '/ ', '* ', '- ', &
                                                                  '+ ', '==', '<=', '>=', '< ', &
                                                                  '> ', '<>', '!=']
INTEGER, DIMENSION(num_opers), PARAMETER :: oper_priority = [3, 3, 2, 2, 1, &
                                                             1, 0, 0, 0, 0, &
                                                             0, 0, 0]

TYPE TokenType
  CHARACTER(LEN=32) :: string   ! e.g. '12.345' or 'var_name'
  CHARACTER(LEN=11) :: ttype    ! 'number', 'variable', 'left_paren', 'right_paren', 'operator'
END TYPE TokenType

CONTAINS

  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE PRINT_TOKENS(OUNIT, tokens, num_tokens)
  IMPLICIT NONE
  INTEGER :: i, OUNIT, num_tokens
  TYPE(TokenType), DIMENSION(num_tokens) :: tokens
  INTENT(IN) :: OUNIT, tokens, num_tokens

  WRITE(OUNIT, '(A)', ADVANCE='no') "tokens:"
  DO i = 1, num_tokens
    WRITE(OUNIT, '(A)', ADVANCE='no') " " // TRIM(tokens(i)%string)
  END DO
  WRITE(OUNIT, '(A)')

  END SUBROUTINE PRINT_TOKENS


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE PRINT_STACK(OUNIT, stack)
  use stack_mod
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: OUNIT
  TYPE(StackType), INTENT(IN) :: stack
  INTEGER :: i

  WRITE(OUNIT, '(A)', ADVANCE='no') "rpn stack:"
  DO i = 1, stack%size()
    WRITE(OUNIT, '(A)', ADVANCE='no') " " // TRIM(stack%data(i))
  END DO
  WRITE(OUNIT, '(A)')

  END SUBROUTINE PRINT_STACK


  ! ------------------------------------------------------------------------------------------------
  FUNCTION GET_OPER_INDEX(oper) RESULT(loc)
  IMPLICIT NONE
  INTEGER :: loc, i
  CHARACTER(LEN=*) :: oper
  INTENT(IN) :: oper

  loc = 0
  DO i = 1, num_opers
    IF (oper == TRIM(operators(i))) THEN
      loc = i
      EXIT
    END IF
  END DO

  END FUNCTION GET_OPER_INDEX


  ! ------------------------------------------------------------------------------------------------
  FUNCTION GET_PRIORITY(oper) RESULT(xout)
  IMPLICIT NONE
  INTEGER :: loc, xout
  CHARACTER(LEN=*) :: oper
  INTENT(IN) :: oper

  loc = GET_OPER_INDEX(oper)

  IF (loc < 1) THEN
    xout = -9999
  ELSE
    xout = oper_priority(loc)
  END IF

  END FUNCTION GET_PRIORITY


  ! ------------------------------------------------------------------------------------------------
  PURE LOGICAL FUNCTION IsNumeric(my_char) 
  CHARACTER(LEN=1), INTENT(IN) :: my_char
    IF (ICHAR(my_char) == 46 .OR. (ICHAR(my_char) >= 48 .AND. ICHAR(my_char) <= 57)) THEN
      IsNumeric = .TRUE.
    ELSE
      IsNumeric = .FALSE.
    END IF
  END FUNCTION IsNumeric

  ! ------------------------------------------------------------------------------------------------
  PURE LOGICAL FUNCTION IsAlphabetic(my_char) 
  CHARACTER(LEN=1), INTENT(IN) :: my_char
    IF ((ICHAR(my_char) >= 65 .AND. ICHAR(my_char) <= 90) .OR. &    ! A through Z
        (ICHAR(my_char) >= 97 .AND. ICHAR(my_char) <= 122)) THEN    ! a through z
      IsAlphabetic = .TRUE.
    ELSE
      IsAlphabetic = .FALSE.
    END IF
  END FUNCTION IsAlphabetic


  ! ------------------------------------------------------------------------------------------------
  PURE LOGICAL FUNCTION IsAlphaNumeric(my_char)
  CHARACTER(LEN=1), INTENT(IN) :: my_char
    IF (IsNumeric(my_char) .OR. IsAlphabetic(my_char)) THEN
      IsAlphaNumeric = .TRUE.
    ELSE
      IsAlphaNumeric = .FALSE.
    END IF
  END FUNCTION IsAlphaNumeric


  ! ------------------------------------------------------------------------------------------------
  FUNCTION PEEK_NEXT_CHAR(card, curr_loc) RESULT(next_char)
  IMPLICIT NONE
  CHARACTER(LEN=mlc) :: card
  CHARACTER(LEN=1) :: next_char
  INTEGER :: curr_loc
  INTENT(IN) :: card, curr_loc

  IF (curr_loc == LEN_TRIM(card)) THEN
    next_char = CHAR(0)  ! null character
  ELSE
    next_char = card(curr_loc+1:curr_loc+1)
  END IF

  END FUNCTION PEEK_NEXT_CHAR


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE TOKENIZER(card, tokens, num_tokens)
  IMPLICIT NONE
  TYPE(TokenType), DIMENSION(mss) :: tokens
  CHARACTER(LEN=mlc) :: card 
  CHARACTER(LEN=1) :: curr_char, next_char
  LOGICAL :: exit_flag
  INTEGER :: i, vc, c, t, num_tokens, loc
  INTENT(IN) :: card
  INTENT(OUT) :: tokens, num_tokens

  ! get tokens
  num_tokens = 0
  exit_flag = .FALSE.
  c = 0

  DO
    c = c + 1
    IF (c > LEN_TRIM(card)) THEN
      EXIT
    END IF

    curr_char = card(c:c)
    IF (curr_char == " ")  CYCLE

    IF (curr_char == "(") THEN
      num_tokens = num_tokens + 1
      tokens(num_tokens)%string = curr_char
      tokens(num_tokens)%ttype = 'left_paren'
      CYCLE
    END IF
    
    IF (curr_char == ")") THEN
      num_tokens = num_tokens + 1
      tokens(num_tokens)%string = curr_char
      tokens(num_tokens)%ttype = 'right_paren'
      CYCLE
    END IF
    
    ! check if this character plus next character are a two-character operator
    next_char = PEEK_NEXT_CHAR(card,c)
    IF (ICHAR(next_char) /= 0 .AND. ICHAR(next_char) /= 32) THEN  ! neither null nor space
      loc = GET_OPER_INDEX(curr_char // next_char)
      IF (loc > 0) THEN
        num_tokens = num_tokens + 1
        tokens(num_tokens)%string = curr_char // next_char
        tokens(num_tokens)%ttype = 'operator'
        c = c + 1
        CYCLE
      END IF
    END IF
         
    ! check if this character is a single-character operator
    loc = GET_OPER_INDEX(curr_char)
    IF (loc > 0) THEN
      num_tokens = num_tokens + 1
      tokens(num_tokens)%string = curr_char
      tokens(num_tokens)%ttype = 'operator'
      CYCLE
    END IF

    IF (IsAlphaNumeric(curr_char)) THEN
      num_tokens = num_tokens + 1
      vc = 1
      IF (IsNumeric(curr_char)) THEN
        tokens(num_tokens)%ttype = 'number'
      ELSE
        tokens(num_tokens)%ttype = 'variable'
      END IF
      tokens(num_tokens)%string(:) = ""
      tokens(num_tokens)%string(vc:vc) = curr_char

      ! read until next non-alphanumeric value
      DO
        next_char = PEEK_NEXT_CHAR(card, c)
        IF (ICHAR(next_char) == 0)  EXIT   ! found end-of-line

        IF (IsAlphaNumeric(next_char)) THEN
          vc = vc + 1
          c = c + 1
          tokens(num_tokens)%string(vc:vc) = next_char
        ELSE
          ! found end of number or variable name
          EXIT
        END IF
      END DO
    END IF
  END DO

  DO t = 1, num_tokens
    IF (tokens(t)%ttype /= "number" .AND. &
        tokens(t)%ttype /= "variable" .AND. &
        tokens(t)%ttype /= "left_paren" .AND. &
        tokens(t)%ttype /= "right_paren" .AND. &
        tokens(t)%ttype /= "operator") THEN
      WRITE(0,*)"unknown token type for " // tokens(t)%string
      STOP
    END IF
  END DO

  ! Clean up any instances where a plus or minus sign (unary) that should be associated with
  !   a value was instead treated as a separate token.
  t = 0
  DO
    t = t + 1
    IF (t > (num_tokens - 2)) EXIT

    IF (tokens(t)%ttype == "operator" .AND. tokens(t+1)%ttype == "operator" .AND. &
        (tokens(t+2)%ttype == "number" .OR. tokens(t+2)%ttype == "variable")) THEN

      IF (tokens(t+1)%string == "-") THEN
        IF (tokens(t+2)%ttype == "number") THEN
          ! change "... * - 31" to "... * -31"
          tokens(t+1)%string = "-" // TRIM(tokens(t+2)%string)
          tokens(t+1)%ttype = tokens(t+2)%ttype
           
          DO i = t+2, num_tokens-1
            tokens(i) = tokens(i+1)
          END DO
          num_tokens = num_tokens - 1

        ELSE
          ! change "... * - AAA" to "... * (-1 * AAA)"
          DO i = num_tokens, t+3, -1
            tokens(i+3) = tokens(i)
          END DO
          tokens(t+5)%ttype = "right_paren"
          tokens(t+5)%string = ")"
          tokens(t+4) = tokens(t+2)
          tokens(t+3)%ttype = "operator"
          tokens(t+3)%string = "*"
          tokens(t+2)%ttype = "number"
          tokens(t+2)%string = "-1"
          tokens(t+1)%ttype = "left_paren"
          tokens(t+1)%string = "("
          num_tokens = num_tokens + 3
        END IF

      ELSEIF (tokens(t+1)%string == "+") THEN
        ! change "* + 31" to "* 31"
        DO i = t+1, num_tokens-1
          tokens(i) = tokens(i+1)
        END DO
        num_tokens = num_tokens - 1

      ELSE
        WRITE(0,*)"ERROR in pattern: " // TRIM(tokens(t)%string)   // " " // &
                                          TRIM(tokens(t+1)%string) // " " // & 
                                          TRIM(tokens(t+2)%string)
        STOP

      END IF
    END IF
  END DO

  END SUBROUTINE TOKENIZER


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE CREATE_STACK(tokens, num_tokens, output_stack)
  use stack_mod
  IMPLICIT NONE
  INTEGER :: num_tokens
  TYPE(TokenType), DIMENSION(num_tokens) :: tokens
  TYPE(StackType) :: output_stack, oper_stack
  INTEGER :: t, priority, top_priority
  INTENT(IN) :: tokens, num_tokens
  INTENT(OUT) :: output_stack

  ! using Shunting-yard algorithm to convert list of tokens into Reverse Polish notation (RPN)
  DO t = 1, num_tokens
    IF (tokens(t)%ttype == "number" .OR. tokens(t)%ttype == "variable") THEN
      CALL output_stack%push(tokens(t)%string)
    END IF

    IF (tokens(t)%ttype == "operator") THEN
      priority = GET_PRIORITY(tokens(t)%string)
      DO
        IF (oper_stack%size() == 0)  EXIT

        top_priority = GET_PRIORITY(oper_stack%top())
        IF (priority >= top_priority .OR. oper_stack%top() == "(")  EXIT

        CALL output_stack%push(oper_stack%top())
        CALL oper_stack%pop()
      END DO

      CALL oper_stack%push(tokens(t)%string)
    END IF

    IF (tokens(t)%ttype == "left_paren") THEN
      CALL oper_stack%push(tokens(t)%string)
    END IF

    IF (tokens(t)%ttype == "right_paren") THEN
      IF (oper_stack%size() == 0) THEN
        WRITE(0,*)"unexpected closing parentheses"
        STOP
      END IF

      DO
        IF (oper_stack%top() == "(")  EXIT
        CALL output_stack%push(oper_stack%top())
        CALL oper_stack%pop()
      END DO
      IF (oper_stack%top() == "(") THEN
        CALL oper_stack%pop()
      END IF
    END IF
  END DO

  ! move operation stack onto output stack
  DO
    IF (oper_stack%size() == 0)  EXIT
    CALL output_stack%push(oper_stack%top())
    CALL oper_stack%pop()
  END DO

  END SUBROUTINE CREATE_STACK
END MODULE PARSER_MOD
