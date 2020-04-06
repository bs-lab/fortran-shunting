MODULE PARSER_MOD
INTEGER, PARAMETER :: num_opers=4
CHARACTER(LEN=1), DIMENSION(num_opers), PARAMETER :: operators = ['/',  '*',  '-', '+']
INTEGER, DIMENSION(num_opers), PARAMETER :: oper_priority = [2, 2, 1, 1]
INTEGER, PARAMETER :: mql=5000, mnt=5000

CONTAINS

  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE PRINT_TOKENS(OUNIT, tokens, num_tokens)
  IMPLICIT NONE
  INTEGER :: i, OUNIT, num_tokens
  CHARACTER(LEN=512), DIMENSION(num_tokens) :: tokens
  INTENT(IN) :: OUNIT, tokens, num_tokens

  WRITE(OUNIT, '(A)', ADVANCE='no') "tokens:"
  DO i = 1, num_tokens
    WRITE(OUNIT, '(A)', ADVANCE='no') " " // TRIM(tokens(i))
  END DO
  WRITE(OUNIT, '(A)')

  END SUBROUTINE PRINT_TOKENS


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE PRINT_STACK(OUNIT, stack, stack_size)
  IMPLICIT NONE
  INTEGER :: i, OUNIT, stack_size
  CHARACTER(LEN=512), DIMENSION(stack_size) :: stack
  INTENT(IN) :: OUNIT, stack, stack_size

  WRITE(OUNIT, '(A)', ADVANCE='no') "rpn stack:"
  DO i = 1, stack_size
    WRITE(OUNIT, '(A)', ADVANCE='no') " " // TRIM(stack(i))
  END DO
  WRITE(OUNIT, '(A)')

  END SUBROUTINE PRINT_STACK


  ! ------------------------------------------------------------------------------------------------
  FUNCTION GET_PRIORITY(oper) RESULT(out)
  IMPLICIT NONE
  INTEGER :: loc, out, i
  INTENT(IN) :: oper
  CHARACTER(LEN=1) :: oper

  loc = 0
  DO i = 1, num_opers
    IF (operators(i) == oper) THEN
      loc = i
      EXIT
    END IF
  END DO

  IF (loc < 1) THEN
    !WRITE(0,*) "ERROR: bad operator", oper
    !STOP
    out = -9999
  ELSE
    out = oper_priority(loc)
  END IF

  END FUNCTION GET_PRIORITY


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE TOKENIZER(card, tokens, num_tokens)
  IMPLICIT NONE
  CHARACTER(LEN=512), DIMENSION(mnt) :: tokens
  CHARACTER(LEN=128) :: card, num_value
  CHARACTER(LEN=1) :: curr_char
  LOGICAL :: IsNumber, exit_flag
  INTEGER :: vc, c, i, num_tokens
  INTENT(IN) :: card
  INTENT(OUT) :: tokens, num_tokens

  ! get tokens
  num_tokens = 0
  IsNumber = .FALSE.
  exit_flag = .FALSE.
  c = 0

  DO
    c = c + 1
    IF (c > LEN_TRIM(card)) THEN
      EXIT
    END IF

    curr_char = card(c:c)
    !write(0,*)"current:", curr_char
    DO i = 1, num_opers
      IF (curr_char == operators(i)) THEN
        num_tokens = num_tokens + 1
        tokens(num_tokens) = curr_char
        EXIT
      END IF
    END DO

    IF (curr_char == " ")  CYCLE

    IF (curr_char == ")" .OR. curr_char == "(") THEN
      num_tokens = num_tokens + 1
      tokens(num_tokens) = curr_char
      CYCLE
    END IF

    IF (ICHAR(curr_char) >= 48 .AND. ICHAR(curr_char) <= 57) THEN
      IsNumber = .TRUE.
      num_tokens = num_tokens + 1
      vc = 1
      num_value = ""
      num_value(vc:vc) = curr_char

      ! read until next non-number
      DO 
        c = c + 1
        IF (c > LEN_TRIM(card)) THEN
          tokens(num_tokens) = TRIM(num_value)
          EXIT
        END IF

        curr_char = card(c:c)
        !write(0,*)"  current:", curr_char, c

        IF (curr_char == " " .OR. curr_char == ")" .OR. curr_char == "(") THEN
          c = c - 1
          tokens(num_tokens) = TRIM(num_value)
          EXIT
        END IF

        DO i = 1, num_opers
          IF (curr_char == operators(i)) THEN
            c = c - 1
            tokens(num_tokens) = TRIM(num_value)
            exit_flag = .TRUE.
            EXIT
          END IF
        END DO

        IF (exit_flag) THEN
          exit_flag = .FALSE.
          EXIT
        END IF

        vc = vc + 1
        num_value(vc:vc) = curr_char
      END DO
    END IF
  END DO

  END SUBROUTINE TOKENIZER


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE PARSER(tokens, num_tokens, output_queue, out_head)
  IMPLICIT NONE
  INTEGER :: num_tokens
  CHARACTER(LEN=512), DIMENSION(num_tokens) :: tokens
  CHARACTER(LEN=512), DIMENSION(mql) :: output_queue
  CHARACTER(LEN=512), DIMENSION(mql) :: oper_stack
  LOGICAL :: IsNumeric, IsLeftParen, IsRightParen, IsOperator
  INTEGER :: i, t, priority, top_priority
  INTEGER  :: out_head, oper_head
  INTENT(IN) :: tokens, num_tokens
  INTENT(OUT) :: output_queue, out_head

  out_head = 0
  oper_head = 0

  DO t = 1, num_tokens
    !write(0,*)t,"token:", trim(tokens(t))
    IsNumeric = .FALSE.
    IsLeftParen = .FALSE.
    IsRightParen = .FALSE.
    IsOperator = .FALSE.

    IF (ICHAR(tokens(t)(1:1)) >= 48 .AND. ICHAR(tokens(t)(1:1)) <= 57) THEN
      IsNumeric = .TRUE.

    ELSEIF(tokens(t) == "(") THEN
      IsLeftParen = .TRUE.

    ELSEIF(tokens(t) == ")") THEN
      IsRightParen = .TRUE.

    ELSE
      DO i = 1, num_opers
        IF (tokens(t) == operators(i)) THEN
          IsOperator = .TRUE.
          priority = oper_priority(i)
          EXIT
        END IF
      END DO
    END IF

    IF (.NOT. IsNumeric .AND. .NOT. IsLeftParen .AND. .NOT. IsRightParen .AND. .NOT. IsOperator) THEN
      WRITE(0,*) "Unknown token type"
      STOP
    END IF

    IF (IsNumeric) THEN
      out_head = out_head + 1
      output_queue(out_head) = tokens(t)
    END IF

    IF (IsOperator) THEN
      DO
        IF (oper_head == 0)  EXIT
        top_priority = GET_PRIORITY(oper_stack(oper_head))
        !write(0,*) "current priority", priority
        !write(0,*) "top stack prior ", top_priority

        IF (priority >= top_priority .OR. oper_stack(oper_head) == "(")  EXIT

        out_head = out_head + 1
        output_queue(out_head) = oper_stack(oper_head)
        oper_head = oper_head - 1
      END DO

      oper_head = oper_head + 1
      oper_stack(oper_head) = tokens(t)
    END IF

    IF (IsLeftParen) THEN
      oper_head = oper_head + 1
      oper_stack(oper_head) = tokens(t)
    END IF

    IF (IsRightParen) THEN
      IF (oper_head == 0) THEN
        WRITE(0,*)"unexpected closing parentheses"
        STOP
      END IF

      DO
        IF (oper_stack(oper_head) == "(")  EXIT
        out_head = out_head + 1
        output_queue(out_head) = oper_stack(oper_head)
        oper_head = oper_head - 1
      END DO
      IF (oper_stack(oper_head) == "(") THEN
        oper_head = oper_head - 1
      END IF
    END IF

  END DO

  DO
    IF (oper_head == 0) EXIT
    out_head = out_head + 1
    output_queue(out_head) = oper_stack(oper_head)
    oper_head = oper_head - 1
  END DO


  END SUBROUTINE PARSER
END MODULE PARSER_MOD


! --------------------------------------------------------------------------------------------------
PROGRAM MAIN
use parser_mod
IMPLICIT NONE
CHARACTER(LEN=128) :: card
CHARACTER(LEN=512), DIMENSION(mnt) :: tokens, queue
INTEGER :: num_tokens, size_queue

  ! --------------------------------
  card(1:128) = " "
  card = " 51*(41+31) "

  CALL TOKENIZER(card, tokens, num_tokens)
  CALL PARSER(tokens, num_tokens, queue, size_queue)

  WRITE(6,'(/A)')"card: " // TRIM(card)
  CALL PRINT_TOKENS(6, tokens, num_tokens)
  CALL PRINT_STACK(6, queue, size_queue)

  ! --------------------------------
  card(1:128) = " "
  card = " 51*41+ 31 "

  CALL TOKENIZER(card, tokens, num_tokens)
  CALL PARSER(tokens, num_tokens, queue, size_queue)

  WRITE(6,'(/A)')"card: " // TRIM(card)
  CALL PRINT_TOKENS(6, tokens, num_tokens)
  CALL PRINT_STACK(6, queue, size_queue)

  ! --------------------------------
  card(1:128) = " "
  card = " ((51*41))+ 31 "

  CALL TOKENIZER(card, tokens, num_tokens)
  CALL PARSER(tokens, num_tokens, queue, size_queue)

  WRITE(6,'(/A)')"card: " // TRIM(card)
  CALL PRINT_TOKENS(6, tokens, num_tokens)
  CALL PRINT_STACK(6, queue, size_queue)

END PROGRAM MAIN
