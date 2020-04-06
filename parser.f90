PROGRAM PARSER
CHARACTER(LEN=1), DIMENSION(7), PARAMETER :: operators = ['/',  '*', ')', '(', '-', '+', '-']
INTEGER, PARAMETER :: mql=5000, mnt=5000
CHARACTER(LEN=512), DIMENSION(mql) :: queue
CHARACTER(LEN=512), DIMENSION(mnt) :: token
CHARACTER(LEN=128) :: num_value
CHARACTER(LEN=1) :: curr_char
CHARACTER(LEN=128) :: card
LOGICAL :: IsNumber, exit_flag
INTEGER :: head, vc, c, i

  card(1:128) = " "
  card = " 51*(41+31) "

  ! get tokens
  head = 0
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
    DO i = 1, 7
      IF (curr_char == operators(i)) THEN
        head = head + 1
        token(head) = curr_char
        EXIT
      END IF
    END DO

    IF (curr_char == " ") THEN
      CYCLE
    END IF

    IF (ICHAR(curr_char) >= 48 .AND. ICHAR(curr_char) <= 57) THEN
      IsNumber = .TRUE.
      head = head + 1
      vc = 1
      num_value = ""
      num_value(vc:vc) = curr_char
      !write(0,*) "  num_value", num_value(1:vc)

      ! read until next non-number
      DO 
        c = c + 1
        IF (c > LEN_TRIM(card)) THEN
          token(head) = TRIM(num_value)
          EXIT
        END IF
        curr_char = card(c:c)
        !write(0,*)"  current:", curr_char, c

        IF (curr_char == " ") THEN
          token(head) = TRIM(num_value)
          EXIT
        END IF

        DO i = 1, 7
          IF (curr_char == operators(i)) THEN
            c = c - 1
            token(head) = TRIM(num_value)
            !write(0,*)"set to true"
            exit_flag = .TRUE.
            EXIT
          END IF
        END DO

        IF (exit_flag) THEN
          exit_flag = .FALSE.
          !write(0,*)"    exiting"
          EXIT
        END IF

        vc = vc + 1
        num_value(vc:vc) = curr_char
        !write(0,*) vc, "  num_value", num_value(1:vc)

      END DO
    END IF
  END DO

  write(0,*)"card:", TRIM(card)
  DO i = 1, head
    write(0,*) i, TRIM(token(i))
  END DO

END PROGRAM PARSER
