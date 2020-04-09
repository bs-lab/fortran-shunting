PROGRAM MAIN
use parser_mod
IMPLICIT NONE
INTEGER, PARAMETER :: inn=101
CHARACTER(LEN=mlc) :: card
CHARACTER(LEN=mli), DIMENSION(mss) :: queue
CHARACTER(LEN=256) :: eqns_file
TYPE(TokenType), DIMENSION(mss) :: tokens
INTEGER :: num_tokens, size_queue, ios

  CALL GETARG(1, eqns_file)
  OPEN(UNIT=inn, FILE=eqns_file, ACTION='read', STATUS='old')

  DO
    READ(inn, '(A)', iostat=ios) card
    IF (ios < 0) EXIT
    IF (LEN_TRIM(card) == 0)  CYCLE
    CALL TOKENIZER(card, tokens, num_tokens)
    CALL CREATE_STACK(tokens, num_tokens, queue, size_queue)

    WRITE(6,'(/A)')"card: " // TRIM(card)
    CALL PRINT_TOKENS(6, tokens, num_tokens)
    CALL PRINT_STACK(6, queue, size_queue)
  END DO

  CLOSE(inn)

END PROGRAM MAIN
