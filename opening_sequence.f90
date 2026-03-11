MODULE Opening_Sequence
    USE Chess_Types
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move
    USE Notation_Utils, ONLY: move_matches_input, move_to_san
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: load_opening_sequence

CONTAINS

    SUBROUTINE load_opening_sequence(filename, board, move_history, num_half_moves, loaded_ok, error_message, &
        move_stack, unmake_stack, position_key_history)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: move_history
        INTEGER, INTENT(INOUT) :: num_half_moves
        LOGICAL, INTENT(OUT) :: loaded_ok
        CHARACTER(LEN=*), INTENT(OUT) :: error_message
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT), OPTIONAL :: move_stack
        TYPE(UnmakeInfo_Type), DIMENSION(:), INTENT(INOUT), OPTIONAL :: unmake_stack
        INTEGER(KIND=8), DIMENSION(:), INTENT(INOUT), OPTIONAL :: position_key_history

        INTEGER, PARAMETER :: MAX_LINE_TOKENS = 64
        INTEGER :: unit_no, ios, line_no, token_idx, i
        CHARACTER(LEN=256) :: line, trimmed_line
        CHARACTER(LEN=64), DIMENSION(MAX_LINE_TOKENS) :: tokens
        INTEGER :: num_tokens, num_legal_moves
        LOGICAL :: exists, found_move, in_brace_comment
        INTEGER :: paren_depth
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: move_info
        CHARACTER(LEN=32) :: san_text

        loaded_ok = .FALSE.
        error_message = ''
        INQUIRE(file=filename, exist=exists)
        IF (.NOT. exists) THEN
            error_message = 'Opening sequence file not found.'
            RETURN
        END IF

        unit_no = 38
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            error_message = 'Could not open opening sequence file.'
            RETURN
        END IF

        line_no = 0
        in_brace_comment = .FALSE.
        paren_depth = 0

        DO
            READ(unit_no, '(A)', IOSTAT=ios) line
            IF (ios < 0) EXIT
            IF (ios /= 0) THEN
                error_message = 'Error while reading opening sequence file.'
                CLOSE(unit_no)
                RETURN
            END IF

            line_no = line_no + 1
            trimmed_line = ADJUSTL(line)
            IF (.NOT. in_brace_comment .AND. paren_depth == 0) THEN
                IF (LEN_TRIM(trimmed_line) == 0) CYCLE
                IF (trimmed_line(1:1) == '[') CYCLE
            END IF

            CALL tokenize_pgn_line(line, tokens, num_tokens, in_brace_comment, paren_depth)
            DO token_idx = 1, num_tokens
                IF (should_ignore_token(tokens(token_idx))) CYCLE

                CALL generate_moves(board, legal_moves, num_legal_moves)
                found_move = .FALSE.
                DO i = 1, num_legal_moves
                    IF (move_matches_input(board, legal_moves(i), legal_moves, num_legal_moves, tokens(token_idx))) THEN
                        chosen_move = legal_moves(i)
                        found_move = .TRUE.
                        EXIT
                    END IF
                END DO

                IF (.NOT. found_move) THEN
                    WRITE(error_message, '(A,A,A,I0,A)') 'Could not apply token "', TRIM(tokens(token_idx)), &
                        '" on line ', line_no, '.'
                    CLOSE(unit_no)
                    RETURN
                END IF

                IF (num_half_moves >= SIZE(move_history)) THEN
                    error_message = 'Opening sequence is too long for move history storage.'
                    CLOSE(unit_no)
                    RETURN
                END IF

                san_text = move_to_san(board, chosen_move, legal_moves, num_legal_moves)
                num_half_moves = num_half_moves + 1
                move_history(num_half_moves) = TRIM(san_text)
                CALL make_move(board, chosen_move, move_info)
                IF (PRESENT(move_stack)) move_stack(num_half_moves) = chosen_move
                IF (PRESENT(unmake_stack)) unmake_stack(num_half_moves) = move_info
                IF (PRESENT(position_key_history)) position_key_history(num_half_moves + 1) = board%zobrist_key
            END DO
        END DO

        CLOSE(unit_no)

        IF (in_brace_comment) THEN
            error_message = 'Opening sequence has an unclosed { comment }.'
            RETURN
        END IF
        IF (paren_depth /= 0) THEN
            error_message = 'Opening sequence has an unclosed ( variation ).'
            RETURN
        END IF

        loaded_ok = .TRUE.
    END SUBROUTINE load_opening_sequence

    SUBROUTINE tokenize_pgn_line(line, tokens, num_tokens, in_brace_comment, paren_depth)
        CHARACTER(LEN=*), INTENT(IN) :: line
        CHARACTER(LEN=*), DIMENSION(:), INTENT(OUT) :: tokens
        INTEGER, INTENT(OUT) :: num_tokens
        LOGICAL, INTENT(INOUT) :: in_brace_comment
        INTEGER, INTENT(INOUT) :: paren_depth

        INTEGER :: i, token_len
        CHARACTER(LEN=1) :: ch
        CHARACTER(LEN=64) :: token

        num_tokens = 0
        token = ''
        token_len = 0

        DO i = 1, LEN_TRIM(line)
            ch = line(i:i)

            IF (in_brace_comment) THEN
                IF (ch == '}') in_brace_comment = .FALSE.
                CYCLE
            END IF

            IF (paren_depth > 0) THEN
                IF (ch == '(') THEN
                    paren_depth = paren_depth + 1
                ELSE IF (ch == ')') THEN
                    paren_depth = paren_depth - 1
                END IF
                CYCLE
            END IF

            IF (ch == ';') EXIT

            IF (ch == '{') THEN
                CALL flush_token(token, token_len, tokens, num_tokens)
                in_brace_comment = .TRUE.
                CYCLE
            END IF

            IF (ch == '(') THEN
                CALL flush_token(token, token_len, tokens, num_tokens)
                paren_depth = paren_depth + 1
                CYCLE
            END IF

            IF (ch == ' ' .OR. IACHAR(ch) == 9) THEN
                CALL flush_token(token, token_len, tokens, num_tokens)
                CYCLE
            END IF

            IF (token_len < LEN(token)) THEN
                token_len = token_len + 1
                token(token_len:token_len) = ch
            END IF
        END DO

        CALL flush_token(token, token_len, tokens, num_tokens)
    END SUBROUTINE tokenize_pgn_line

    SUBROUTINE flush_token(token, token_len, tokens, num_tokens)
        CHARACTER(LEN=*), INTENT(INOUT) :: token
        INTEGER, INTENT(INOUT) :: token_len
        CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: tokens
        INTEGER, INTENT(INOUT) :: num_tokens

        IF (token_len <= 0) RETURN
        IF (num_tokens < SIZE(tokens)) THEN
            num_tokens = num_tokens + 1
            tokens(num_tokens) = token
        END IF
        token = ''
        token_len = 0
    END SUBROUTINE flush_token

    LOGICAL FUNCTION should_ignore_token(token)
        CHARACTER(LEN=*), INTENT(IN) :: token
        CHARACTER(LEN=64) :: trimmed

        trimmed = TRIM(ADJUSTL(token))
        should_ignore_token = .FALSE.
        IF (LEN_TRIM(trimmed) == 0) THEN
            should_ignore_token = .TRUE.
        ELSE IF (trimmed == '*') THEN
            should_ignore_token = .TRUE.
        ELSE IF (trimmed == '1-0' .OR. trimmed == '0-1' .OR. trimmed == '1/2-1/2') THEN
            should_ignore_token = .TRUE.
        ELSE IF (trimmed(1:1) == '$') THEN
            should_ignore_token = .TRUE.
        ELSE IF (is_move_number_token(trimmed)) THEN
            should_ignore_token = .TRUE.
        END IF
    END FUNCTION should_ignore_token

    LOGICAL FUNCTION is_move_number_token(token)
        CHARACTER(LEN=*), INTENT(IN) :: token
        INTEGER :: i

        is_move_number_token = .TRUE.
        DO i = 1, LEN_TRIM(token)
            IF (.NOT. ((token(i:i) >= '0' .AND. token(i:i) <= '9') .OR. token(i:i) == '.')) THEN
                is_move_number_token = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION is_move_number_token

END MODULE Opening_Sequence
