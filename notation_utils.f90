MODULE Notation_Utils
    USE Chess_Types
    USE Board_Utils, ONLY: is_in_check
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: move_to_san, move_to_coordinate, move_matches_input, normalize_move_text, &
              write_pgn_file, to_lower_string

CONTAINS

    PURE CHARACTER(LEN=1) FUNCTION file_to_char(file_idx) RESULT(ch)
        INTEGER, INTENT(IN) :: file_idx
        ch = CHAR(ICHAR('a') + file_idx - 1)
    END FUNCTION file_to_char

    PURE CHARACTER(LEN=1) FUNCTION rank_to_char(rank_idx) RESULT(ch)
        INTEGER, INTENT(IN) :: rank_idx
        ch = CHAR(ICHAR('0') + rank_idx)
    END FUNCTION rank_to_char

    PURE CHARACTER(LEN=1) FUNCTION piece_to_san_char(piece) RESULT(ch)
        INTEGER, INTENT(IN) :: piece
        SELECT CASE(piece)
        CASE(KNIGHT)
            ch = 'N'
        CASE(BISHOP)
            ch = 'B'
        CASE(ROOK)
            ch = 'R'
        CASE(QUEEN)
            ch = 'Q'
        CASE(KING)
            ch = 'K'
        CASE DEFAULT
            ch = ' '
        END SELECT
    END FUNCTION piece_to_san_char

    PURE CHARACTER(LEN=1) FUNCTION promotion_to_coord_char(piece) RESULT(ch)
        INTEGER, INTENT(IN) :: piece
        SELECT CASE(piece)
        CASE(QUEEN)
            ch = 'q'
        CASE(ROOK)
            ch = 'r'
        CASE(BISHOP)
            ch = 'b'
        CASE(KNIGHT)
            ch = 'n'
        CASE DEFAULT
            ch = 'q'
        END SELECT
    END FUNCTION promotion_to_coord_char

    PURE FUNCTION to_lower_string(text) RESULT(lowered)
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(LEN=LEN(text)) :: lowered
        INTEGER :: i, code

        lowered = text
        DO i = 1, LEN(text)
            code = IACHAR(lowered(i:i))
            IF (code >= IACHAR('A') .AND. code <= IACHAR('Z')) THEN
                lowered(i:i) = ACHAR(code + 32)
            END IF
        END DO
    END FUNCTION to_lower_string

    PURE LOGICAL FUNCTION moves_match(m1, m2)
        TYPE(Move_Type), INTENT(IN) :: m1, m2

        moves_match = (m1%from_sq%rank == m2%from_sq%rank .AND. &
                       m1%from_sq%file == m2%from_sq%file .AND. &
                       m1%to_sq%rank == m2%to_sq%rank .AND. &
                       m1%to_sq%file == m2%to_sq%file .AND. &
                       m1%promotion_piece == m2%promotion_piece)
    END FUNCTION moves_match

    PURE LOGICAL FUNCTION is_capture_move(mv) RESULT(is_capture)
        TYPE(Move_Type), INTENT(IN) :: mv

        is_capture = (mv%captured_piece /= NO_PIECE .OR. mv%is_en_passant)
    END FUNCTION is_capture_move

    FUNCTION move_to_coordinate(mv) RESULT(coord)
        TYPE(Move_Type), INTENT(IN) :: mv
        CHARACTER(LEN=8) :: coord

        coord = '        '
        coord(1:1) = file_to_char(mv%from_sq%file)
        coord(2:2) = rank_to_char(mv%from_sq%rank)
        coord(3:3) = file_to_char(mv%to_sq%file)
        coord(4:4) = rank_to_char(mv%to_sq%rank)
        IF (mv%promotion_piece /= NO_PIECE) THEN
            coord(5:5) = promotion_to_coord_char(mv%promotion_piece)
        END IF
    END FUNCTION move_to_coordinate

    FUNCTION move_to_san(board, mv, legal_moves, num_legal_moves) RESULT(san)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), INTENT(IN) :: mv
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        CHARACTER(LEN=32) :: san

        TYPE(Board_Type) :: temp_board
        TYPE(UnmakeInfo_Type) :: unmake_info
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: reply_moves
        INTEGER :: num_reply_moves
        INTEGER :: piece_moved, i
        LOGICAL :: is_capture, ambiguous, same_file, same_rank
        CHARACTER(LEN=1) :: piece_char, promo_char
        CHARACTER(LEN=2) :: destination

        san = ''
        piece_moved = board%squares_piece(mv%from_sq%rank, mv%from_sq%file)

        IF (mv%is_castling) THEN
            IF (mv%to_sq%file == 7) THEN
                san = 'O-O'
            ELSE
                san = 'O-O-O'
            END IF
        ELSE
            is_capture = (mv%captured_piece /= NO_PIECE .OR. mv%is_en_passant)
            destination(1:1) = file_to_char(mv%to_sq%file)
            destination(2:2) = rank_to_char(mv%to_sq%rank)

            IF (piece_moved == PAWN) THEN
                IF (is_capture) san = TRIM(san) // file_to_char(mv%from_sq%file)
            ELSE
                piece_char = piece_to_san_char(piece_moved)
                san = TRIM(san) // piece_char

                ambiguous = .FALSE.
                same_file = .FALSE.
                same_rank = .FALSE.
                DO i = 1, num_legal_moves
                    IF (moves_match(legal_moves(i), mv)) CYCLE
                    IF (legal_moves(i)%to_sq%rank /= mv%to_sq%rank .OR. &
                        legal_moves(i)%to_sq%file /= mv%to_sq%file) CYCLE
                    IF (board%squares_piece(legal_moves(i)%from_sq%rank, legal_moves(i)%from_sq%file) /= piece_moved) CYCLE
                    ambiguous = .TRUE.
                    IF (legal_moves(i)%from_sq%file == mv%from_sq%file) same_file = .TRUE.
                    IF (legal_moves(i)%from_sq%rank == mv%from_sq%rank) same_rank = .TRUE.
                END DO

                IF (ambiguous) THEN
                    IF (.NOT. same_file) THEN
                        san = TRIM(san) // file_to_char(mv%from_sq%file)
                    ELSE IF (.NOT. same_rank) THEN
                        san = TRIM(san) // rank_to_char(mv%from_sq%rank)
                    ELSE
                        san = TRIM(san) // file_to_char(mv%from_sq%file) // rank_to_char(mv%from_sq%rank)
                    END IF
                END IF
            END IF

            IF (is_capture) san = TRIM(san) // 'x'
            san = TRIM(san) // destination

            IF (mv%promotion_piece /= NO_PIECE) THEN
                promo_char = piece_to_san_char(mv%promotion_piece)
                san = TRIM(san) // '=' // promo_char
            END IF
        END IF

        temp_board = board
        CALL make_move(temp_board, mv, unmake_info)
        CALL generate_moves(temp_board, reply_moves, num_reply_moves)
        IF (is_in_check(temp_board, temp_board%current_player)) THEN
            IF (num_reply_moves == 0) THEN
                san = TRIM(san) // '#'
            ELSE
                san = TRIM(san) // '+'
            END IF
        END IF
    END FUNCTION move_to_san

    FUNCTION normalize_move_text(raw_text) RESULT(normalized)
        CHARACTER(LEN=*), INTENT(IN) :: raw_text
        CHARACTER(LEN=32) :: normalized
        CHARACTER(LEN=64) :: lowered
        INTEGER :: i, start_pos, out_pos

        lowered = ADJUSTL(TRIM(to_lower_string(raw_text)))
        DO i = 1, LEN_TRIM(lowered)
            IF (lowered(i:i) == '0') lowered(i:i) = 'o'
        END DO

        start_pos = 1
        DO WHILE (start_pos <= LEN_TRIM(lowered))
            IF ((lowered(start_pos:start_pos) >= '0' .AND. lowered(start_pos:start_pos) <= '9') .OR. &
                lowered(start_pos:start_pos) == '.') THEN
                start_pos = start_pos + 1
            ELSE
                EXIT
            END IF
        END DO

        normalized = ''
        out_pos = 0
        DO i = start_pos, LEN_TRIM(lowered)
            SELECT CASE (lowered(i:i))
            CASE ('x', '+', '#', '=', '!', '?', ' ')
                CYCLE
            CASE DEFAULT
                out_pos = out_pos + 1
                IF (out_pos <= LEN(normalized)) normalized(out_pos:out_pos) = lowered(i:i)
            END SELECT
        END DO
    END FUNCTION normalize_move_text

    LOGICAL FUNCTION move_matches_input(board, mv, legal_moves, num_legal_moves, raw_input) RESULT(matches)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), INTENT(IN) :: mv
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        CHARACTER(LEN=*), INTENT(IN) :: raw_input

        CHARACTER(LEN=32) :: normalized_input, normalized_san, normalized_coord
        INTEGER :: i, shorthand_match_count
        CHARACTER(LEN=1) :: from_file_char, to_file_char

        normalized_input = normalize_move_text(raw_input)
        normalized_san = normalize_move_text(move_to_san(board, mv, legal_moves, num_legal_moves))
        normalized_coord = normalize_move_text(move_to_coordinate(mv))

        matches = (TRIM(normalized_input) == TRIM(normalized_san) .OR. &
                   TRIM(normalized_input) == TRIM(normalized_coord))
        IF (matches) RETURN

        IF (LEN_TRIM(normalized_input) /= 2) RETURN
        from_file_char = normalized_input(1:1)
        to_file_char = normalized_input(2:2)
        IF (from_file_char < 'a' .OR. from_file_char > 'h') RETURN
        IF (to_file_char < 'a' .OR. to_file_char > 'h') RETURN
        IF (from_file_char == to_file_char) RETURN

        IF (board%squares_piece(mv%from_sq%rank, mv%from_sq%file) /= PAWN) RETURN
        IF (.NOT. is_capture_move(mv)) RETURN
        IF (file_to_char(mv%from_sq%file) /= from_file_char) RETURN
        IF (file_to_char(mv%to_sq%file) /= to_file_char) RETURN

        shorthand_match_count = 0
        DO i = 1, num_legal_moves
            IF (board%squares_piece(legal_moves(i)%from_sq%rank, legal_moves(i)%from_sq%file) /= PAWN) CYCLE
            IF (.NOT. is_capture_move(legal_moves(i))) CYCLE
            IF (file_to_char(legal_moves(i)%from_sq%file) /= from_file_char) CYCLE
            IF (file_to_char(legal_moves(i)%to_sq%file) /= to_file_char) CYCLE
            shorthand_match_count = shorthand_match_count + 1
        END DO
        matches = (shorthand_match_count == 1)
    END FUNCTION move_matches_input

    SUBROUTINE write_pgn_file(filename, move_history, num_half_moves, result_text, move_elapsed_ms_history, time_control_tag, &
        event_name, white_name, black_name)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        INTEGER, INTENT(IN) :: num_half_moves
        CHARACTER(LEN=*), INTENT(IN) :: result_text
        INTEGER(KIND=8), DIMENSION(:), INTENT(IN), OPTIONAL :: move_elapsed_ms_history
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: time_control_tag
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: event_name, white_name, black_name

        INTEGER :: unit_no, i, values(8), current_len, token_len
        CHARACTER(LEN=10) :: date_tag
        CHARACTER(LEN=256) :: current_line, token_text
        CHARACTER(LEN=128) :: local_event_name, local_white_name, local_black_name

        CALL DATE_AND_TIME(VALUES=values)
        WRITE(date_tag, '(I4.4,A,I2.2,A,I2.2)') values(1), '.', values(2), '.', values(3)

        local_event_name = 'FortranChess Console Game'
        local_white_name = 'Human'
        local_black_name = 'Computer'
        IF (PRESENT(event_name)) local_event_name = TRIM(event_name)
        IF (PRESENT(white_name)) local_white_name = TRIM(white_name)
        IF (PRESENT(black_name)) local_black_name = TRIM(black_name)

        unit_no = 37
        OPEN(unit=unit_no, file=filename, status='UNKNOWN', action='WRITE', position='APPEND')
        WRITE(unit_no, '(A,A,A)') '[Event "', TRIM(local_event_name), '"]'
        WRITE(unit_no, '(A)') '[Site "?"]'
        WRITE(unit_no, '(A,A,A)') '[Date "', TRIM(date_tag), '"]'
        WRITE(unit_no, '(A)') '[Round "?"]'
        WRITE(unit_no, '(A,A,A)') '[White "', TRIM(local_white_name), '"]'
        WRITE(unit_no, '(A,A,A)') '[Black "', TRIM(local_black_name), '"]'
        WRITE(unit_no, '(A,A,A)') '[Result "', TRIM(result_text), '"]'
        IF (PRESENT(time_control_tag)) THEN
            WRITE(unit_no, '(A,A,A)') '[TimeControl "', TRIM(time_control_tag), '"]'
        END IF
        WRITE(unit_no, '(A)') ''

        current_line = ''
        current_len = 0
        DO i = 1, num_half_moves, 2
            WRITE(token_text, '(I0,A,A)') (i + 1) / 2, '. ', TRIM(annotated_move_text(move_history(i), i, move_elapsed_ms_history))
            IF (i + 1 <= num_half_moves) THEN
                token_text = TRIM(token_text) // ' ' // TRIM(annotated_move_text(move_history(i + 1), i + 1, move_elapsed_ms_history))
            END IF
            token_len = LEN_TRIM(token_text)

            IF (current_len == 0) THEN
                current_line = TRIM(token_text)
                current_len = token_len
            ELSE IF (current_len + 1 + token_len > 100) THEN
                WRITE(unit_no, '(A)') TRIM(current_line)
                current_line = TRIM(token_text)
                current_len = token_len
            ELSE
                current_line = TRIM(current_line) // ' ' // TRIM(token_text)
                current_len = current_len + 1 + token_len
            END IF
        END DO

        token_text = TRIM(result_text)
        token_len = LEN_TRIM(token_text)
        IF (current_len == 0) THEN
            current_line = TRIM(token_text)
        ELSE IF (current_len + 1 + token_len > 100) THEN
            WRITE(unit_no, '(A)') TRIM(current_line)
            current_line = TRIM(token_text)
        ELSE
            current_line = TRIM(current_line) // ' ' // TRIM(token_text)
        END IF
        WRITE(unit_no, '(A)') TRIM(current_line)
        WRITE(unit_no, '(A)') ''
        WRITE(unit_no, '(A)') ''
        CLOSE(unit_no)
    END SUBROUTINE write_pgn_file

    FUNCTION annotated_move_text(move_text, move_index, move_elapsed_ms_history) RESULT(text)
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        INTEGER, INTENT(IN) :: move_index
        INTEGER(KIND=8), DIMENSION(:), INTENT(IN), OPTIONAL :: move_elapsed_ms_history
        CHARACTER(LEN=96) :: text

        text = TRIM(move_text)
        IF (.NOT. PRESENT(move_elapsed_ms_history)) RETURN
        IF (move_index > SIZE(move_elapsed_ms_history)) RETURN
        IF (move_elapsed_ms_history(move_index) < 0_8) RETURN

        text = TRIM(move_text) // ' {' // TRIM(format_elapsed_time(move_elapsed_ms_history(move_index))) // '}'
    END FUNCTION annotated_move_text

    FUNCTION format_elapsed_time(elapsed_ms) RESULT(text)
        INTEGER(KIND=8), INTENT(IN) :: elapsed_ms
        CHARACTER(LEN=16) :: text
        INTEGER(KIND=8) :: total_seconds, hours, minutes, seconds

        total_seconds = MAX(0_8, (elapsed_ms + 500_8) / 1000_8)
        hours = total_seconds / 3600_8
        minutes = MOD(total_seconds, 3600_8) / 60_8
        seconds = MOD(total_seconds, 60_8)
        IF (hours > 0_8) THEN
            WRITE(text, '(I0,A,I2.2,A,I2.2,A)') hours, 'h', minutes, 'm', seconds, 's'
        ELSE IF (minutes > 0_8) THEN
            WRITE(text, '(I0,A,I2.2,A)') minutes, 'm', seconds, 's'
        ELSE
            WRITE(text, '(I0,A)') seconds, 's'
        END IF
    END FUNCTION format_elapsed_time

END MODULE Notation_Utils
