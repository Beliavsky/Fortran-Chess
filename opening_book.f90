MODULE Opening_Book
    USE Chess_Types
    USE Notation_Utils, ONLY: normalize_move_text, move_matches_input
    USE Board_Utils, ONLY: init_board
    USE Make_Unmake, ONLY: make_move
    USE Move_Generation, ONLY: generate_moves
    USE Transposition_Table, ONLY: init_zobrist_keys, ZOBRIST_EP_FILE
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: Opening_Book_Type, load_opening_book, choose_book_move, apply_opening_book_fix

    INTEGER, PARAMETER :: MAX_BOOK_LINES = 512
    INTEGER, PARAMETER :: MAX_BOOK_MOVES = 256
    INTEGER, PARAMETER :: MAX_BOOK_CANDIDATES = 64

    TYPE :: Opening_Book_Type
        LOGICAL :: found = .FALSE.
        LOGICAL :: valid = .FALSE.
        INTEGER :: num_lines = 0
        INTEGER :: error_line = 0
        CHARACTER(LEN=260) :: filename = ''
        CHARACTER(LEN=256) :: error_message = ''
        CHARACTER(LEN=256) :: suggestion = ''
        CHARACTER(LEN=512) :: error_text = ''
        CHARACTER(LEN=512) :: replacement_line = ''
        INTEGER, ALLOCATABLE, DIMENSION(:) :: line_lengths
        INTEGER, ALLOCATABLE, DIMENSION(:) :: source_line_numbers
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:, :) :: lines
        INTEGER :: num_positions = 0
        INTEGER(KIND=8), ALLOCATABLE, DIMENSION(:) :: position_keys
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:) :: position_moves
    END TYPE Opening_Book_Type

CONTAINS

    SUBROUTINE load_opening_book(filename, book)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(Opening_Book_Type), INTENT(OUT) :: book

        INTEGER :: unit_no, ios, line_no, prev_len, entry_len, base_len, line_start, next_expected
        CHARACTER(LEN=512) :: raw_line, trimmed_line
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES) :: prev_sequence, parsed_moves, full_sequence
        INTEGER, ALLOCATABLE, DIMENSION(:) :: temp_lengths, temp_source_lines
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:, :) :: temp_lines
        INTEGER :: parsed_count
        CHARACTER(LEN=256) :: err_msg, suggestion
        CHARACTER(LEN=512) :: replacement_line
        LOGICAL :: exists

        book%found = .FALSE.
        book%valid = .FALSE.
        book%num_lines = 0
        book%error_line = 0
        book%filename = filename
        book%error_message = ''
        book%suggestion = ''
        book%error_text = ''
        book%replacement_line = ''
        book%num_positions = 0
        prev_len = 0
        prev_sequence = ''

        INQUIRE(FILE=filename, EXIST=exists)
        IF (.NOT. exists) THEN
            book%found = .FALSE.
            RETURN
        END IF

        book%found = .TRUE.
        ALLOCATE(temp_lengths(MAX_BOOK_LINES))
        ALLOCATE(temp_source_lines(MAX_BOOK_LINES))
        ALLOCATE(temp_lines(MAX_BOOK_MOVES, MAX_BOOK_LINES))
        temp_lengths = 0
        temp_source_lines = 0
        temp_lines = ''
        unit_no = 41
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            book%error_message = 'Could not open opening book file.'
            book%suggestion = 'Check the file path and permissions.'
            DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
            RETURN
        END IF

        line_no = 0
        DO
            READ(unit_no, '(A)', IOSTAT=ios) raw_line
            IF (ios /= 0) EXIT
            line_no = line_no + 1
            trimmed_line = ADJUSTL(TRIM(raw_line))
            IF (LEN_TRIM(trimmed_line) == 0) CYCLE
            IF (trimmed_line(1:1) == '!' .OR. trimmed_line(1:1) == '#') CYCLE

            CALL parse_book_line(trimmed_line, parsed_moves, parsed_count, line_start, err_msg, suggestion, replacement_line)
            IF (LEN_TRIM(err_msg) /= 0) THEN
                book%error_line = line_no
                book%error_message = err_msg
                book%suggestion = suggestion
                book%error_text = TRIM(raw_line)
                book%replacement_line = replacement_line
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
                RETURN
            END IF

            IF (book%num_lines == 0) THEN
                IF (line_start /= 1) THEN
                    book%error_line = line_no
                    book%error_message = 'The first book line must start at move 1.'
                    book%suggestion = 'Start the line with `1.e4` or `1.d4`.'
                    book%error_text = TRIM(raw_line)
                    book%replacement_line = build_book_line(parsed_moves, parsed_count, 1)
                    CLOSE(unit_no)
                    DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
                    RETURN
                END IF
                base_len = 0
            ELSE
                next_expected = prev_len + 1
                IF (line_start == next_expected) THEN
                    base_len = prev_len
                ELSE IF (line_start >= 1 .AND. line_start <= prev_len) THEN
                    base_len = line_start - 1
                ELSE
                    book%error_line = line_no
                    book%error_message = 'Line starts too late to be a continuation or variation.'
                    book%suggestion = 'Use `' // TRIM(halfmove_marker(next_expected)) // '` to continue, or restart from an earlier move to branch.'
                    book%error_text = TRIM(raw_line)
                    book%replacement_line = build_book_line(parsed_moves, parsed_count, next_expected)
                    CLOSE(unit_no)
                    DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
                    RETURN
                END IF
            END IF

            entry_len = base_len + parsed_count
            IF (entry_len > MAX_BOOK_MOVES) THEN
                book%error_line = line_no
                book%error_message = 'Book line is too long.'
                book%suggestion = 'Increase structure depth by splitting into shorter variations.'
                book%error_text = TRIM(raw_line)
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
                RETURN
            END IF
            IF (book%num_lines >= MAX_BOOK_LINES) THEN
                book%error_line = line_no
                book%error_message = 'Too many book lines for current fixed storage.'
                book%suggestion = 'Reduce the book size or raise MAX_BOOK_LINES in opening_book.f90.'
                book%error_text = TRIM(raw_line)
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
                RETURN
            END IF

            full_sequence = ''
            IF (base_len > 0) full_sequence(1:base_len) = prev_sequence(1:base_len)
            full_sequence(base_len + 1:entry_len) = parsed_moves(1:parsed_count)

            book%num_lines = book%num_lines + 1
            temp_lengths(book%num_lines) = entry_len
            temp_source_lines(book%num_lines) = line_no
            temp_lines(:, book%num_lines) = ''
            temp_lines(1:entry_len, book%num_lines) = full_sequence(1:entry_len)

            prev_sequence = full_sequence
            prev_len = entry_len
        END DO

        CLOSE(unit_no)
        IF (book%num_lines > 0) THEN
            ALLOCATE(book%line_lengths(book%num_lines))
            ALLOCATE(book%source_line_numbers(book%num_lines))
            ALLOCATE(book%lines(MAX_BOOK_MOVES, book%num_lines))
            book%line_lengths = temp_lengths(1:book%num_lines)
            book%source_line_numbers = temp_source_lines(1:book%num_lines)
            book%lines = temp_lines(:, 1:book%num_lines)
            CALL build_position_index(book)
        END IF
        DEALLOCATE(temp_lengths, temp_source_lines, temp_lines)
    END SUBROUTINE load_opening_book

    SUBROUTINE apply_opening_book_fix(book, success, error_message)
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        LOGICAL, INTENT(OUT) :: success
        CHARACTER(LEN=*), INTENT(OUT) :: error_message

        INTEGER :: unit_no, ios, line_count, i
        CHARACTER(LEN=512) :: raw_line
        CHARACTER(LEN=512), ALLOCATABLE, DIMENSION(:) :: file_lines

        success = .FALSE.
        error_message = ''

        IF (.NOT. book%found) THEN
            error_message = 'Opening book file was not found.'
            RETURN
        END IF
        IF (book%error_line <= 0) THEN
            error_message = 'No editable error line is available.'
            RETURN
        END IF
        IF (LEN_TRIM(book%replacement_line) == 0) THEN
            error_message = 'No automatic fix is available for this error.'
            RETURN
        END IF

        unit_no = 42
        OPEN(unit=unit_no, file=book%filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            error_message = 'Could not reopen the opening book for reading.'
            RETURN
        END IF

        line_count = 0
        DO
            READ(unit_no, '(A)', IOSTAT=ios) raw_line
            IF (ios /= 0) EXIT
            line_count = line_count + 1
        END DO
        CLOSE(unit_no)

        IF (book%error_line > line_count) THEN
            error_message = 'The saved error line no longer exists in the file.'
            RETURN
        END IF

        ALLOCATE(file_lines(line_count))
        OPEN(unit=unit_no, file=book%filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            error_message = 'Could not reopen the opening book for editing.'
            DEALLOCATE(file_lines)
            RETURN
        END IF

        DO i = 1, line_count
            READ(unit_no, '(A)', IOSTAT=ios) file_lines(i)
            IF (ios /= 0) THEN
                error_message = 'Could not read the full opening book while applying the fix.'
                CLOSE(unit_no)
                DEALLOCATE(file_lines)
                RETURN
            END IF
        END DO
        CLOSE(unit_no)

        file_lines(book%error_line) = TRIM(book%replacement_line)

        OPEN(unit=unit_no, file=book%filename, status='REPLACE', action='WRITE', iostat=ios)
        IF (ios /= 0) THEN
            error_message = 'Could not overwrite the opening book with the proposed fix.'
            DEALLOCATE(file_lines)
            RETURN
        END IF

        DO i = 1, line_count
            WRITE(unit_no, '(A)', IOSTAT=ios) TRIM(file_lines(i))
            IF (ios /= 0) THEN
                error_message = 'Failed while writing the updated opening book.'
                CLOSE(unit_no)
                DEALLOCATE(file_lines)
                RETURN
            END IF
        END DO
        CLOSE(unit_no)
        DEALLOCATE(file_lines)
        success = .TRUE.
    END SUBROUTINE apply_opening_book_fix

    SUBROUTINE build_position_index(book)
        TYPE(Opening_Book_Type), INTENT(INOUT) :: book

        TYPE(Board_Type) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: move_info
        INTEGER(KIND=8), ALLOCATABLE, DIMENSION(:) :: temp_keys
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:) :: temp_moves
        INTEGER :: total_entries, i, j, num_legal_moves, entry_idx, move_idx
        LOGICAL :: found_move

        book%valid = .FALSE.
        book%num_positions = 0
        IF (book%num_lines <= 0) THEN
            book%valid = .TRUE.
            RETURN
        END IF

        CALL init_zobrist_keys()
        total_entries = SUM(book%line_lengths)
        IF (total_entries <= 0) THEN
            book%valid = .TRUE.
            RETURN
        END IF

        ALLOCATE(temp_keys(total_entries))
        ALLOCATE(temp_moves(total_entries))
        temp_keys = 0_8
        temp_moves = ''
        entry_idx = 0

        DO i = 1, book%num_lines
            CALL init_board(board)
            DO j = 1, book%line_lengths(i)
                CALL generate_moves(board, legal_moves, num_legal_moves)
                found_move = .FALSE.
                DO move_idx = 1, num_legal_moves
                    IF (move_matches_input(board, legal_moves(move_idx), legal_moves, num_legal_moves, book%lines(j, i))) THEN
                        chosen_move = legal_moves(move_idx)
                        found_move = .TRUE.
                        EXIT
                    END IF
                END DO

                IF (.NOT. found_move) EXIT

                entry_idx = entry_idx + 1
                temp_keys(entry_idx) = book_position_key(board)
                temp_moves(entry_idx) = book%lines(j, i)
                CALL make_move(board, chosen_move, move_info)
            END DO
        END DO

        IF (entry_idx > 0) THEN
            ALLOCATE(book%position_keys(entry_idx))
            ALLOCATE(book%position_moves(entry_idx))
            book%position_keys = temp_keys(1:entry_idx)
            book%position_moves = temp_moves(1:entry_idx)
        END IF
        book%num_positions = entry_idx
        book%valid = .TRUE.
        DEALLOCATE(temp_keys, temp_moves)
    END SUBROUTINE build_position_index

    LOGICAL FUNCTION choose_book_move(book, move_history, num_half_moves, board, legal_moves, num_legal_moves, chosen_move, chosen_text) RESULT(found)
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        INTEGER, INTENT(IN) :: num_half_moves
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        TYPE(Move_Type), INTENT(OUT) :: chosen_move
        CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: chosen_text

        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_CANDIDATES) :: candidates
        CHARACTER(LEN=32) :: next_move_text
        INTEGER :: i, j, candidate_count, selected_index
        REAL :: rand_val
        LOGICAL :: duplicate

        found = .FALSE.
        candidates = ''
        candidate_count = 0
        chosen_move%from_sq%rank = 0
        chosen_move%from_sq%file = 0
        chosen_move%to_sq%rank = 0
        chosen_move%to_sq%file = 0
        IF (.NOT. book%valid) RETURN
        IF (book%num_positions <= 0) RETURN
        IF (num_half_moves < 0) RETURN
        IF (SIZE(move_history) < 0) RETURN

        DO i = 1, book%num_positions
            IF (book%position_keys(i) /= book_position_key(board)) CYCLE
            next_move_text = book%position_moves(i)
            duplicate = .FALSE.
            DO j = 1, candidate_count
                IF (TRIM(candidates(j)) == TRIM(next_move_text)) THEN
                    duplicate = .TRUE.
                    EXIT
                END IF
            END DO
            IF (.NOT. duplicate) THEN
                IF (candidate_count >= MAX_BOOK_CANDIDATES) EXIT
                candidate_count = candidate_count + 1
                candidates(candidate_count) = next_move_text
            END IF
        END DO

        IF (candidate_count == 0) RETURN

        CALL RANDOM_NUMBER(rand_val)
        selected_index = 1 + INT(rand_val * REAL(candidate_count))
        IF (selected_index > candidate_count) selected_index = candidate_count

        DO i = 1, num_legal_moves
            IF (move_matches_input(board, legal_moves(i), legal_moves, num_legal_moves, candidates(selected_index))) THEN
                chosen_move = legal_moves(i)
                found = .TRUE.
                IF (PRESENT(chosen_text)) chosen_text = candidates(selected_index)
                RETURN
            END IF
        END DO

        DO j = 1, candidate_count
            DO i = 1, num_legal_moves
                IF (move_matches_input(board, legal_moves(i), legal_moves, num_legal_moves, candidates(j))) THEN
                    chosen_move = legal_moves(i)
                    found = .TRUE.
                    IF (PRESENT(chosen_text)) chosen_text = candidates(j)
                    RETURN
                END IF
            END DO
        END DO
    END FUNCTION choose_book_move

    INTEGER(KIND=8) FUNCTION book_position_key(board) RESULT(key)
        TYPE(Board_Type), INTENT(IN) :: board

        key = board%zobrist_key
        IF (board%ep_target_present .AND. .NOT. en_passant_capture_available(board)) THEN
            key = IEOR(key, ZOBRIST_EP_FILE(board%ep_target_sq%file))
        END IF
    END FUNCTION book_position_key

    LOGICAL FUNCTION en_passant_capture_available(board) RESULT(is_available)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: pawn_rank, test_file

        is_available = .FALSE.
        IF (.NOT. board%ep_target_present) RETURN

        IF (board%current_player == WHITE) THEN
            pawn_rank = board%ep_target_sq%rank - 1
        ELSE
            pawn_rank = board%ep_target_sq%rank + 1
        END IF

        IF (pawn_rank < 1 .OR. pawn_rank > BOARD_SIZE) RETURN
        DO test_file = MAX(1, board%ep_target_sq%file - 1), MIN(BOARD_SIZE, board%ep_target_sq%file + 1)
            IF (test_file == board%ep_target_sq%file) CYCLE
            IF (board%squares_piece(pawn_rank, test_file) /= PAWN) CYCLE
            IF (board%squares_color(pawn_rank, test_file) /= board%current_player) CYCLE
            is_available = .TRUE.
            RETURN
        END DO
    END FUNCTION en_passant_capture_available

    LOGICAL FUNCTION history_matches_prefix(book_line, line_len, move_history, num_half_moves)
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES), INTENT(IN) :: book_line
        INTEGER, INTENT(IN) :: line_len, num_half_moves
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history

        INTEGER :: i

        history_matches_prefix = .FALSE.
        IF (num_half_moves > line_len) RETURN
        DO i = 1, num_half_moves
            IF (TRIM(book_line(i)) /= TRIM(normalize_move_text(move_history(i)))) RETURN
        END DO
        history_matches_prefix = .TRUE.
    END FUNCTION history_matches_prefix

    SUBROUTINE parse_book_line(line_text, parsed_moves, parsed_count, line_start, err_msg, suggestion, replacement_line)
        CHARACTER(LEN=*), INTENT(IN) :: line_text
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES), INTENT(OUT) :: parsed_moves
        INTEGER, INTENT(OUT) :: parsed_count, line_start
        CHARACTER(LEN=256), INTENT(OUT) :: err_msg, suggestion
        CHARACTER(LEN=512), INTENT(OUT) :: replacement_line

        CHARACTER(LEN=64) :: token, move_text
        INTEGER :: pos, line_len, token_len, current_halfmove, token_halfmove, token_start, token_end
        LOGICAL :: has_number

        parsed_moves = ''
        parsed_count = 0
        line_start = 0
        err_msg = ''
        suggestion = ''
        replacement_line = ''
        current_halfmove = 0
        pos = 1
        line_len = LEN_TRIM(line_text)

        DO WHILE (pos <= line_len)
            DO WHILE (pos <= line_len .AND. line_text(pos:pos) == ' ')
                pos = pos + 1
            END DO
            IF (pos > line_len) EXIT

            token_start = pos
            token_len = pos
            DO WHILE (token_len <= line_len)
                IF (line_text(token_len:token_len) == ' ') EXIT
                token_len = token_len + 1
            END DO
            token_end = token_len - 1
            token = line_text(pos:token_len - 1)
            pos = token_len + 1

            CALL parse_book_token(TRIM(token), has_number, token_halfmove, move_text, err_msg, suggestion)
            IF (LEN_TRIM(err_msg) /= 0) RETURN

            IF (has_number) THEN
                IF (line_start == 0) line_start = token_halfmove
                IF (current_halfmove == 0) THEN
                    current_halfmove = token_halfmove
                ELSE IF (token_halfmove /= current_halfmove) THEN
                    err_msg = 'Move number does not match the implied sequence on this line.'
                    suggestion = 'Use `' // TRIM(halfmove_marker(current_halfmove)) // '` here, or remove the repeated move number.'
                    replacement_line = replace_token_in_line(line_text, token_start, token_end, &
                        TRIM(halfmove_marker(current_halfmove)) // TRIM(move_text))
                    RETURN
                END IF
                IF (LEN_TRIM(move_text) == 0) CYCLE
            ELSE
                IF (line_start == 0) THEN
                    err_msg = 'Book line must start with a numbered move token.'
                    suggestion = 'Start the line with `1.e4`, `5.Nf3`, or `5...c5`.'
                    RETURN
                END IF
                move_text = token
            END IF

            IF (parsed_count >= MAX_BOOK_MOVES) THEN
                err_msg = 'Book line exceeds MAX_BOOK_MOVES.'
                suggestion = 'Split the variation across more lines.'
                RETURN
            END IF

            parsed_count = parsed_count + 1
            parsed_moves(parsed_count) = normalize_move_text(move_text)
            IF (LEN_TRIM(parsed_moves(parsed_count)) == 0) THEN
                err_msg = 'Missing move text after move number.'
                suggestion = 'Write moves like `1.e4` or `5...c5`.'
                RETURN
            END IF
            current_halfmove = current_halfmove + 1
        END DO

        IF (line_start == 0) THEN
            err_msg = 'Book line is empty after trimming.'
            suggestion = 'Remove the line or start it with a move like `1.e4`.'
        END IF
    END SUBROUTINE parse_book_line

    SUBROUTINE parse_book_token(token, has_number, halfmove, move_text, err_msg, suggestion)
        CHARACTER(LEN=*), INTENT(IN) :: token
        LOGICAL, INTENT(OUT) :: has_number
        INTEGER, INTENT(OUT) :: halfmove
        CHARACTER(LEN=64), INTENT(OUT) :: move_text
        CHARACTER(LEN=256), INTENT(OUT) :: err_msg, suggestion

        INTEGER :: i, move_number, dot_count, ios
        CHARACTER(LEN=16) :: number_text

        has_number = .FALSE.
        halfmove = 0
        move_text = ''
        err_msg = ''
        suggestion = ''

        i = 1
        DO WHILE (i <= LEN_TRIM(token))
            IF (token(i:i) < '0' .OR. token(i:i) > '9') EXIT
            i = i + 1
        END DO

        IF (i == 1) THEN
            RETURN
        END IF
        IF (i > LEN_TRIM(token)) THEN
            RETURN
        END IF
        IF (token(i:i) /= '.') THEN
            RETURN
        END IF

        has_number = .TRUE.
        number_text = token(1:i-1)
        READ(number_text, *, IOSTAT=ios) move_number
        IF (ios /= 0 .OR. move_number < 1) THEN
            err_msg = 'Invalid move number in opening book.'
            suggestion = 'Move numbers must start at 1 and increase normally.'
            RETURN
        END IF

        dot_count = 0
        DO WHILE (i <= LEN_TRIM(token))
            IF (token(i:i) /= '.') EXIT
            dot_count = dot_count + 1
            i = i + 1
        END DO

        IF (dot_count == 1) THEN
            halfmove = 2 * move_number - 1
        ELSE IF (dot_count == 3) THEN
            halfmove = 2 * move_number
        ELSE
            err_msg = 'Move-number token must use `.` for White or `...` for Black.'
            suggestion = 'Examples: `1.e4`, `5.Nf3`, `5...c5`.'
            RETURN
        END IF

        IF (i <= LEN_TRIM(token)) move_text = token(i:)
    END SUBROUTINE parse_book_token

    FUNCTION halfmove_marker(halfmove) RESULT(marker)
        INTEGER, INTENT(IN) :: halfmove
        CHARACTER(LEN=16) :: marker
        INTEGER :: move_number

        marker = ''
        IF (MOD(halfmove, 2) == 1) THEN
            move_number = (halfmove + 1) / 2
            WRITE(marker, '(I0,A)') move_number, '.'
        ELSE
            move_number = halfmove / 2
            WRITE(marker, '(I0,A)') move_number, '...'
        END IF
    END FUNCTION halfmove_marker

    FUNCTION build_book_line(parsed_moves, parsed_count, start_halfmove) RESULT(line_text)
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES), INTENT(IN) :: parsed_moves
        INTEGER, INTENT(IN) :: parsed_count, start_halfmove
        CHARACTER(LEN=512) :: line_text
        INTEGER :: i

        line_text = ''
        IF (parsed_count <= 0) RETURN

        line_text = TRIM(halfmove_marker(start_halfmove)) // TRIM(parsed_moves(1))
        DO i = 2, parsed_count
            line_text = TRIM(line_text) // ' ' // TRIM(parsed_moves(i))
        END DO
    END FUNCTION build_book_line

    FUNCTION replace_token_in_line(line_text, token_start, token_end, replacement_token) RESULT(updated_line)
        CHARACTER(LEN=*), INTENT(IN) :: line_text, replacement_token
        INTEGER, INTENT(IN) :: token_start, token_end
        CHARACTER(LEN=512) :: updated_line
        INTEGER :: line_len

        updated_line = ''
        line_len = LEN_TRIM(line_text)

        IF (token_start <= 1) THEN
            IF (token_end < line_len) THEN
                updated_line = TRIM(replacement_token) // line_text(token_end + 1:line_len)
            ELSE
                updated_line = TRIM(replacement_token)
            END IF
        ELSE IF (token_end < line_len) THEN
            updated_line = line_text(1:token_start - 1) // TRIM(replacement_token) // line_text(token_end + 1:line_len)
        ELSE
            updated_line = line_text(1:token_start - 1) // TRIM(replacement_token)
        END IF
    END FUNCTION replace_token_in_line

END MODULE Opening_Book
