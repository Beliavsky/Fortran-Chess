MODULE Opening_Book
    USE Chess_Types
    USE Notation_Utils, ONLY: normalize_move_text, move_matches_input
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: Opening_Book_Type, load_opening_book, choose_book_move

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
        INTEGER, ALLOCATABLE, DIMENSION(:) :: line_lengths
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:, :) :: lines
    END TYPE Opening_Book_Type

CONTAINS

    SUBROUTINE load_opening_book(filename, book)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(Opening_Book_Type), INTENT(OUT) :: book

        INTEGER :: unit_no, ios, line_no, prev_len, entry_len, base_len, line_start, next_expected
        CHARACTER(LEN=512) :: raw_line, trimmed_line
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES) :: prev_sequence, parsed_moves, full_sequence
        INTEGER, ALLOCATABLE, DIMENSION(:) :: temp_lengths
        CHARACTER(LEN=32), ALLOCATABLE, DIMENSION(:, :) :: temp_lines
        INTEGER :: parsed_count
        CHARACTER(LEN=256) :: err_msg, suggestion
        LOGICAL :: exists

        book%found = .FALSE.
        book%valid = .FALSE.
        book%num_lines = 0
        book%error_line = 0
        book%filename = filename
        book%error_message = ''
        book%suggestion = ''
        prev_len = 0
        prev_sequence = ''

        INQUIRE(FILE=filename, EXIST=exists)
        IF (.NOT. exists) THEN
            book%found = .FALSE.
            RETURN
        END IF

        book%found = .TRUE.
        ALLOCATE(temp_lengths(MAX_BOOK_LINES))
        ALLOCATE(temp_lines(MAX_BOOK_MOVES, MAX_BOOK_LINES))
        temp_lengths = 0
        temp_lines = ''
        unit_no = 41
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            book%error_message = 'Could not open opening book file.'
            book%suggestion = 'Check the file path and permissions.'
            DEALLOCATE(temp_lengths, temp_lines)
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

            CALL parse_book_line(trimmed_line, parsed_moves, parsed_count, line_start, err_msg, suggestion)
            IF (LEN_TRIM(err_msg) /= 0) THEN
                book%error_line = line_no
                book%error_message = err_msg
                book%suggestion = suggestion
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_lines)
                RETURN
            END IF

            IF (book%num_lines == 0) THEN
                IF (line_start /= 1) THEN
                    book%error_line = line_no
                    book%error_message = 'The first book line must start at move 1.'
                    book%suggestion = 'Start the line with `1.e4` or `1.d4`.'
                    CLOSE(unit_no)
                    DEALLOCATE(temp_lengths, temp_lines)
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
                    CLOSE(unit_no)
                    DEALLOCATE(temp_lengths, temp_lines)
                    RETURN
                END IF
            END IF

            entry_len = base_len + parsed_count
            IF (entry_len > MAX_BOOK_MOVES) THEN
                book%error_line = line_no
                book%error_message = 'Book line is too long.'
                book%suggestion = 'Increase structure depth by splitting into shorter variations.'
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_lines)
                RETURN
            END IF
            IF (book%num_lines >= MAX_BOOK_LINES) THEN
                book%error_line = line_no
                book%error_message = 'Too many book lines for current fixed storage.'
                book%suggestion = 'Reduce the book size or raise MAX_BOOK_LINES in opening_book.f90.'
                CLOSE(unit_no)
                DEALLOCATE(temp_lengths, temp_lines)
                RETURN
            END IF

            full_sequence = ''
            IF (base_len > 0) full_sequence(1:base_len) = prev_sequence(1:base_len)
            full_sequence(base_len + 1:entry_len) = parsed_moves(1:parsed_count)

            book%num_lines = book%num_lines + 1
            temp_lengths(book%num_lines) = entry_len
            temp_lines(:, book%num_lines) = ''
            temp_lines(1:entry_len, book%num_lines) = full_sequence(1:entry_len)

            prev_sequence = full_sequence
            prev_len = entry_len
        END DO

        CLOSE(unit_no)
        IF (book%num_lines > 0) THEN
            ALLOCATE(book%line_lengths(book%num_lines))
            ALLOCATE(book%lines(MAX_BOOK_MOVES, book%num_lines))
            book%line_lengths = temp_lengths(1:book%num_lines)
            book%lines = temp_lines(:, 1:book%num_lines)
            book%valid = .TRUE.
        END IF
        DEALLOCATE(temp_lengths, temp_lines)
    END SUBROUTINE load_opening_book

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
        IF (.NOT. book%valid) RETURN

        DO i = 1, book%num_lines
            IF (book%line_lengths(i) <= num_half_moves) CYCLE
            IF (.NOT. history_matches_prefix(book%lines(:, i), book%line_lengths(i), move_history, num_half_moves)) CYCLE

            next_move_text = book%lines(num_half_moves + 1, i)
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

    SUBROUTINE parse_book_line(line_text, parsed_moves, parsed_count, line_start, err_msg, suggestion)
        CHARACTER(LEN=*), INTENT(IN) :: line_text
        CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES), INTENT(OUT) :: parsed_moves
        INTEGER, INTENT(OUT) :: parsed_count, line_start
        CHARACTER(LEN=256), INTENT(OUT) :: err_msg, suggestion

        CHARACTER(LEN=64) :: token, move_text
        INTEGER :: pos, line_len, token_len, current_halfmove, token_halfmove
        LOGICAL :: has_number

        parsed_moves = ''
        parsed_count = 0
        line_start = 0
        err_msg = ''
        suggestion = ''
        current_halfmove = 0
        pos = 1
        line_len = LEN_TRIM(line_text)

        DO WHILE (pos <= line_len)
            DO WHILE (pos <= line_len .AND. line_text(pos:pos) == ' ')
                pos = pos + 1
            END DO
            IF (pos > line_len) EXIT

            token_len = pos
            DO WHILE (token_len <= line_len)
                IF (line_text(token_len:token_len) == ' ') EXIT
                token_len = token_len + 1
            END DO
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
        IF (i > LEN_TRIM(token) .OR. token(i:i) /= '.') THEN
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
        DO WHILE (i <= LEN_TRIM(token) .AND. token(i:i) == '.')
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

END MODULE Opening_Book
