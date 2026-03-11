MODULE Search_Debug_Log
    USE Chess_Types, ONLY: WHITE, BLACK
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: start_ai_turn_debug_log, log_opening_book_hit, log_opening_book_miss, &
              log_search_depth_start, log_search_root_candidate, log_search_aspiration_retry, &
              log_search_depth_complete, log_search_timeout, log_search_final_choice, &
              finish_ai_turn_debug_log

    INTEGER, PARAMETER :: LOG_UNIT = 92
    CHARACTER(LEN=*), PARAMETER :: LOG_FILENAME = 'search_debug.log'
    LOGICAL, SAVE :: log_active = .FALSE.

CONTAINS

    SUBROUTINE start_ai_turn_debug_log(frontend_name, player_color, move_history, num_half_moves, max_depth, &
        time_budget_seconds)
        CHARACTER(LEN=*), INTENT(IN) :: frontend_name
        INTEGER, INTENT(IN) :: player_color, num_half_moves, max_depth
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        REAL, INTENT(IN), OPTIONAL :: time_budget_seconds

        INTEGER :: ios, values(8), fullmove_number
        CHARACTER(LEN=32) :: time_text

        IF (log_active) THEN
            CLOSE(LOG_UNIT)
            log_active = .FALSE.
        END IF

        OPEN(unit=LOG_UNIT, file=LOG_FILENAME, status='UNKNOWN', action='WRITE', position='APPEND', iostat=ios)
        IF (ios /= 0) RETURN
        log_active = .TRUE.

        CALL DATE_AND_TIME(VALUES=values)
        fullmove_number = num_half_moves / 2 + 1

        WRITE(LOG_UNIT, '(A)') '================================================================'
        WRITE(LOG_UNIT, '(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') 'Timestamp: ', &
            values(1), '-', values(2), '-', values(3), ' ', values(5), ':', values(6), ':', values(7)
        WRITE(LOG_UNIT, '(A,A)') 'Frontend: ', TRIM(frontend_name)
        WRITE(LOG_UNIT, '(A,A,A,I0)') 'AI Turn: ', TRIM(color_name(player_color)), ' to move, fullmove ', fullmove_number
        WRITE(LOG_UNIT, '(A,I0)') 'Max depth: ', max_depth
        IF (PRESENT(time_budget_seconds)) THEN
            IF (time_budget_seconds > 0.0) THEN
                WRITE(time_text, '(F8.3)') time_budget_seconds
                WRITE(LOG_UNIT, '(A,A,A)') 'Time budget: ', TRIM(ADJUSTL(time_text)), ' s'
            ELSE
                WRITE(LOG_UNIT, '(A)') 'Time budget: unlimited'
            END IF
        ELSE
            WRITE(LOG_UNIT, '(A)') 'Time budget: not supplied'
        END IF
        IF (num_half_moves > 0) THEN
            WRITE(LOG_UNIT, '(A,A)') 'Moves so far: ', TRIM(history_as_text(move_history, num_half_moves))
        ELSE
            WRITE(LOG_UNIT, '(A)') 'Moves so far: (start position)'
        END IF
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE start_ai_turn_debug_log

    SUBROUTINE log_opening_book_hit(book_filename, raw_move_text, san_text, coord_text)
        CHARACTER(LEN=*), INTENT(IN) :: book_filename, raw_move_text, san_text, coord_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A)') 'Opening book hit: ', TRIM(book_filename)
        WRITE(LOG_UNIT, '(A,A)') 'Book move text: ', TRIM(raw_move_text)
        WRITE(LOG_UNIT, '(A,A,A,A,A)') 'Chosen move: ', TRIM(san_text), ' [', TRIM(coord_text), ']'
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_opening_book_hit

    SUBROUTINE log_opening_book_miss(book_filename)
        CHARACTER(LEN=*), INTENT(IN) :: book_filename

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A)') 'Opening book miss: ', TRIM(book_filename)
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_opening_book_miss

    SUBROUTINE log_search_depth_start(depth, attempt, alpha, beta)
        INTEGER, INTENT(IN) :: depth, attempt, alpha, beta

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,I0,A,I0,A,A,A,A)') 'Depth ', depth, ', attempt ', attempt, &
            ', window [', TRIM(bound_to_text(alpha)), ', ', TRIM(bound_to_text(beta)) // ']'
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_depth_start

    SUBROUTINE log_search_root_candidate(depth, attempt, move_text, coord_text, score)
        INTEGER, INTENT(IN) :: depth, attempt, score
        CHARACTER(LEN=*), INTENT(IN) :: move_text, coord_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,I0,A,I0,A,A,A,A,A)') '  Candidate d', depth, ' a', attempt, ': ', &
            TRIM(move_text), ' [', TRIM(coord_text), ']'
        WRITE(LOG_UNIT, '(A,A)') '    Score: ', TRIM(score_to_text(score))
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_root_candidate

    SUBROUTINE log_search_aspiration_retry(depth, attempt, reason, score)
        INTEGER, INTENT(IN) :: depth, attempt, score
        CHARACTER(LEN=*), INTENT(IN) :: reason

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,I0,A,I0,A,A,A,A)') '  Aspiration retry at depth ', depth, ', attempt ', &
            attempt, ': ', TRIM(reason), ' after ', TRIM(score_to_text(score))
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_aspiration_retry

    SUBROUTINE log_search_depth_complete(depth, attempt, best_move_text, coord_text, score)
        INTEGER, INTENT(IN) :: depth, attempt, score
        CHARACTER(LEN=*), INTENT(IN) :: best_move_text, coord_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,I0,A,I0,A,A,A,A,A)') 'Depth ', depth, ', attempt ', attempt, ' complete. Best: ', &
            TRIM(best_move_text), ' [', TRIM(coord_text), ']'
        WRITE(LOG_UNIT, '(A,A)') '  Best score: ', TRIM(score_to_text(score))
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_depth_complete

    SUBROUTINE log_search_timeout(depth, attempt)
        INTEGER, INTENT(IN) :: depth, attempt

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,I0,A,I0,A)') 'Search timed out during depth ', depth, ', attempt ', attempt, '.'
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_timeout

    SUBROUTINE log_search_final_choice(move_text, coord_text, score, timed_out, used_completed_iteration)
        INTEGER, INTENT(IN) :: score
        LOGICAL, INTENT(IN) :: timed_out, used_completed_iteration
        CHARACTER(LEN=*), INTENT(IN) :: move_text, coord_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A,A,A,A)') 'Final choice: ', TRIM(move_text), ' [', TRIM(coord_text), ']'
        WRITE(LOG_UNIT, '(A,A)') 'Final score: ', TRIM(score_to_text(score))
        IF (timed_out) THEN
            IF (used_completed_iteration) THEN
                WRITE(LOG_UNIT, '(A)') 'Search ended on time and fell back to the last completed iteration.'
            ELSE
                WRITE(LOG_UNIT, '(A)') 'Search ended on time before any full iteration completed; using the best root move seen so far.'
            END IF
        ELSE
            WRITE(LOG_UNIT, '(A)') 'Search completed normally.'
        END IF
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_search_final_choice

    SUBROUTINE finish_ai_turn_debug_log(outcome_text)
        CHARACTER(LEN=*), INTENT(IN) :: outcome_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A)') 'Outcome: ', TRIM(outcome_text)
        WRITE(LOG_UNIT, '(A)') ''
        CALL FLUSH(LOG_UNIT)
        CLOSE(LOG_UNIT)
        log_active = .FALSE.
    END SUBROUTINE finish_ai_turn_debug_log

    PURE FUNCTION color_name(player_color) RESULT(name_text)
        INTEGER, INTENT(IN) :: player_color
        CHARACTER(LEN=5) :: name_text

        SELECT CASE (player_color)
        CASE (WHITE)
            name_text = 'White'
        CASE (BLACK)
            name_text = 'Black'
        CASE DEFAULT
            name_text = '?????'
        END SELECT
    END FUNCTION color_name

    FUNCTION history_as_text(move_history, num_half_moves) RESULT(text)
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        INTEGER, INTENT(IN) :: num_half_moves
        CHARACTER(LEN=4096) :: text

        INTEGER :: i
        CHARACTER(LEN=64) :: token

        text = ''
        DO i = 1, num_half_moves
            IF (MOD(i, 2) == 1) THEN
                WRITE(token, '(I0,A,A)') (i + 1) / 2, '. ', TRIM(move_history(i))
            ELSE
                token = TRIM(move_history(i))
            END IF
            CALL append_fragment(text, token)
        END DO
    END FUNCTION history_as_text

    PURE FUNCTION score_to_text(score) RESULT(text)
        INTEGER, INTENT(IN) :: score
        CHARACTER(LEN=32) :: text

        IF (ABS(score) >= 99000) THEN
            IF (score > 0) THEN
                text = 'mate-ish for side to move'
            ELSE
                text = 'mate-ish against side to move'
            END IF
        ELSE
            WRITE(text, '(I0,A)') score, ' cp'
        END IF
    END FUNCTION score_to_text

    PURE FUNCTION bound_to_text(bound) RESULT(text)
        INTEGER, INTENT(IN) :: bound
        CHARACTER(LEN=16) :: text

        IF (bound <= -100000) THEN
            text = '-INF'
        ELSE IF (bound >= 100000) THEN
            text = 'INF'
        ELSE
            WRITE(text, '(I0)') bound
        END IF
    END FUNCTION bound_to_text

    SUBROUTINE append_fragment(buffer, fragment)
        CHARACTER(LEN=*), INTENT(INOUT) :: buffer
        CHARACTER(LEN=*), INTENT(IN) :: fragment

        INTEGER :: buffer_len, fragment_len, used_len, copy_len

        fragment_len = LEN_TRIM(fragment)
        IF (fragment_len == 0) RETURN

        used_len = LEN_TRIM(buffer)
        buffer_len = LEN(buffer)

        IF (used_len > 0) THEN
            IF (used_len < buffer_len) THEN
                buffer(used_len + 1:used_len + 1) = ' '
                used_len = used_len + 1
            ELSE
                RETURN
            END IF
        END IF

        copy_len = MIN(fragment_len, buffer_len - used_len)
        IF (copy_len > 0) buffer(used_len + 1:used_len + copy_len) = fragment(1:copy_len)
    END SUBROUTINE append_fragment

END MODULE Search_Debug_Log
