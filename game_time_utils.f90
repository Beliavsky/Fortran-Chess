MODULE Game_Time_Utils
    USE Chess_Types
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: DEFAULT_COMPUTER_TIME_LIMIT_SECONDS, parse_time_control, elapsed_milliseconds, &
              apply_clock_after_move, get_ai_time_budget_seconds, format_clock

    REAL, PARAMETER :: DEFAULT_COMPUTER_TIME_LIMIT_SECONDS = 60.0

CONTAINS

    LOGICAL FUNCTION parse_time_control(text, initial_ms, increment_out_ms)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER(KIND=8), INTENT(OUT) :: initial_ms, increment_out_ms

        INTEGER :: plus_pos, ios
        REAL :: minutes_value, increment_value

        parse_time_control = .FALSE.
        initial_ms = 0
        increment_out_ms = 0
        plus_pos = INDEX(text, '+')
        IF (plus_pos <= 1 .OR. plus_pos >= LEN_TRIM(text)) RETURN

        READ(text(1:plus_pos - 1), *, IOSTAT=ios) minutes_value
        IF (ios /= 0 .OR. minutes_value <= 0.0) RETURN
        READ(text(plus_pos + 1:LEN_TRIM(text)), *, IOSTAT=ios) increment_value
        IF (ios /= 0 .OR. increment_value < 0.0) RETURN

        initial_ms = NINT(minutes_value * 60000.0)
        increment_out_ms = NINT(increment_value * 1000.0)
        parse_time_control = (initial_ms > 0)
    END FUNCTION parse_time_control

    INTEGER(KIND=8) FUNCTION elapsed_milliseconds(start_count, end_count, local_count_rate)
        INTEGER(KIND=8), INTENT(IN) :: start_count, end_count, local_count_rate

        elapsed_milliseconds = 0
        IF (local_count_rate <= 0) RETURN
        elapsed_milliseconds = MAX(0_8, NINT(1000.0 * REAL(end_count - start_count) / REAL(local_count_rate)))
    END FUNCTION elapsed_milliseconds

    LOGICAL FUNCTION apply_clock_after_move(player_color, spent_ms, white_ms, black_ms, inc_ms)
        INTEGER, INTENT(IN) :: player_color
        INTEGER(KIND=8), INTENT(IN) :: spent_ms, inc_ms
        INTEGER(KIND=8), INTENT(INOUT) :: white_ms, black_ms

        apply_clock_after_move = .TRUE.
        IF (player_color == WHITE) THEN
            IF (spent_ms >= white_ms) THEN
                white_ms = 0
                apply_clock_after_move = .FALSE.
            ELSE
                white_ms = white_ms - spent_ms + inc_ms
            END IF
        ELSE
            IF (spent_ms >= black_ms) THEN
                black_ms = 0
                apply_clock_after_move = .FALSE.
            ELSE
                black_ms = black_ms - spent_ms + inc_ms
            END IF
        END IF
    END FUNCTION apply_clock_after_move

    REAL FUNCTION get_ai_time_budget_seconds(player_color, white_ms, black_ms, inc_ms, half_moves_played, &
        time_control_enabled) RESULT(seconds_out)
        INTEGER, INTENT(IN) :: player_color, half_moves_played
        INTEGER(KIND=8), INTENT(IN) :: white_ms, black_ms, inc_ms
        LOGICAL, INTENT(IN) :: time_control_enabled

        INTEGER(KIND=8) :: remaining_ms, reserve_ms, share_ms, budget_ms
        INTEGER :: estimated_moves_remaining

        IF (.NOT. time_control_enabled) THEN
            seconds_out = DEFAULT_COMPUTER_TIME_LIMIT_SECONDS
            RETURN
        END IF

        IF (player_color == WHITE) THEN
            remaining_ms = white_ms
        ELSE
            remaining_ms = black_ms
        END IF

        IF (remaining_ms <= 0) THEN
            seconds_out = 1.0
            RETURN
        END IF

        estimated_moves_remaining = MAX(8, 40 - half_moves_played / 2)
        share_ms = remaining_ms / estimated_moves_remaining
        reserve_ms = MIN(MAX(inc_ms * 3, 5000_8), remaining_ms / 2)
        budget_ms = share_ms + inc_ms / 2
        budget_ms = MAX(1000_8, budget_ms)
        budget_ms = MIN(budget_ms, MAX(1000_8, remaining_ms - reserve_ms))

        seconds_out = REAL(MAX(1_8, budget_ms)) / 1000.0
    END FUNCTION get_ai_time_budget_seconds

    FUNCTION format_clock(clock_ms) RESULT(clock_text)
        INTEGER(KIND=8), INTENT(IN) :: clock_ms
        CHARACTER(LEN=16) :: clock_text
        INTEGER(KIND=8) :: total_seconds, hours, minutes, seconds

        total_seconds = MAX(0_8, (clock_ms + 500_8) / 1000_8)
        hours = total_seconds / 3600_8
        minutes = MOD(total_seconds, 3600_8) / 60_8
        seconds = MOD(total_seconds, 60_8)

        IF (hours > 0) THEN
            WRITE(clock_text, '(I0,A,I2.2,A,I2.2)') hours, ':', minutes, ':', seconds
        ELSE
            WRITE(clock_text, '(I0,A,I2.2)') minutes, ':', seconds
        END IF
    END FUNCTION format_clock

END MODULE Game_Time_Utils
