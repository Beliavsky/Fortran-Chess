! ============================================
! Main Program: Human vs Computer
! ============================================
PROGRAM Fortran_Chess
    USE Chess_Types
    USE Board_Utils
    USE Evaluation, ONLY: evaluate_board
    USE Move_Generation ! Needs Make_Unmake implicitly
    USE Make_Unmake
    USE Search
    USE Transposition_Table, ONLY: init_zobrist_keys
    USE User_Input_Processor, ONLY: get_human_move, print_in_game_help
    USE Game_State_Checker
    USE Notation_Utils, ONLY: move_to_san, write_pgn_file, to_lower_string
    USE Opening_Book, ONLY: Opening_Book_Type, load_opening_book, choose_book_move
    USE Opening_Sequence, ONLY: load_opening_sequence
    USE UCI_Driver, ONLY: run_uci_mode
    IMPLICIT NONE

    TYPE(Board_Type) :: game_board
    TYPE(Move_Type) :: chosen_move
    TYPE(UnmakeInfo_Type) :: move_info ! Needed for make_move call
    LOGICAL :: move_found, is_human_turn, game_over
    INTEGER :: human_player_color, ai_player_color
    INTEGER :: search_depth
    CHARACTER(LEN=10) :: user_input ! Keep for color selection
    TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
    INTEGER :: num_legal_moves
    INTEGER :: game_winner_color, current_game_status
    INTEGER :: argc, arg_idx
    CHARACTER(LEN=260) :: arg_value
    LOGICAL :: uci_mode
    CHARACTER(LEN=32), DIMENSION(512) :: move_history
    INTEGER :: num_half_moves
    CHARACTER(LEN=32) :: move_text
    CHARACTER(LEN=7) :: pgn_result
    TYPE(Opening_Book_Type) :: white_book, black_book
    TYPE(Opening_Book_Type) :: active_book
    LOGICAL :: book_move_found
    CHARACTER(LEN=32) :: book_move_text
    LOGICAL :: self_play_mode
    LOGICAL :: show_eval_after_ai
    LOGICAL :: resign_enabled
    LOGICAL :: game_resigned
    CHARACTER(LEN=32) :: settings_input
    CHARACTER(LEN=260) :: lower_arg1
    INTEGER :: resign_threshold_cp
    INTEGER :: resignation_winner_color
    LOGICAL :: accept_resignation
    LOGICAL :: open_requested, opening_loaded
    CHARACTER(LEN=260) :: opening_filename
    CHARACTER(LEN=256) :: opening_error
    REAL, PARAMETER :: DEFAULT_COMPUTER_TIME_LIMIT_SECONDS = 60.0
    LOGICAL :: time_control_enabled, game_lost_on_time
    INTEGER(KIND=8) :: white_time_ms, black_time_ms, increment_ms
    INTEGER(KIND=8) :: turn_start_count, turn_end_count, count_rate, elapsed_ms
    INTEGER :: time_forfeit_winner_color
    INTEGER :: resignation_attempts
    INTEGER, PARAMETER :: MAX_RESIGNATION_ATTEMPTS = 3

    uci_mode = .FALSE.
    self_play_mode = .FALSE.
    show_eval_after_ai = .TRUE.
    resign_enabled = .TRUE.
    resign_threshold_cp = 500
    game_resigned = .FALSE.
    num_half_moves = 0
    pgn_result = '*'
    current_game_status = GAME_ONGOING
    game_winner_color = NO_COLOR
    resignation_winner_color = NO_COLOR
    time_control_enabled = .FALSE.
    game_lost_on_time = .FALSE.
    white_time_ms = 0
    black_time_ms = 0
    increment_ms = 0
    time_forfeit_winner_color = NO_COLOR
    resignation_attempts = 0
    open_requested = .FALSE.
    opening_filename = ''
    opening_error = ''
    CALL SYSTEM_CLOCK(count_rate=count_rate)
    argc = COMMAND_ARGUMENT_COUNT()
    arg_idx = 1
    DO WHILE (arg_idx <= argc)
        CALL GET_COMMAND_ARGUMENT(arg_idx, arg_value)
        lower_arg1 = TRIM(ADJUSTL(to_lower_string(arg_value)))
        IF (lower_arg1 == '--help' .OR. lower_arg1 == '-help' .OR. lower_arg1 == 'help') THEN
            CALL print_help()
            STOP
        ELSE IF (lower_arg1 == '--uci' .OR. lower_arg1 == '-uci') THEN
            uci_mode = .TRUE.
        ELSE IF (lower_arg1 == '--open' .OR. lower_arg1 == '-open') THEN
            IF (arg_idx >= argc) THEN
                PRINT *, "Missing filename after --open."
                CALL print_help()
                STOP
            END IF
            arg_idx = arg_idx + 1
            CALL GET_COMMAND_ARGUMENT(arg_idx, opening_filename)
            open_requested = .TRUE.
        ELSE
            PRINT *, "Unknown option: ", TRIM(arg_value)
            CALL print_help()
            STOP
        END IF
        arg_idx = arg_idx + 1
    END DO

    IF (uci_mode) THEN
        CALL run_uci_mode()
        STOP
    END IF

    ! --- Initialize Zobrist Keys and Board ---
    CALL init_zobrist_keys()
    CALL init_board(game_board)
    IF (open_requested) THEN
        CALL load_opening_sequence(opening_filename, game_board, move_history, num_half_moves, opening_loaded, opening_error)
        IF (.NOT. opening_loaded) THEN
            PRINT *, "Could not load opening sequence from " // TRIM(opening_filename)
            PRINT *, TRIM(opening_error)
            STOP
        END IF
        PRINT *, "Loaded opening sequence from " // TRIM(opening_filename)
    END IF

    search_depth = 5 ! AI Difficulty
    ! --- Player Color Selection ---
    DO
        PRINT *, "Choose your color (White/Black): "
        READ *, user_input
        SELECT CASE (TRIM(ADJUSTL(user_input))) ! Basic input handling
        CASE ('White', 'white', 'W', 'w')
            human_player_color = WHITE
            ai_player_color = BLACK
            PRINT *, "You play as White."
            EXIT
        CASE ('Black', 'black', 'B', 'b')
            human_player_color = BLACK
            ai_player_color = WHITE
            PRINT *, "You play as Black."
            EXIT
        CASE ('None', 'none', 'N', 'n')
            human_player_color = NO_COLOR
            ai_player_color = NO_COLOR
            self_play_mode = .TRUE.
            PRINT *, "Self-play mode."
            EXIT
        CASE DEFAULT
            PRINT *, "Invalid input. Please enter 'White', 'Black', or 'N' for self-play."
        END SELECT
    END DO

    ! --- Search Depth Selection ---
    DO
        PRINT *, "Time control for both sides (e.g., 3+2 for 3 minutes plus 2 seconds increment, blank for none): "
        READ(*, '(A)') settings_input
        settings_input = TRIM(ADJUSTL(to_lower_string(settings_input)))
        IF (LEN_TRIM(settings_input) == 0 .OR. settings_input == 'off' .OR. settings_input == 'none') THEN
            time_control_enabled = .FALSE.
            EXIT
        ELSE IF (parse_time_control(settings_input, white_time_ms, increment_ms)) THEN
            time_control_enabled = .TRUE.
            black_time_ms = white_time_ms
            EXIT
        ELSE
            PRINT *, "Enter a time control like 3+2 or leave blank for none."
        END IF
    END DO

    DO
        IF (time_control_enabled) THEN
            PRINT *, "Enter AI max search depth (e.g., 5, actual think time comes from the shared clock): "
        ELSE
            PRINT *, "Enter AI max search depth (e.g., 5, still capped at 60 seconds per computer move): "
        END IF
        READ *, search_depth
        IF (search_depth >= 1 .AND. search_depth <= 10) THEN ! Example valid range
            EXIT
        ELSE
            PRINT *, "Invalid search depth. Please enter a number between 1 and 10."
        END IF
    END DO

    DO
        PRINT *, "Show evaluation after computer moves? (Y/n): "
        READ(*, '(A)') settings_input
        settings_input = TRIM(ADJUSTL(to_lower_string(settings_input)))
        IF (LEN_TRIM(settings_input) == 0 .OR. settings_input == 'y' .OR. settings_input == 'yes') THEN
            show_eval_after_ai = .TRUE.
            EXIT
        ELSE IF (settings_input == 'n' .OR. settings_input == 'no') THEN
            show_eval_after_ai = .FALSE.
            EXIT
        ELSE
            PRINT *, "Please answer Y or N."
        END IF
    END DO

    DO
        PRINT *, "Resign threshold in pawns (default 5, type off for no resignation): "
        READ(*, '(A)') settings_input
        settings_input = TRIM(ADJUSTL(to_lower_string(settings_input)))
        IF (LEN_TRIM(settings_input) == 0) THEN
            resign_enabled = .TRUE.
            resign_threshold_cp = 500
            EXIT
        ELSE IF (settings_input == 'off' .OR. settings_input == 'none') THEN
            resign_enabled = .FALSE.
            EXIT
        ELSE IF (verify_numeric_setting(settings_input)) THEN
            CALL apply_resign_setting(settings_input, resign_enabled, resign_threshold_cp)
            EXIT
        ELSE
            PRINT *, "Enter a positive number like 5 or type off."
        END IF
    END DO

    CALL load_opening_book('book_white.txt', white_book)
    CALL load_opening_book('book_black.txt', black_book)
    CALL report_book_status(white_book, 'White')
    CALL report_book_status(black_book, 'Black')
    CALL print_board(game_board)

    ! --- Game Loop ---
    game_over = .FALSE.
    DO WHILE (.NOT. game_over)

        ! 1. Check Game Over
        game_over = is_game_over(game_board, game_winner_color, current_game_status)
        IF (game_over) THEN
            SELECT CASE (current_game_status)
                CASE (GAME_CHECKMATE)
                    IF (game_winner_color == WHITE) THEN
                        PRINT *, "=== CHECKMATE! White wins! ==="
                    ELSE
                        PRINT *, "=== CHECKMATE! Black wins! ==="
                    END IF
                CASE (GAME_STALEMATE)
                    PRINT *, "=== STALEMATE! Draw. ==="
                CASE DEFAULT
                    ! Should not happen
                    PRINT *, "Error: Unknown game over status."
            END SELECT
            EXIT ! Exit game loop
        END IF

        ! 2. Determine Turn
        is_human_turn = (.NOT. self_play_mode .AND. game_board%current_player == human_player_color)
        IF (time_control_enabled) CALL print_clock_status(white_time_ms, black_time_ms)

        IF (is_human_turn) THEN
            ! --- Human's Turn ---
            ! Generate the current legal moves before prompting the player
            CALL generate_moves(game_board, legal_moves, num_legal_moves)
            IF (num_legal_moves == 0) THEN
                PRINT *, "No legal moves available."
                game_over = .TRUE.
                CYCLE
            END IF

            IF (time_control_enabled) CALL SYSTEM_CLOCK(turn_start_count)
            move_found = get_human_move(game_board, legal_moves, num_legal_moves, search_depth, &
                show_eval_after_ai, resign_enabled, resign_threshold_cp, chosen_move, game_over)
            IF (time_control_enabled) THEN
                CALL SYSTEM_CLOCK(turn_end_count)
                elapsed_ms = elapsed_milliseconds(turn_start_count, turn_end_count, count_rate)
            END IF

            IF (game_over) EXIT ! Exit game loop if user quit

            IF (move_found) THEN
                 IF (time_control_enabled) THEN
                     IF (.NOT. apply_clock_after_move(game_board%current_player, elapsed_ms, white_time_ms, black_time_ms, increment_ms)) THEN
                         game_lost_on_time = .TRUE.
                         time_forfeit_winner_color = get_opponent_color(game_board%current_player)
                         game_over = .TRUE.
                         EXIT
                     END IF
                 END IF
                 move_text = move_to_san(game_board, chosen_move, legal_moves, num_legal_moves)
                 IF (num_half_moves < SIZE(move_history)) THEN
                     num_half_moves = num_half_moves + 1
                     move_history(num_half_moves) = TRIM(move_text)
                 END IF
                 PRINT *, "You played ", TRIM(move_text)
                 CALL make_move(game_board, chosen_move, move_info)
            END IF

        ELSE
            ! --- AI's Turn ---
                        PRINT *, "" ! Newline
            PRINT *, "Computer's turn. Thinking..."
            CALL generate_moves(game_board, legal_moves, num_legal_moves)
            book_move_found = .FALSE.
            book_move_text = ''

            IF (game_board%current_player == WHITE) THEN
                active_book = white_book
            ELSE
                active_book = black_book
            END IF

            IF (resign_enabled .AND. resignation_attempts < MAX_RESIGNATION_ATTEMPTS .AND. &
                should_computer_resign(game_board, resign_threshold_cp)) THEN
                resignation_attempts = resignation_attempts + 1
                accept_resignation = .TRUE.
                IF (.NOT. self_play_mode) THEN
                    accept_resignation = prompt_resignation_acceptance(game_board%current_player)
                END IF
                IF (accept_resignation) THEN
                    game_resigned = .TRUE.
                    resignation_winner_color = get_opponent_color(game_board%current_player)
                    game_over = .TRUE.
                    EXIT
                ELSE
                    PRINT *, "Resignation refused. Play continues."
                END IF
            END IF

            IF (active_book%valid) THEN
                book_move_found = choose_book_move(active_book, move_history, num_half_moves, game_board, &
                    legal_moves, num_legal_moves, chosen_move, book_move_text)
            END IF

            IF (book_move_found) THEN
                move_found = .TRUE.
                PRINT *, "Computer found a book move."
            ELSE
                IF (time_control_enabled) CALL SYSTEM_CLOCK(turn_start_count)
                CALL find_best_move(game_board, search_depth, move_found, chosen_move, &
                    time_limit_seconds=get_ai_time_budget_seconds(game_board%current_player, white_time_ms, black_time_ms, increment_ms, num_half_moves), &
                    show_countdown=.TRUE.)
                IF (time_control_enabled) THEN
                    CALL SYSTEM_CLOCK(turn_end_count)
                    elapsed_ms = elapsed_milliseconds(turn_start_count, turn_end_count, count_rate)
                ELSE
                    elapsed_ms = 0
                END IF
                CALL generate_moves(game_board, legal_moves, num_legal_moves)
            END IF
            IF (move_found) THEN
                IF (time_control_enabled) THEN
                    IF (book_move_found) THEN
                        elapsed_ms = 0
                    END IF
                    IF (.NOT. apply_clock_after_move(game_board%current_player, elapsed_ms, white_time_ms, black_time_ms, increment_ms)) THEN
                        game_lost_on_time = .TRUE.
                        time_forfeit_winner_color = get_opponent_color(game_board%current_player)
                        game_over = .TRUE.
                        EXIT
                    END IF
                END IF
                move_text = move_to_san(game_board, chosen_move, legal_moves, num_legal_moves)
                IF (num_half_moves < SIZE(move_history)) THEN
                    num_half_moves = num_half_moves + 1
                    move_history(num_half_moves) = TRIM(move_text)
                END IF
                PRINT *, "Computer played ", TRIM(move_text)
                CALL make_move(game_board, chosen_move, move_info)
                IF (show_eval_after_ai) THEN
                    CALL print_position_evaluation(game_board)
                END IF
            ELSE
                ! Should be caught by game over check, but safety print
                PRINT *, "Error: AI found no move but game not over?"
                game_over = .TRUE.
            END IF
        END IF

        ! Print board after move (if game not over)
         IF (.NOT. game_over) THEN
             CALL print_board(game_board)
         END IF

    END DO ! End game loop

    IF (game_lost_on_time) THEN
        IF (time_forfeit_winner_color == WHITE) THEN
            pgn_result = '1-0'
            PRINT *, "=== TIME FORFEIT! White wins! ==="
        ELSE
            pgn_result = '0-1'
            PRINT *, "=== TIME FORFEIT! Black wins! ==="
        END IF
    ELSE IF (game_resigned) THEN
        IF (resignation_winner_color == WHITE) THEN
            pgn_result = '1-0'
            PRINT *, "=== RESIGNATION! White wins! ==="
        ELSE
            pgn_result = '0-1'
            PRINT *, "=== RESIGNATION! Black wins! ==="
        END IF
    ELSE
        SELECT CASE (current_game_status)
        CASE (GAME_CHECKMATE)
            IF (game_winner_color == WHITE) THEN
                pgn_result = '1-0'
            ELSE
                pgn_result = '0-1'
            END IF
        CASE (GAME_STALEMATE)
            pgn_result = '1/2-1/2'
        CASE DEFAULT
            pgn_result = '*'
        END SELECT
    END IF

    CALL write_pgn_file('games.pgn', move_history, num_half_moves, pgn_result)
    PRINT *, "Saved game to games.pgn"
    PRINT *, "Game finished."

CONTAINS

    SUBROUTINE report_book_status(book, side_name)
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        CHARACTER(LEN=*), INTENT(IN) :: side_name

        IF (.NOT. book%found) RETURN

        IF (book%valid) THEN
            PRINT *, TRIM(side_name) // " opening book loaded from " // TRIM(book%filename)
        ELSE
            PRINT *, "Opening book syntax error in " // TRIM(book%filename)
            IF (book%error_line > 0) PRINT *, "Line ", book%error_line
            IF (LEN_TRIM(book%error_message) > 0) PRINT *, TRIM(book%error_message)
            IF (LEN_TRIM(book%suggestion) > 0) PRINT *, "Suggestion: " // TRIM(book%suggestion)
        END IF
    END SUBROUTINE report_book_status

    SUBROUTINE print_help()
        PRINT *, "FortranChess"
        PRINT *, "Usage:"
        PRINT *, "  chess               Start console mode"
        PRINT *, "  chess --uci         Start UCI mode"
        PRINT *, "  chess --open FILE   Start from PGN movetext in FILE"
        PRINT *, "  chess --help        Show this help"
        PRINT *, "  chess -help"
        PRINT *, "  chess help"
        PRINT *, ""
        PRINT *, "Console setup:"
        PRINT *, "  Choose White, Black, or N for self-play."
        PRINT *, "  Optional shared time control uses M+I, e.g. 3+2."
        PRINT *, "  Set max search depth, eval display, and resignation threshold."
        PRINT *, "  Without a shared time control, computer moves use a default 60 second time limit."
        PRINT *, "  PGN tags, comments, and results are ignored in --open files."
        PRINT *, ""
        CALL print_in_game_help()
    END SUBROUTINE print_help

    SUBROUTINE print_clock_status(white_ms, black_ms)
        INTEGER(KIND=8), INTENT(IN) :: white_ms, black_ms

        PRINT *, "Clocks: White ", TRIM(format_clock(white_ms)), "   Black ", TRIM(format_clock(black_ms))
    END SUBROUTINE print_clock_status

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
        INTEGER(KIND=8), INTENT(IN) :: start_count, end_count
        INTEGER(KIND=8), INTENT(IN) :: local_count_rate

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

    REAL FUNCTION get_ai_time_budget_seconds(player_color, white_ms, black_ms, inc_ms, half_moves_played)
        INTEGER, INTENT(IN) :: player_color, half_moves_played
        INTEGER(KIND=8), INTENT(IN) :: white_ms, black_ms, inc_ms

        INTEGER(KIND=8) :: remaining_ms, reserve_ms, share_ms, budget_ms
        INTEGER :: estimated_moves_remaining

        IF (player_color == WHITE) THEN
            remaining_ms = white_ms
        ELSE
            remaining_ms = black_ms
        END IF

        IF (remaining_ms <= 0) THEN
            get_ai_time_budget_seconds = 1.0
            RETURN
        END IF

        estimated_moves_remaining = MAX(8, 40 - half_moves_played / 2)
        share_ms = remaining_ms / estimated_moves_remaining
        reserve_ms = MIN(MAX(inc_ms * 3, 5000_8), remaining_ms / 2)
        budget_ms = share_ms + inc_ms / 2
        budget_ms = MAX(1000_8, budget_ms)
        budget_ms = MIN(budget_ms, MAX(1000_8, remaining_ms - reserve_ms))

        get_ai_time_budget_seconds = REAL(MAX(1_8, budget_ms)) / 1000.0
        IF (.NOT. time_control_enabled) THEN
            get_ai_time_budget_seconds = DEFAULT_COMPUTER_TIME_LIMIT_SECONDS
        END IF
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

    SUBROUTINE print_position_evaluation(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: eval_cp, white_eval_cp

        eval_cp = evaluate_board(board)
        IF (board%current_player == BLACK) THEN
            white_eval_cp = -eval_cp
        ELSE
            white_eval_cp = eval_cp
        END IF
        WRITE(*, '(A,F7.2,A)') "Position evaluation (White perspective): ", REAL(white_eval_cp) / 100.0, " pawns"
    END SUBROUTINE print_position_evaluation

    LOGICAL FUNCTION should_computer_resign(board, threshold_cp)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: threshold_cp
        INTEGER :: eval_cp

        eval_cp = evaluate_board(board)
        should_computer_resign = (eval_cp <= -threshold_cp)
    END FUNCTION should_computer_resign

    LOGICAL FUNCTION prompt_resignation_acceptance(resigning_color)
        INTEGER, INTENT(IN) :: resigning_color
        CHARACTER(LEN=32) :: reply

        IF (resigning_color == WHITE) THEN
            PRINT *, "White wants to resign. Accept? (Y/n): "
        ELSE
            PRINT *, "Black wants to resign. Accept? (Y/n): "
        END IF
        READ(*, '(A)') reply
        reply = TRIM(ADJUSTL(to_lower_string(reply)))
        prompt_resignation_acceptance = .NOT. (reply == 'n' .OR. reply == 'no')
    END FUNCTION prompt_resignation_acceptance

    SUBROUTINE apply_resign_setting(text, enabled, threshold_cp)
        CHARACTER(LEN=*), INTENT(IN) :: text
        LOGICAL, INTENT(INOUT) :: enabled
        INTEGER, INTENT(INOUT) :: threshold_cp
        INTEGER :: ios
        REAL :: threshold_pawns

        READ(text, *, IOSTAT=ios) threshold_pawns
        IF (ios /= 0) THEN
            enabled = .TRUE.
            threshold_cp = MAX(threshold_cp, 500)
            RETURN
        END IF

        IF (threshold_pawns <= 0.0) THEN
            enabled = .FALSE.
        ELSE
            enabled = .TRUE.
            threshold_cp = NINT(100.0 * threshold_pawns)
        END IF
    END SUBROUTINE apply_resign_setting

    LOGICAL FUNCTION verify_numeric_setting(text)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER :: ios
        REAL :: threshold_pawns

        READ(text, *, IOSTAT=ios) threshold_pawns
        verify_numeric_setting = (ios == 0)
    END FUNCTION verify_numeric_setting

END PROGRAM Fortran_Chess
