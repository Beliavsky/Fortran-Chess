! ============================================
! Main Program: Human vs Computer
! ============================================
PROGRAM Fortran_Chess
    USE App_Defaults, ONLY: DEFAULT_SEARCH_DEPTH
    USE Chess_Types
    USE Board_Utils
    USE Evaluation, ONLY: evaluate_board
    USE Move_Generation ! Needs Make_Unmake implicitly
    USE Make_Unmake
    USE Search, ONLY: find_best_move, display_eval_white_cp
    USE Transposition_Table, ONLY: init_zobrist_keys
    USE User_Input_Processor, ONLY: get_human_move, print_in_game_help
    USE Game_State_Checker
    USE Game_Time_Utils, ONLY: parse_time_control, elapsed_milliseconds, &
        apply_clock_after_move, get_ai_time_budget_seconds, format_clock
    USE Notation_Utils, ONLY: move_to_san, move_to_coordinate, write_pgn_file, to_lower_string
    USE Opening_Book, ONLY: Opening_Book_Type, load_opening_book, choose_book_move, apply_opening_book_fix
    USE Opening_Sequence, ONLY: load_opening_sequence
    USE Search_Debug_Log, ONLY: start_ai_turn_debug_log, log_opening_book_hit, log_opening_book_miss, &
        finish_ai_turn_debug_log
    USE UCI_Driver, ONLY: run_uci_mode
    IMPLICIT NONE

    TYPE(Board_Type) :: game_board
    TYPE(Move_Type) :: chosen_move
    TYPE(Move_Type), DIMENSION(512) :: played_move_history
    TYPE(UnmakeInfo_Type), DIMENSION(512) :: unmake_history
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
    INTEGER(KIND=8), DIMENSION(512) :: move_elapsed_ms_history
    INTEGER(KIND=8), DIMENSION(512) :: white_time_before_move, black_time_before_move
    INTEGER(KIND=8), DIMENSION(513) :: position_key_history
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
    LOGICAL :: autoplay_requested, takeback_requested
    CHARACTER(LEN=260) :: opening_filename
    CHARACTER(LEN=256) :: opening_error
    LOGICAL :: batch_selfplay_mode
    LOGICAL :: batch_time_control_set
    LOGICAL :: blitz_mode
    LOGICAL :: verbose_mode
    INTEGER :: batch_game_count
    INTEGER :: batch_search_depth
    CHARACTER(LEN=260) :: batch_pgn_filename
    CHARACTER(LEN=32) :: batch_time_control_text
    LOGICAL :: time_control_enabled, game_lost_on_time
    INTEGER(KIND=8) :: white_time_ms, black_time_ms, increment_ms
    INTEGER(KIND=8) :: turn_start_count, turn_end_count, count_rate, elapsed_ms
    INTEGER(KIND=8) :: move_elapsed_ms
    INTEGER :: time_forfeit_winner_color
    INTEGER :: resignation_attempts
    INTEGER, PARAMETER :: MAX_RESIGNATION_ATTEMPTS = 3
    REAL :: ai_time_budget_seconds
    INTEGER :: takeback_count, takeback_index
    CHARACTER(LEN=32) :: pgn_time_control_tag
    CHARACTER(LEN=128) :: pgn_event_name, pgn_white_name, pgn_black_name
    INTEGER :: searched_best_score
    LOGICAL :: search_score_valid
    CHARACTER(LEN=32) :: pending_ai_move_text

    uci_mode = .FALSE.
    self_play_mode = .FALSE.
    batch_selfplay_mode = .FALSE.
    batch_time_control_set = .FALSE.
    blitz_mode = .FALSE.
    verbose_mode = .FALSE.
    batch_game_count = 0
    batch_search_depth = DEFAULT_SEARCH_DEPTH
    batch_pgn_filename = 'games.pgn'
    batch_time_control_text = ''
    show_eval_after_ai = .TRUE.
    resign_enabled = .TRUE.
    resign_threshold_cp = 2000
    game_resigned = .FALSE.
    num_half_moves = 0
    move_elapsed_ms_history = -1_8
    white_time_before_move = 0_8
    black_time_before_move = 0_8
    position_key_history = 0_8
    pgn_result = '*'
    current_game_status = GAME_ONGOING
    searched_best_score = 0
    search_score_valid = .FALSE.
    pending_ai_move_text = ''
    game_winner_color = NO_COLOR
    resignation_winner_color = NO_COLOR
    time_control_enabled = .FALSE.
    game_lost_on_time = .FALSE.
    white_time_ms = 0
    black_time_ms = 0
    increment_ms = 0
    time_forfeit_winner_color = NO_COLOR
    resignation_attempts = 0
    pgn_time_control_tag = '-'
    pgn_event_name = 'FortranChess Console Game'
    pgn_white_name = 'Human'
    pgn_black_name = 'Computer'
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
        ELSE IF (lower_arg1 == '--selfplay-batch' .OR. lower_arg1 == '-selfplay-batch') THEN
            IF (arg_idx >= argc) THEN
                PRINT *, "Missing game count after --selfplay-batch."
                CALL print_help()
                STOP
            END IF
            arg_idx = arg_idx + 1
            CALL GET_COMMAND_ARGUMENT(arg_idx, arg_value)
            IF (.NOT. parse_integer_argument(TRIM(arg_value), batch_game_count) .OR. batch_game_count <= 0) THEN
                PRINT *, "Invalid game count for --selfplay-batch: ", TRIM(arg_value)
                STOP
            END IF
            batch_selfplay_mode = .TRUE.
        ELSE IF (lower_arg1 == '--tc' .OR. lower_arg1 == '-tc') THEN
            IF (arg_idx >= argc) THEN
                PRINT *, "Missing time control after --tc."
                CALL print_help()
                STOP
            END IF
            arg_idx = arg_idx + 1
            CALL GET_COMMAND_ARGUMENT(arg_idx, batch_time_control_text)
            batch_time_control_set = .TRUE.
        ELSE IF (lower_arg1 == '--pgn' .OR. lower_arg1 == '-pgn') THEN
            IF (arg_idx >= argc) THEN
                PRINT *, "Missing filename after --pgn."
                CALL print_help()
                STOP
            END IF
            arg_idx = arg_idx + 1
            CALL GET_COMMAND_ARGUMENT(arg_idx, batch_pgn_filename)
        ELSE IF (lower_arg1 == '--depth' .OR. lower_arg1 == '-depth') THEN
            IF (arg_idx >= argc) THEN
                PRINT *, "Missing value after --depth."
                CALL print_help()
                STOP
            END IF
            arg_idx = arg_idx + 1
            CALL GET_COMMAND_ARGUMENT(arg_idx, arg_value)
            IF (.NOT. parse_integer_argument(TRIM(arg_value), batch_search_depth) .OR. batch_search_depth < 1 .OR. &
                batch_search_depth > 10) THEN
                PRINT *, "Invalid depth for --depth: ", TRIM(arg_value)
                STOP
            END IF
        ELSE IF (lower_arg1 == '--blitz' .OR. lower_arg1 == '-blitz') THEN
            blitz_mode = .TRUE.
        ELSE IF (lower_arg1 == '--verbose' .OR. lower_arg1 == '-verbose') THEN
            verbose_mode = .TRUE.
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

    CALL init_zobrist_keys()

    ! Verify any opening books before other console-mode startup work.
    IF (.NOT. verify_opening_book('White', 'book_white.txt', white_book)) STOP
    IF (.NOT. verify_opening_book('Black', 'book_black.txt', black_book)) STOP

    IF (batch_selfplay_mode) THEN
        CALL run_batch_selfplay()
        STOP
    END IF

    ! --- Initialize Zobrist Keys and Board ---
    CALL init_board(game_board)
    position_key_history(1) = game_board%zobrist_key
    IF (open_requested) THEN
        CALL load_opening_sequence(opening_filename, game_board, move_history, num_half_moves, opening_loaded, opening_error, &
            played_move_history, unmake_history, position_key_history)
        IF (.NOT. opening_loaded) THEN
            PRINT *, "Could not load opening sequence from " // TRIM(opening_filename)
            PRINT *, TRIM(opening_error)
            STOP
        END IF
        PRINT *, "Loaded opening sequence from " // TRIM(opening_filename)
    END IF

    search_depth = DEFAULT_SEARCH_DEPTH
    IF (blitz_mode) THEN
        human_player_color = WHITE
        ai_player_color = BLACK
        self_play_mode = .FALSE.
        time_control_enabled = .TRUE.
        white_time_ms = 180_8 * 1000_8
        black_time_ms = white_time_ms
        increment_ms = 2_8 * 1000_8
        pgn_time_control_tag = format_pgn_time_control_tag(white_time_ms, increment_ms)
        search_depth = DEFAULT_SEARCH_DEPTH
        show_eval_after_ai = .TRUE.
        resign_enabled = .TRUE.
        resign_threshold_cp = 2000
        CALL configure_console_pgn_headers()
        PRINT *, "Blitz preset: White, 3+2, eval on, resignation threshold 20 pawns."
    ELSE
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
        CALL configure_console_pgn_headers()

        ! --- Search Depth Selection ---
        DO
            PRINT *, "Time control for both sides (e.g., 3+2 for 3 minutes plus 2 seconds increment, blank for none): "
            READ(*, '(A)') settings_input
            settings_input = TRIM(ADJUSTL(to_lower_string(settings_input)))
            IF (LEN_TRIM(settings_input) == 0 .OR. settings_input == 'off' .OR. settings_input == 'none') THEN
                time_control_enabled = .FALSE.
                pgn_time_control_tag = '-'
                EXIT
            ELSE IF (parse_time_control(settings_input, white_time_ms, increment_ms)) THEN
                time_control_enabled = .TRUE.
                black_time_ms = white_time_ms
                pgn_time_control_tag = format_pgn_time_control_tag(white_time_ms, increment_ms)
                EXIT
            ELSE
                PRINT *, "Enter a time control like 3+2 or leave blank for none."
            END IF
        END DO

        DO
            IF (time_control_enabled) THEN
                PRINT '(A,I0,A)', "Enter AI max search depth (e.g., ", DEFAULT_SEARCH_DEPTH, &
                    ", actual think time comes from the shared clock): "
            ELSE
                PRINT '(A,I0,A)', "Enter AI max search depth (e.g., ", DEFAULT_SEARCH_DEPTH, &
                    ", still capped at 60 seconds per computer move): "
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
            PRINT *, "Resign threshold in pawns (default 20, type off for no resignation): "
            READ(*, '(A)') settings_input
            settings_input = TRIM(ADJUSTL(to_lower_string(settings_input)))
            IF (LEN_TRIM(settings_input) == 0) THEN
                resign_enabled = .TRUE.
                resign_threshold_cp = 2000
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
    END IF

    CALL print_board(game_board)

    ! --- Game Loop ---
    game_over = .FALSE.
    DO WHILE (.NOT. game_over)

        ! 1. Check Game Over
        game_over = is_game_over(game_board, game_winner_color, current_game_status, position_key_history, num_half_moves + 1)
        IF (game_over) THEN
            IF (LEN_TRIM(pending_ai_move_text) > 0) THEN
                PRINT *, TRIM(pending_ai_move_text)
                pending_ai_move_text = ''
            END IF
            SELECT CASE (current_game_status)
                CASE (GAME_CHECKMATE)
                    IF (game_winner_color == WHITE) THEN
                        PRINT *, "=== CHECKMATE! White wins! ==="
                    ELSE
                        PRINT *, "=== CHECKMATE! Black wins! ==="
                    END IF
                CASE (GAME_STALEMATE)
                    PRINT *, "=== STALEMATE! Draw. ==="
                CASE (GAME_THREEFOLD_REPETITION)
                    PRINT *, "=== THREEFOLD REPETITION! Draw. ==="
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
            IF (LEN_TRIM(pending_ai_move_text) > 0) THEN
                PRINT *, TRIM(pending_ai_move_text)
                pending_ai_move_text = ''
            END IF
            ! Generate the current legal moves before prompting the player
            CALL generate_moves(game_board, legal_moves, num_legal_moves)
            IF (num_legal_moves == 0) THEN
                PRINT *, "No legal moves available."
                game_over = .TRUE.
                CYCLE
            END IF

            CALL SYSTEM_CLOCK(turn_start_count)
            move_found = get_human_move(game_board, legal_moves, num_legal_moves, search_depth, move_history, &
                num_half_moves, white_book, black_book, show_eval_after_ai, resign_enabled, resign_threshold_cp, &
                verbose_mode, &
                chosen_move, game_over, autoplay_requested, takeback_requested)
            CALL SYSTEM_CLOCK(turn_end_count)
            move_elapsed_ms = elapsed_milliseconds(turn_start_count, turn_end_count, count_rate)
            IF (time_control_enabled) elapsed_ms = move_elapsed_ms

            IF (game_over) EXIT ! Exit game loop if user quit

            IF (autoplay_requested) THEN
                self_play_mode = .TRUE.
                human_player_color = NO_COLOR
                ai_player_color = NO_COLOR
                PRINT *, "Autoplay enabled."
                CYCLE
            END IF

            IF (takeback_requested) THEN
                CALL apply_console_takeback()
                CYCLE
            END IF

            IF (move_found) THEN
                 takeback_index = num_half_moves + 1
                 IF (takeback_index <= SIZE(move_history)) THEN
                     white_time_before_move(takeback_index) = white_time_ms
                     black_time_before_move(takeback_index) = black_time_ms
                 END IF
                 IF (time_control_enabled) THEN
                     IF (.NOT. apply_clock_after_move(game_board%current_player, elapsed_ms, white_time_ms, black_time_ms, increment_ms)) THEN
                         IF (handle_console_flag_fall(game_board%current_player, .TRUE.)) THEN
                             PRINT *, "Time expired. Continuing untimed."
                         ELSE
                             game_lost_on_time = .TRUE.
                             time_forfeit_winner_color = get_opponent_color(game_board%current_player)
                             game_over = .TRUE.
                             EXIT
                         END IF
                     END IF
                 END IF
                 move_text = move_to_san(game_board, chosen_move, legal_moves, num_legal_moves)
                 IF (num_half_moves < SIZE(move_history)) THEN
                     num_half_moves = num_half_moves + 1
                     move_history(num_half_moves) = TRIM(move_text)
                     move_elapsed_ms_history(num_half_moves) = move_elapsed_ms
                 END IF
                 PRINT *, "You played ", TRIM(move_text)
                 CALL make_move(game_board, chosen_move, move_info)
                 played_move_history(num_half_moves) = chosen_move
                 unmake_history(num_half_moves) = move_info
                 position_key_history(num_half_moves + 1) = game_board%zobrist_key
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

            CALL SYSTEM_CLOCK(turn_start_count)
            IF (active_book%valid) THEN
                book_move_found = choose_book_move(active_book, move_history, num_half_moves, game_board, &
                    legal_moves, num_legal_moves, chosen_move, book_move_text)
            END IF

            search_score_valid = .FALSE.
            ai_time_budget_seconds = get_ai_time_budget_seconds(game_board%current_player, white_time_ms, black_time_ms, &
                increment_ms, num_half_moves, time_control_enabled)
            CALL start_ai_turn_debug_log('console', game_board%current_player, move_history, num_half_moves, &
                search_depth, ai_time_budget_seconds)

            IF (book_move_found) THEN
                move_found = .TRUE.
                PRINT *, "Computer found a book move."
                CALL log_opening_book_hit(active_book%filename, book_move_text, &
                    move_to_san(game_board, chosen_move, legal_moves, num_legal_moves), move_to_coordinate(chosen_move))
            ELSE
                IF (active_book%valid) CALL log_opening_book_miss(active_book%filename)
                CALL find_best_move(game_board, search_depth, move_found, chosen_move, &
                    best_score_out=searched_best_score, &
                    time_limit_seconds=ai_time_budget_seconds, &
                    show_countdown=.TRUE.)
                search_score_valid = move_found
                CALL generate_moves(game_board, legal_moves, num_legal_moves)
            END IF
            CALL SYSTEM_CLOCK(turn_end_count)
            move_elapsed_ms = elapsed_milliseconds(turn_start_count, turn_end_count, count_rate)
            IF (time_control_enabled) THEN
                elapsed_ms = move_elapsed_ms
            ELSE
                elapsed_ms = 0_8
            END IF
            IF (move_found) THEN
                IF (.NOT. book_move_found .AND. search_score_valid .AND. resign_enabled .AND. &
                    resignation_attempts < MAX_RESIGNATION_ATTEMPTS .AND. searched_best_score <= -resign_threshold_cp) THEN
                    resignation_attempts = resignation_attempts + 1
                    accept_resignation = .TRUE.
                    IF (.NOT. self_play_mode) THEN
                        accept_resignation = prompt_resignation_acceptance(game_board%current_player)
                    END IF
                    IF (accept_resignation) THEN
                        CALL finish_ai_turn_debug_log('Search score below resignation threshold. Resigned.')
                        game_resigned = .TRUE.
                        resignation_winner_color = get_opponent_color(game_board%current_player)
                        game_over = .TRUE.
                        EXIT
                    ELSE
                        PRINT *, "Resignation refused. Play continues."
                    END IF
                END IF
                takeback_index = num_half_moves + 1
                IF (takeback_index <= SIZE(move_history)) THEN
                    white_time_before_move(takeback_index) = white_time_ms
                    black_time_before_move(takeback_index) = black_time_ms
                END IF
                IF (time_control_enabled) THEN
                    IF (book_move_found) THEN
                        elapsed_ms = 0_8
                    END IF
                    IF (.NOT. apply_clock_after_move(game_board%current_player, elapsed_ms, white_time_ms, black_time_ms, increment_ms)) THEN
                        IF (handle_console_flag_fall(game_board%current_player, .FALSE.)) THEN
                            CALL finish_ai_turn_debug_log('Computer flagged, but user continued untimed.')
                            PRINT *, "Time expired. Continuing untimed."
                        ELSE
                            game_lost_on_time = .TRUE.
                            time_forfeit_winner_color = get_opponent_color(game_board%current_player)
                            CALL finish_ai_turn_debug_log('Computer lost on time before completing the move.')
                            game_over = .TRUE.
                            EXIT
                        END IF
                    END IF
                END IF
                move_text = move_to_san(game_board, chosen_move, legal_moves, num_legal_moves)
                IF (num_half_moves < SIZE(move_history)) THEN
                    num_half_moves = num_half_moves + 1
                    move_history(num_half_moves) = TRIM(move_text)
                    move_elapsed_ms_history(num_half_moves) = move_elapsed_ms
                END IF
                pending_ai_move_text = format_console_move_label(num_half_moves, game_board%current_player, move_text)
                CALL make_move(game_board, chosen_move, move_info)
                played_move_history(num_half_moves) = chosen_move
                unmake_history(num_half_moves) = move_info
                position_key_history(num_half_moves + 1) = game_board%zobrist_key
                IF (book_move_found) THEN
                    CALL finish_ai_turn_debug_log('Opening-book move played.')
                ELSE
                    CALL finish_ai_turn_debug_log('Search move played.')
                END IF
                IF (show_eval_after_ai) THEN
                    CALL print_position_evaluation(game_board)
                END IF
            ELSE
                ! Should be caught by game over check, but safety print
                PRINT *, "Error: AI found no move but game not over?"
                CALL finish_ai_turn_debug_log('No move found.')
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
        CASE (GAME_THREEFOLD_REPETITION)
            pgn_result = '1/2-1/2'
        CASE DEFAULT
            pgn_result = '*'
        END SELECT
    END IF

    CALL write_pgn_file('games.pgn', move_history, num_half_moves, pgn_result, move_elapsed_ms_history, pgn_time_control_tag, &
        pgn_event_name, pgn_white_name, pgn_black_name)
    PRINT *, "Saved game to games.pgn"
    PRINT *, "Game finished."

CONTAINS

    LOGICAL FUNCTION handle_console_flag_fall(flagged_player_color, flagged_human) RESULT(continued)
        INTEGER, INTENT(IN) :: flagged_player_color
        LOGICAL, INTENT(IN) :: flagged_human
        CHARACTER(LEN=64) :: player_name
        CHARACTER(LEN=32) :: reply

        continued = .FALSE.
        IF (self_play_mode .OR. batch_selfplay_mode) RETURN

        IF (flagged_human) THEN
            player_name = 'Your'
        ELSE IF (flagged_player_color == WHITE) THEN
            player_name = 'White''s'
        ELSE
            player_name = 'Black''s'
        END IF

        PRINT *, TRIM(player_name), " time expired. Continue untimed? (y/n)"
        READ(*,'(A)') reply
        reply = TRIM(ADJUSTL(to_lower_string(reply)))
        IF (reply == 'y' .OR. reply == 'yes') THEN
            continued = .TRUE.
            time_control_enabled = .FALSE.
            game_lost_on_time = .FALSE.
            time_forfeit_winner_color = NO_COLOR
        END IF
    END FUNCTION handle_console_flag_fall

    SUBROUTINE apply_console_takeback()
        INTEGER :: undone_count

        undone_count = 0
        IF (self_play_mode) THEN
            takeback_count = MIN(1, num_half_moves)
        ELSE
            takeback_count = MIN(2, num_half_moves)
        END IF

        IF (takeback_count <= 0) THEN
            PRINT *, "No moves available to take back."
            RETURN
        END IF

        DO WHILE (takeback_count > 0)
            CALL unmake_move(game_board, played_move_history(num_half_moves), unmake_history(num_half_moves))
            white_time_ms = white_time_before_move(num_half_moves)
            black_time_ms = black_time_before_move(num_half_moves)
            move_history(num_half_moves) = ''
            move_elapsed_ms_history(num_half_moves) = -1_8
            num_half_moves = num_half_moves - 1
            takeback_count = takeback_count - 1
            undone_count = undone_count + 1
        END DO

        game_over = .FALSE.
        game_lost_on_time = .FALSE.
        game_resigned = .FALSE.
        current_game_status = GAME_ONGOING
        game_winner_color = NO_COLOR
        resignation_winner_color = NO_COLOR
        time_forfeit_winner_color = NO_COLOR
        PRINT *, "Took back ", undone_count, " move(s)."
        CALL print_board(game_board)
    END SUBROUTINE apply_console_takeback

    FUNCTION format_pgn_time_control_tag(initial_ms, increment_ms_value) RESULT(tag)
        INTEGER(KIND=8), INTENT(IN) :: initial_ms, increment_ms_value
        CHARACTER(LEN=32) :: tag
        INTEGER(KIND=8) :: initial_seconds, increment_seconds

        initial_seconds = MAX(0_8, initial_ms / 1000_8)
        increment_seconds = MAX(0_8, increment_ms_value / 1000_8)
        WRITE(tag, '(I0,A,I0)') initial_seconds, '+', increment_seconds
    END FUNCTION format_pgn_time_control_tag

    SUBROUTINE report_book_status(book, side_name)
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        CHARACTER(LEN=*), INTENT(IN) :: side_name

        IF (.NOT. book%found) RETURN

        IF (book%valid) THEN
            PRINT *, TRIM(side_name) // " opening book loaded from " // TRIM(book%filename)
        ELSE
            PRINT *, "Opening book syntax error in " // TRIM(book%filename)
            IF (book%error_line > 0) PRINT *, "Line ", book%error_line
            IF (LEN_TRIM(book%error_text) > 0) PRINT *, "Bad line: " // TRIM(book%error_text)
            IF (LEN_TRIM(book%error_message) > 0) PRINT *, TRIM(book%error_message)
            IF (LEN_TRIM(book%suggestion) > 0) PRINT *, "Suggestion: " // TRIM(book%suggestion)
            IF (LEN_TRIM(book%replacement_line) > 0) PRINT *, "Possible replacement: " // TRIM(book%replacement_line)
        END IF
    END SUBROUTINE report_book_status

    LOGICAL FUNCTION verify_opening_book(side_name, filename, book) RESULT(ok)
        CHARACTER(LEN=*), INTENT(IN) :: side_name, filename
        TYPE(Opening_Book_Type), INTENT(OUT) :: book

        CHARACTER(LEN=32) :: response
        CHARACTER(LEN=256) :: fix_error
        LOGICAL :: fix_applied

        ok = .FALSE.
        DO
            CALL load_opening_book(filename, book)
            IF (.NOT. book%found) THEN
                ok = .TRUE.
                RETURN
            END IF
            IF (book%valid) THEN
                CALL report_book_status(book, side_name)
                ok = .TRUE.
                RETURN
            END IF

            CALL report_book_status(book, side_name)
            IF (LEN_TRIM(book%replacement_line) == 0) THEN
                PRINT *, "No automatic fix is available. Exiting."
                RETURN
            END IF

            DO
                PRINT *, "Apply this fix and overwrite " // TRIM(book%filename) // "? (Y/n): "
                READ(*, '(A)') response
                response = TRIM(ADJUSTL(to_lower_string(response)))
                IF (LEN_TRIM(response) == 0 .OR. response == 'y' .OR. response == 'yes') THEN
                    CALL apply_opening_book_fix(book, fix_applied, fix_error)
                    IF (.NOT. fix_applied) THEN
                        PRINT *, "Failed to update " // TRIM(book%filename)
                        IF (LEN_TRIM(fix_error) > 0) PRINT *, TRIM(fix_error)
                        RETURN
                    END IF
                    PRINT *, "Updated " // TRIM(book%filename) // ". Rechecking."
                    EXIT
                ELSE IF (response == 'n' .OR. response == 'no') THEN
                    PRINT *, "Opening book fix declined. Exiting."
                    RETURN
                ELSE
                    PRINT *, "Please answer Y or N."
                END IF
            END DO
        END DO
    END FUNCTION verify_opening_book

    SUBROUTINE run_batch_selfplay()
        INTEGER :: game_idx
        INTEGER :: white_wins, black_wins, draws

        IF (.NOT. batch_time_control_set) THEN
            PRINT *, "Batch self-play requires --tc, for example --tc 3+2."
            STOP
        END IF

        white_wins = 0
        black_wins = 0
        draws = 0
        show_eval_after_ai = .FALSE.
        self_play_mode = .TRUE.
        human_player_color = NO_COLOR
        ai_player_color = NO_COLOR
        search_depth = batch_search_depth
        pgn_event_name = 'FortranChess Self-Play Batch'
        pgn_white_name = 'Computer'
        pgn_black_name = 'Computer'

        CALL init_zobrist_keys()

        DO game_idx = 1, batch_game_count
            CALL initialize_batch_game()
            PRINT '(A,I0,A,I0,A)', "Batch self-play game ", game_idx, " of ", batch_game_count, "..."
            CALL play_batch_game()
            CALL write_pgn_file(TRIM(batch_pgn_filename), move_history, num_half_moves, pgn_result, &
                move_elapsed_ms_history, pgn_time_control_tag, pgn_event_name, pgn_white_name, pgn_black_name)
            CALL update_batch_summary(pgn_result, white_wins, black_wins, draws)
            PRINT *, "Result: ", TRIM(pgn_result)
        END DO

        PRINT *, ""
        PRINT *, "Batch self-play complete."
        PRINT *, "PGN file: ", TRIM(batch_pgn_filename)
        PRINT *, "White wins: ", white_wins
        PRINT *, "Black wins: ", black_wins
        PRINT *, "Draws: ", draws
    END SUBROUTINE run_batch_selfplay

    SUBROUTINE initialize_batch_game()
        INTEGER :: batch_takeback_index

        game_over = .FALSE.
        game_resigned = .FALSE.
        game_lost_on_time = .FALSE.
        current_game_status = GAME_ONGOING
        game_winner_color = NO_COLOR
        resignation_winner_color = NO_COLOR
        time_forfeit_winner_color = NO_COLOR
        resignation_attempts = 0
        num_half_moves = 0
        move_history = ''
        move_elapsed_ms_history = -1_8
        white_time_before_move = 0_8
        black_time_before_move = 0_8
        position_key_history = 0_8
        pgn_result = '*'
        opening_error = ''
        white_time_ms = 0_8
        black_time_ms = 0_8
        increment_ms = 0_8
        pgn_time_control_tag = '-'

        CALL init_board(game_board)
        position_key_history(1) = game_board%zobrist_key

        IF (open_requested) THEN
            CALL load_opening_sequence(opening_filename, game_board, move_history, num_half_moves, opening_loaded, opening_error, &
                played_move_history, unmake_history, position_key_history)
            IF (.NOT. opening_loaded) THEN
                PRINT *, "Could not load opening sequence from " // TRIM(opening_filename)
                PRINT *, TRIM(opening_error)
                STOP
            END IF
        END IF

        DO batch_takeback_index = 1, num_half_moves
            white_time_before_move(batch_takeback_index) = 0_8
            black_time_before_move(batch_takeback_index) = 0_8
            move_elapsed_ms_history(batch_takeback_index) = 0_8
        END DO

        IF (.NOT. parse_time_control(TRIM(batch_time_control_text), white_time_ms, increment_ms)) THEN
            PRINT *, "Invalid batch time control: ", TRIM(batch_time_control_text)
            STOP
        END IF
        black_time_ms = white_time_ms
        time_control_enabled = .TRUE.
        pgn_time_control_tag = format_pgn_time_control_tag(white_time_ms, increment_ms)
    END SUBROUTINE initialize_batch_game

    SUBROUTINE play_batch_game()
        DO WHILE (.NOT. game_over)
            game_over = is_game_over(game_board, game_winner_color, current_game_status, position_key_history, num_half_moves + 1)
            IF (game_over) EXIT

            CALL play_one_engine_turn(.TRUE.)
        END DO

        CALL finalize_current_pgn_result()
    END SUBROUTINE play_batch_game

    SUBROUTINE play_one_engine_turn(quiet_mode)
        LOGICAL, INTENT(IN) :: quiet_mode

        CALL generate_moves(game_board, legal_moves, num_legal_moves)
        book_move_found = .FALSE.
        book_move_text = ''

        IF (game_board%current_player == WHITE) THEN
            active_book = white_book
        ELSE
            active_book = black_book
        END IF

        CALL SYSTEM_CLOCK(turn_start_count)
        IF (active_book%valid) THEN
            book_move_found = choose_book_move(active_book, move_history, num_half_moves, game_board, &
                legal_moves, num_legal_moves, chosen_move, book_move_text)
        END IF

        search_score_valid = .FALSE.
        ai_time_budget_seconds = get_ai_time_budget_seconds(game_board%current_player, white_time_ms, black_time_ms, &
            increment_ms, num_half_moves, time_control_enabled)
        CALL start_ai_turn_debug_log('console-batch', game_board%current_player, move_history, num_half_moves, &
            search_depth, ai_time_budget_seconds)

        IF (book_move_found) THEN
            move_found = .TRUE.
            CALL log_opening_book_hit(active_book%filename, book_move_text, &
                move_to_san(game_board, chosen_move, legal_moves, num_legal_moves), move_to_coordinate(chosen_move))
        ELSE
            IF (active_book%valid) CALL log_opening_book_miss(active_book%filename)
            CALL find_best_move(game_board, search_depth, move_found, chosen_move, &
                best_score_out=searched_best_score, &
                time_limit_seconds=ai_time_budget_seconds, show_countdown=.FALSE.)
            search_score_valid = move_found
            CALL generate_moves(game_board, legal_moves, num_legal_moves)
        END IF

        CALL SYSTEM_CLOCK(turn_end_count)
        move_elapsed_ms = elapsed_milliseconds(turn_start_count, turn_end_count, count_rate)
        elapsed_ms = move_elapsed_ms

        IF (.NOT. move_found) THEN
            CALL finish_ai_turn_debug_log('No move found.')
            game_over = .TRUE.
            RETURN
        END IF

        IF (.NOT. book_move_found .AND. search_score_valid .AND. resign_enabled .AND. &
            resignation_attempts < MAX_RESIGNATION_ATTEMPTS .AND. searched_best_score <= -resign_threshold_cp) THEN
            resignation_attempts = resignation_attempts + 1
            CALL finish_ai_turn_debug_log('Search score below resignation threshold. Resigned.')
            game_resigned = .TRUE.
            resignation_winner_color = get_opponent_color(game_board%current_player)
            game_over = .TRUE.
            RETURN
        END IF

        takeback_index = num_half_moves + 1
        IF (takeback_index <= SIZE(move_history)) THEN
            white_time_before_move(takeback_index) = white_time_ms
            black_time_before_move(takeback_index) = black_time_ms
        END IF

        IF (book_move_found) elapsed_ms = 0_8
        IF (.NOT. apply_clock_after_move(game_board%current_player, elapsed_ms, white_time_ms, black_time_ms, increment_ms)) THEN
            game_lost_on_time = .TRUE.
            time_forfeit_winner_color = get_opponent_color(game_board%current_player)
            CALL finish_ai_turn_debug_log('Computer lost on time before completing the move.')
            game_over = .TRUE.
            RETURN
        END IF

        move_text = move_to_san(game_board, chosen_move, legal_moves, num_legal_moves)
        IF (num_half_moves < SIZE(move_history)) THEN
            num_half_moves = num_half_moves + 1
            move_history(num_half_moves) = TRIM(move_text)
            move_elapsed_ms_history(num_half_moves) = move_elapsed_ms
        END IF
        IF (.NOT. quiet_mode) PRINT *, "Computer played ", TRIM(move_text)
        CALL make_move(game_board, chosen_move, move_info)
        played_move_history(num_half_moves) = chosen_move
        unmake_history(num_half_moves) = move_info
        position_key_history(num_half_moves + 1) = game_board%zobrist_key

        IF (book_move_found) THEN
            CALL finish_ai_turn_debug_log('Opening-book move played.')
        ELSE
            CALL finish_ai_turn_debug_log('Search move played.')
        END IF
    END SUBROUTINE play_one_engine_turn

    SUBROUTINE finalize_current_pgn_result()
        IF (game_lost_on_time) THEN
            IF (time_forfeit_winner_color == WHITE) THEN
                pgn_result = '1-0'
            ELSE
                pgn_result = '0-1'
            END IF
        ELSE IF (game_resigned) THEN
            IF (resignation_winner_color == WHITE) THEN
                pgn_result = '1-0'
            ELSE
                pgn_result = '0-1'
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
            CASE (GAME_THREEFOLD_REPETITION)
                pgn_result = '1/2-1/2'
            CASE DEFAULT
                pgn_result = '*'
            END SELECT
        END IF
    END SUBROUTINE finalize_current_pgn_result

    SUBROUTINE update_batch_summary(result_text, white_wins, black_wins, draws)
        CHARACTER(LEN=*), INTENT(IN) :: result_text
        INTEGER, INTENT(INOUT) :: white_wins, black_wins, draws

        SELECT CASE (TRIM(result_text))
        CASE ('1-0')
            white_wins = white_wins + 1
        CASE ('0-1')
            black_wins = black_wins + 1
        CASE ('1/2-1/2')
            draws = draws + 1
        END SELECT
    END SUBROUTINE update_batch_summary

    SUBROUTINE configure_console_pgn_headers()
        pgn_event_name = 'FortranChess Console Game'
        IF (self_play_mode) THEN
            pgn_white_name = 'Computer'
            pgn_black_name = 'Computer'
        ELSE IF (human_player_color == WHITE) THEN
            pgn_white_name = 'Human'
            pgn_black_name = 'Computer'
        ELSE
            pgn_white_name = 'Computer'
            pgn_black_name = 'Human'
        END IF
    END SUBROUTINE configure_console_pgn_headers

    LOGICAL FUNCTION parse_integer_argument(text, value)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER, INTENT(OUT) :: value
        INTEGER :: ios

        READ(text, *, IOSTAT=ios) value
        parse_integer_argument = (ios == 0)
    END FUNCTION parse_integer_argument

    CHARACTER(LEN=32) FUNCTION format_console_move_label(half_move_number, moving_color, move_san) RESULT(label)
        INTEGER, INTENT(IN) :: half_move_number, moving_color
        CHARACTER(LEN=*), INTENT(IN) :: move_san
        INTEGER :: full_move_number

        full_move_number = (half_move_number + 1) / 2
        IF (moving_color == WHITE) THEN
            WRITE(label, '(I0,A,A)') full_move_number, '.', TRIM(move_san)
        ELSE
            WRITE(label, '(I0,A,A)') full_move_number, '...', TRIM(move_san)
        END IF
    END FUNCTION format_console_move_label

    SUBROUTINE print_help()
        PRINT *, "FortranChess"
        PRINT *, "Usage:"
        PRINT *, "  chess               Start console mode"
        PRINT *, "  chess --blitz       Start console mode as White with 3+2, eval on"
        PRINT *, "  chess --verbose     Use the old verbose console prompt/output"
        PRINT *, "  chess --uci         Start UCI mode"
        PRINT *, "  chess --open FILE   Start from PGN movetext in FILE"
        PRINT *, "  chess --selfplay-batch N --tc M+I [--depth N] [--pgn FILE] [--open FILE]"
        PRINT *, "  chess --help        Show this help"
        PRINT *, "  chess -help"
        PRINT *, "  chess help"
        PRINT *, ""
        PRINT *, "Console setup:"
        PRINT *, "  Choose White, Black, or N for self-play."
        PRINT *, "  --blitz skips prompts and uses White, 3+2, eval on, resign at 20 pawns."
        PRINT *, "  --verbose keeps the old 'Your turn' prompt."
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

    SUBROUTINE print_position_evaluation(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: white_eval_cp

        white_eval_cp = display_eval_white_cp(board)
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
            threshold_cp = MAX(threshold_cp, 2000)
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
