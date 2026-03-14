MODULE User_Input_Processor
    USE Chess_Types
    USE Notation_Utils, ONLY: move_matches_input, to_lower_string
    USE Opening_Book, ONLY: Opening_Book_Type
    USE Move_Suggestion, ONLY: suggest_move_for_position
    USE Search, ONLY: display_eval_white_cp
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: get_human_move, print_in_game_help

CONTAINS

    ! --- Get Human Move ---
    ! Prompts the human player for a move, parses it, validates it against legal moves,
    ! and returns the chosen move. Handles input validation and "quit" command.
    !
    ! Parameters:
    !   game_board (IN): Current board state
    !   legal_moves (IN): Array of currently legal moves
    !   num_legal_moves (IN): Number of legal moves in the array
    !   chosen_move (OUT): The validated move chosen by the player
    !   game_over_flag (INOUT): Set to .TRUE. if the player quits
    !
    ! Returns:
    !   LOGICAL: .TRUE. if a valid move was found, .FALSE. otherwise (e.g., quit, invalid input)
    LOGICAL FUNCTION get_human_move(board, legal_moves, num_legal_moves, search_depth, move_history, num_half_moves, &
                                    white_book, black_book, show_eval_after_ai, resign_enabled, resign_threshold_cp, &
                                    verbose_prompt, chosen_move, game_over_flag, autoplay_requested, takeback_requested)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        INTEGER, INTENT(IN) :: search_depth
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        INTEGER, INTENT(IN) :: num_half_moves
        TYPE(Opening_Book_Type), INTENT(IN) :: white_book, black_book
        LOGICAL, INTENT(INOUT) :: show_eval_after_ai
        LOGICAL, INTENT(INOUT) :: resign_enabled
        INTEGER, INTENT(INOUT) :: resign_threshold_cp
        LOGICAL, INTENT(IN) :: verbose_prompt
        TYPE(Move_Type), INTENT(OUT) :: chosen_move
        LOGICAL, INTENT(INOUT) :: game_over_flag
        LOGICAL, INTENT(OUT) :: autoplay_requested, takeback_requested

        CHARACTER(LEN=64) :: user_input
        CHARACTER(LEN=64) :: lower_input
        CHARACTER(LEN=64) :: command_arg
        CHARACTER(LEN=32) :: hint_text
        LOGICAL :: move_found_internal
        INTEGER :: i
        INTEGER :: white_score_cp
        INTEGER :: ios
        REAL :: threshold_pawns
        TYPE(Move_Type) :: hint_move
        LOGICAL :: hint_found
        CHARACTER(LEN=32) :: suggestion_source

        get_human_move = .FALSE. ! Default to no valid move found
        move_found_internal = .FALSE.
        game_over_flag = .FALSE. ! Default to not game over
        autoplay_requested = .FALSE.
        takeback_requested = .FALSE.

        IF (verbose_prompt) THEN
            PRINT *, "" ! Newline
        PRINT *, "Your turn. Enter move (e.g., d4, Nf6, cxd5, dc, O-O, e2e4, score, suggest, autoplay, or takeback): "
        END IF

        DO WHILE (.NOT. move_found_internal .AND. .NOT. game_over_flag)
            READ(*, '(A)') user_input
            lower_input = TRIM(ADJUSTL(to_lower_string(user_input)))

            IF (lower_input == 'quit' .OR. lower_input == 'exit') THEN
                PRINT *, "Exiting game."
                game_over_flag = .TRUE.
                RETURN
            END IF

            IF (lower_input == 'autoplay') THEN
                autoplay_requested = .TRUE.
                RETURN
            END IF

            IF (lower_input == 'takeback' .OR. lower_input == 'undo') THEN
                takeback_requested = .TRUE.
                RETURN
            END IF

            IF (lower_input == 'score') THEN
                white_score_cp = display_eval_white_cp(board)
                WRITE(*, '(A,F7.2,A)') "Score (White perspective): ", REAL(white_score_cp) / 100.0, " pawns"
                CYCLE
            END IF

            IF (lower_input == '?' .OR. lower_input == 'suggest') THEN
                CALL suggest_move_for_position(board, legal_moves, num_legal_moves, move_history, num_half_moves, &
                    white_book, black_book, search_depth, hint_found, hint_move, hint_text, suggestion_source)
                IF (hint_found) THEN
                    PRINT *, "Suggested move: ", TRIM(hint_text), " (", TRIM(suggestion_source), ")"
                ELSE
                    PRINT *, "No suggestion available."
                END IF
                CYCLE
            END IF

            IF (lower_input == 'help') THEN
                CALL print_in_game_help()
                CYCLE
            END IF

            IF (INDEX(lower_input, 'eval') == 1) THEN
                command_arg = TRIM(ADJUSTL(lower_input(5:)))
                IF (LEN_TRIM(command_arg) == 0) THEN
                    IF (show_eval_after_ai) THEN
                        PRINT *, "Evaluation display is on."
                    ELSE
                        PRINT *, "Evaluation display is off."
                    END IF
                ELSE IF (command_arg == 'on') THEN
                    show_eval_after_ai = .TRUE.
                    PRINT *, "Evaluation display turned on."
                ELSE IF (command_arg == 'off') THEN
                    show_eval_after_ai = .FALSE.
                    PRINT *, "Evaluation display turned off."
                ELSE
                    PRINT *, "Use `eval on` or `eval off`."
                END IF
                CYCLE
            END IF

            IF (INDEX(lower_input, 'resign') == 1) THEN
                command_arg = TRIM(ADJUSTL(lower_input(7:)))
                IF (LEN_TRIM(command_arg) == 0) THEN
                    IF (resign_enabled) THEN
                        WRITE(*, '(A,F6.2,A)') "Resignation threshold: ", REAL(resign_threshold_cp) / 100.0, " pawns"
                    ELSE
                        PRINT *, "Resignation is off."
                    END IF
                    CYCLE
                END IF

                IF (command_arg == 'off' .OR. command_arg == 'none') THEN
                    resign_enabled = .FALSE.
                    PRINT *, "Resignation disabled."
                    CYCLE
                END IF

                IF (command_arg == 'on') THEN
                    resign_enabled = .TRUE.
                    IF (resign_threshold_cp <= 0) resign_threshold_cp = 500
                    WRITE(*, '(A,F6.2,A)') "Resignation enabled at ", REAL(resign_threshold_cp) / 100.0, " pawns"
                    CYCLE
                END IF

                READ(command_arg, *, IOSTAT=ios) threshold_pawns
                IF (ios /= 0) THEN
                    PRINT *, "Use `resign off`, `resign on`, or `resign 5.0`."
                ELSE IF (threshold_pawns <= 0.0) THEN
                    resign_enabled = .FALSE.
                    PRINT *, "Resignation disabled."
                ELSE
                    resign_enabled = .TRUE.
                    resign_threshold_cp = NINT(100.0 * threshold_pawns)
                    WRITE(*, '(A,F6.2,A)') "Resignation threshold set to ", REAL(resign_threshold_cp) / 100.0, " pawns"
                END IF
                CYCLE
            END IF

            DO i = 1, num_legal_moves
                IF (move_matches_input(board, legal_moves(i), legal_moves, num_legal_moves, user_input)) THEN
                    chosen_move = legal_moves(i)
                    move_found_internal = .TRUE.
                    EXIT
                END IF
            END DO

            IF (.NOT. move_found_internal .AND. LEN_TRIM(user_input) == 0) THEN
                CYCLE
            END IF

            IF (.NOT. move_found_internal) THEN
                 PRINT *, "Invalid or illegal move. Try again."
            END IF
        END DO ! End move input loop

        IF (move_found_internal) THEN
            get_human_move = .TRUE.
        END IF

    END FUNCTION get_human_move

    SUBROUTINE print_in_game_help()
        PRINT *, "Commands:"
        PRINT *, "  move        Play a move in SAN, coordinate, or unambiguous pawn-capture shorthand like dc"
        PRINT *, "  ?           Suggest a move"
        PRINT *, "  suggest     Suggest a move"
        PRINT *, "  autoplay    Let the computer play both sides from here"
        PRINT *, "  score       Show the current evaluation from White's perspective"
        PRINT *, "  takeback    Take back the last move (or move pair against the engine)"
        PRINT *, "  eval on/off Turn evaluation display after computer moves on or off"
        PRINT *, "  resign X    Set resignation threshold to X pawns"
        PRINT *, "  resign off  Disable resignation"
        PRINT *, "  resign on   Re-enable resignation with the current threshold"
        PRINT *, "  help        Show this help"
        PRINT *, "  quit        End the game"
    END SUBROUTINE print_in_game_help

END MODULE User_Input_Processor
