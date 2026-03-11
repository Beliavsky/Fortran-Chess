MODULE Game_State_Checker
    USE Chess_Types
    USE Board_Utils, ONLY: is_in_check, get_opponent_color
    USE Move_Generation, ONLY: generate_moves
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: is_game_over

CONTAINS

    ! --- Check if game is over (checkmate, stalemate, threefold repetition) ---
    ! Determines if the current board state represents a game over condition.
    !
    ! Parameters:
    !   board (INOUT): Current board state
    !   winner_color (OUT): Color of the winning player (WHITE, BLACK) or NO_COLOR for draw/ongoing
    !   game_status (OUT): Status of the game (GAME_CHECKMATE, GAME_STALEMATE,
    !                      GAME_THREEFOLD_REPETITION, GAME_ONGOING)
    !
    ! Returns:
    !   .TRUE. if the game is over, .FALSE. otherwise.
    LOGICAL FUNCTION is_game_over(board, winner_color, game_status, position_key_history, num_positions) RESULT(is_over)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(OUT) :: winner_color
        INTEGER, INTENT(OUT) :: game_status
        INTEGER(KIND=8), DIMENSION(:), INTENT(IN), OPTIONAL :: position_key_history
        INTEGER, INTENT(IN), OPTIONAL :: num_positions

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves_temp
        INTEGER :: num_legal_moves_temp
        INTEGER :: matching_positions

        ! Default values
        is_over = .FALSE.
        winner_color = NO_COLOR
        game_status = GAME_ONGOING

        IF (PRESENT(position_key_history) .AND. PRESENT(num_positions)) THEN
            matching_positions = count_matching_positions(board%zobrist_key, position_key_history, num_positions)
            IF (matching_positions >= 3) THEN
                game_status = GAME_THREEFOLD_REPETITION
                is_over = .TRUE.
                RETURN
            END IF
        END IF

        ! Generate legal moves for the current player
        CALL generate_moves(board, legal_moves_temp, num_legal_moves_temp)

        IF (num_legal_moves_temp == 0) THEN
            ! No legal moves, so it's either checkmate or stalemate
            IF (is_in_check(board, board%current_player)) THEN
                game_status = GAME_CHECKMATE
                winner_color = get_opponent_color(board%current_player)
            ELSE
                game_status = GAME_STALEMATE
                winner_color = NO_COLOR ! Draw
            END IF
            is_over = .TRUE.
        END IF

    END FUNCTION is_game_over

    INTEGER FUNCTION count_matching_positions(target_key, position_key_history, num_positions) RESULT(match_count)
        INTEGER(KIND=8), INTENT(IN) :: target_key
        INTEGER(KIND=8), DIMENSION(:), INTENT(IN) :: position_key_history
        INTEGER, INTENT(IN) :: num_positions
        INTEGER :: i, position_limit

        match_count = 0
        position_limit = MIN(num_positions, SIZE(position_key_history))
        DO i = 1, position_limit
            IF (position_key_history(i) == target_key) match_count = match_count + 1
        END DO
    END FUNCTION count_matching_positions

END MODULE Game_State_Checker
