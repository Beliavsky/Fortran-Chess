MODULE Move_Suggestion
    USE Chess_Types
    USE Notation_Utils, ONLY: move_to_san
    USE Opening_Book, ONLY: Opening_Book_Type, choose_book_move
    USE Move_Generation, ONLY: generate_moves
    USE Search, ONLY: find_best_move
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: suggest_move_for_position

CONTAINS

    SUBROUTINE suggest_move_for_position(board, legal_moves, num_legal_moves, move_history, num_half_moves, &
        white_book, black_book, search_depth, suggestion_found, suggestion_move, suggestion_san, suggestion_source, &
        allow_opening_book, time_limit_seconds)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves, num_half_moves, search_depth
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        TYPE(Opening_Book_Type), INTENT(IN) :: white_book, black_book
        LOGICAL, INTENT(OUT) :: suggestion_found
        TYPE(Move_Type), INTENT(OUT) :: suggestion_move
        CHARACTER(LEN=*), INTENT(OUT) :: suggestion_san, suggestion_source
        LOGICAL, INTENT(IN), OPTIONAL :: allow_opening_book
        REAL, INTENT(IN), OPTIONAL :: time_limit_seconds

        TYPE(Opening_Book_Type) :: active_book
        TYPE(Board_Type) :: board_copy
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: local_legal_moves
        INTEGER :: local_num_legal_moves
        CHARACTER(LEN=32) :: book_move_text
        LOGICAL :: use_opening_book

        suggestion_found = .FALSE.
        suggestion_san = ''
        suggestion_source = ''
        book_move_text = ''
        suggestion_move%from_sq%rank = 0
        suggestion_move%from_sq%file = 0
        suggestion_move%to_sq%rank = 0
        suggestion_move%to_sq%file = 0
        ! Suggestions regenerate legal moves locally so they stay valid after --open setups.
        IF (num_legal_moves > 0) suggestion_move = legal_moves(1)

        board_copy = board
        CALL generate_moves(board_copy, local_legal_moves, local_num_legal_moves)
        IF (local_num_legal_moves <= 0) RETURN

        IF (board%current_player == WHITE) THEN
            active_book = white_book
        ELSE
            active_book = black_book
        END IF

        use_opening_book = .TRUE.
        IF (PRESENT(allow_opening_book)) use_opening_book = allow_opening_book

        IF (use_opening_book .AND. active_book%valid) THEN
            suggestion_found = choose_book_move(active_book, move_history, num_half_moves, board, local_legal_moves, &
                local_num_legal_moves, suggestion_move, book_move_text)
            IF (suggestion_found) THEN
                suggestion_san = move_to_san(board, suggestion_move, local_legal_moves, local_num_legal_moves)
                suggestion_source = 'opening book'
                RETURN
            END IF
        END IF

        IF (PRESENT(time_limit_seconds)) THEN
            CALL find_best_move(board_copy, search_depth, suggestion_found, suggestion_move, &
                time_limit_seconds=time_limit_seconds)
        ELSE
            CALL find_best_move(board_copy, search_depth, suggestion_found, suggestion_move)
        END IF
        IF (suggestion_found) THEN
            suggestion_san = move_to_san(board, suggestion_move, local_legal_moves, local_num_legal_moves)
            suggestion_source = 'search'
        END IF
    END SUBROUTINE suggest_move_for_position

END MODULE Move_Suggestion
