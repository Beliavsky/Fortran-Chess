MODULE Move_Suggestion
    USE Chess_Types
    USE Notation_Utils, ONLY: move_to_san
    USE Opening_Book, ONLY: Opening_Book_Type, choose_book_move
    USE Search, ONLY: find_best_move
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: suggest_move_for_position

CONTAINS

    SUBROUTINE suggest_move_for_position(board, legal_moves, num_legal_moves, move_history, num_half_moves, &
        white_book, black_book, search_depth, suggestion_found, suggestion_move, suggestion_san, suggestion_source, &
        allow_opening_book)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves, num_half_moves, search_depth
        CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: move_history
        TYPE(Opening_Book_Type), INTENT(IN) :: white_book, black_book
        LOGICAL, INTENT(OUT) :: suggestion_found
        TYPE(Move_Type), INTENT(OUT) :: suggestion_move
        CHARACTER(LEN=*), INTENT(OUT) :: suggestion_san, suggestion_source
        LOGICAL, INTENT(IN), OPTIONAL :: allow_opening_book

        TYPE(Opening_Book_Type) :: active_book
        TYPE(Board_Type) :: board_copy
        CHARACTER(LEN=32) :: book_move_text

        suggestion_found = .FALSE.
        suggestion_san = ''
        suggestion_source = ''
        book_move_text = ''

        IF (board%current_player == WHITE) THEN
            active_book = white_book
        ELSE
            active_book = black_book
        END IF

        IF ((.NOT. PRESENT(allow_opening_book) .OR. allow_opening_book) .AND. active_book%valid) THEN
            suggestion_found = choose_book_move(active_book, move_history, num_half_moves, board, legal_moves, &
                num_legal_moves, suggestion_move, book_move_text)
            IF (suggestion_found) THEN
                suggestion_san = move_to_san(board, suggestion_move, legal_moves, num_legal_moves)
                suggestion_source = 'opening book'
                RETURN
            END IF
        END IF

        board_copy = board
        CALL find_best_move(board_copy, search_depth, suggestion_found, suggestion_move)
        IF (suggestion_found) THEN
            suggestion_san = move_to_san(board, suggestion_move, legal_moves, num_legal_moves)
            suggestion_source = 'search'
        END IF
    END SUBROUTINE suggest_move_for_position

END MODULE Move_Suggestion
