! ============================================
! Module: Evaluation
! Purpose: Static board evaluation using material and piece-square tables
! ============================================
MODULE Evaluation
    USE Chess_Types, ONLY: PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE, &
                           BOARD_SIZE, Board_Type, Square_Type, get_piece_order, WHITE, BLACK, &
                           KNIGHT_DELTAS, BISHOP_DIRS, ROOK_DIRS, QUEEN_DIRS
    USE Board_Utils, ONLY: find_king, sq_is_valid, is_square_attacked
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: evaluate_board

    ! --- Game Phase Constants ---
    INTEGER, PARAMETER :: KNIGHT_PHASE = 1
    INTEGER, PARAMETER :: BISHOP_PHASE = 1
    INTEGER, PARAMETER :: ROOK_PHASE = 2
    INTEGER, PARAMETER :: QUEEN_PHASE = 4
    INTEGER, PARAMETER :: TOTAL_PHASE = (KNIGHT_PHASE * 2 + BISHOP_PHASE * 2 + ROOK_PHASE * 2 + QUEEN_PHASE) * 2 ! = 24
    ! Material values and PSTs borrowed from chess.cpp (mg/eg tapered eval)
    INTEGER, PARAMETER :: MATERIAL_MG(6) = (/ 82, 337, 365, 477, 1025, 20000 /)
    INTEGER, PARAMETER :: MATERIAL_EG(6) = (/ 94, 281, 297, 512, 936, 20000 /)

    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PSTM = RESHAPE( (/ &
        0, 0, 0, 0, 0, 0, 0, 0, &
        -35, -1, -20, -23, -15, 24, 38, -22, &
        -26, -4, -4, -10, 3, 3, 33, -12, &
        -27, -2, -5, 12, 17, 6, 10, -25, &
        -14, 13, 6, 21, 23, 12, 17, -23, &
        -6, 7, 26, 31, 65, 56, 25, -20, &
        98, 134, 61, 95, 68, 126, 34, -11, &
        0, 0, 0, 0, 0, 0, 0, 0 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PSTM = RESHAPE( (/ &
        -105, -21, -58, -33, -17, -28, -19, -23, &
        -29, -53, -12, -3, -1, 18, -14, -19, &
        -23, -9, 12, 10, 19, 17, 25, -16, &
        -13, 4, 16, 13, 28, 19, 21, -8, &
        -9, 17, 19, 53, 37, 69, 18, 22, &
        -47, 60, 37, 65, 84, 129, 73, 44, &
        -73, -41, 72, 36, 23, 62, 7, -17, &
        -167, -89, -34, -49, 61, -97, -15, -107 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PSTM = RESHAPE( (/ &
        -33, -3, -14, -21, -13, -12, -39, -21, &
        4, 15, 16, 0, 7, 21, 33, 1, &
        0, 15, 15, 15, 14, 27, 18, 10, &
        -6, 13, 13, 26, 34, 12, 10, 4, &
        -4, 5, 19, 50, 37, 37, 7, -2, &
        -16, 37, 43, 40, 35, 50, 37, -2, &
        -26, 16, -18, -13, 30, 59, 18, -47, &
        -29, 4, -82, -37, -25, -42, 7, -8 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PSTM = RESHAPE( (/ &
        -19, -13, 1, 17, 16, 7, -37, -26, &
        -44, -16, -20, -9, -1, 11, -6, -71, &
        -45, -25, -16, -17, 3, 0, -5, -33, &
        -36, -26, -12, -1, 9, -7, 6, -23, &
        -24, -11, 7, 26, 24, 35, -8, -20, &
        -5, 19, 26, 36, 17, 45, 61, 16, &
        27, 32, 58, 62, 80, 67, 26, 44, &
        32, 42, 32, 51, 63, 9, 31, 43 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PSTM = RESHAPE( (/ &
        -1, -18, -9, 10, -15, -25, -31, -50, &
        -35, -8, 11, 2, 8, 15, -3, 1, &
        -14, 2, -11, -2, -5, 2, 14, 5, &
        -9, -26, -9, -10, -2, -4, 3, -3, &
        -27, -27, -16, -16, -1, 17, -2, 1, &
        -13, -17, 7, 8, 29, 56, 47, 57, &
        -24, -39, -5, 1, -16, 57, 28, 54, &
        -28, 0, 29, 12, 59, 44, 43, 45 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PSTM = RESHAPE( (/ &
        -15, 36, 12, -54, 8, -28, 34, 14, &
        1, 7, -8, -64, -43, -16, 9, 8, &
        -14, -14, -22, -46, -44, -30, -15, -27, &
        -49, -1, -27, -39, -46, -44, -33, -51, &
        -17, -20, -12, -27, -30, -25, -14, -36, &
        -9, 24, 2, -16, -20, 6, 22, -22, &
        29, -1, -20, -7, -8, -4, -38, -29, &
        -65, 23, 16, -15, -56, -34, 2, 13 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PSTE = RESHAPE( (/ &
        0, 0, 0, 0, 0, 0, 0, 0, &
        13, 8, 8, 10, 13, 0, 2, -7, &
        4, 7, -6, 1, 0, -5, -1, -8, &
        13, 9, -3, -7, -7, -8, 3, -1, &
        32, 24, 13, 5, -2, 4, 17, 17, &
        94, 100, 85, 67, 56, 53, 82, 84, &
        178, 173, 158, 134, 147, 132, 165, 187, &
        0, 0, 0, 0, 0, 0, 0, 0 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PSTE = RESHAPE( (/ &
        -29, -51, -23, -15, -22, -18, -50, -64, &
        -42, -20, -10, -5, -2, -20, -23, -44, &
        -23, -3, -1, 15, 10, -3, -20, -22, &
        -18, -6, 16, 25, 16, 17, 4, -18, &
        -17, 3, 22, 22, 22, 11, 8, -18, &
        -24, -20, 10, 9, -1, -9, -19, -41, &
        -25, -8, -25, -2, -9, -25, -24, -52, &
        -58, -38, -13, -28, -31, -27, -63, -99 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PSTE = RESHAPE( (/ &
        -23, -9, -23, -5, -9, -16, -5, -17, &
        -14, -18, -7, -1, 4, -9, -15, -27, &
        -12, -3, 8, 10, 13, 3, -7, -15, &
        -6, 3, 13, 19, 7, 10, -3, -9, &
        -3, 9, 12, 9, 14, 10, 3, 2, &
        2, -8, 0, -1, -2, 6, 0, 4, &
        -8, -4, 7, -12, -3, -13, -4, -14, &
        -14, -21, -11, -8, -7, -9, -17, -24 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PSTE = RESHAPE( (/ &
        -9, 2, 3, -1, -5, -13, 4, -20, &
        -6, -6, 0, 2, -9, -9, -11, -3, &
        -4, 0, -5, -1, -7, -12, -8, -16, &
        3, 5, 8, 4, -5, -6, -8, -11, &
        4, 3, 13, 1, 2, 1, -1, 2, &
        7, 7, 7, 5, 4, -3, -5, -3, &
        11, 13, 13, 11, -3, 3, 8, 3, &
        13, 10, 18, 15, 12, 12, 8, 5 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PSTE = RESHAPE( (/ &
        -33, -28, -22, -43, -5, -32, -20, -41, &
        -22, -23, -30, -16, -16, -23, -36, -32, &
        -16, -27, 15, 6, 9, 17, 10, 5, &
        -18, 28, 19, 47, 31, 34, 39, 23, &
        3, 22, 24, 45, 57, 40, 57, 36, &
        -20, 6, 9, 49, 47, 35, 19, 9, &
        -17, 20, 32, 41, 58, 25, 30, 0, &
        -9, 22, 22, 27, 27, 19, 10, 20 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PSTE = RESHAPE( (/ &
        -53, -34, -21, -11, -28, -14, -24, -43, &
        -27, -11, 4, 13, 14, 4, -5, -17, &
        -19, -3, 11, 21, 23, 16, 7, -9, &
        -18, -4, 21, 24, 27, 23, 9, -11, &
        -8, 22, 24, 27, 26, 33, 26, 3, &
        10, 17, 23, 15, 20, 45, 44, 13, &
        -12, 17, 14, 17, 17, 38, 23, 11, &
        -74, -35, -18, -18, -11, 15, 4, -17 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER :: BISHOP_PAIR_BONUS_MG = 30
    INTEGER, PARAMETER :: BISHOP_PAIR_BONUS_EG = 45
    INTEGER, PARAMETER :: MINOR_UNDEVELOPED_PENALTY = 12
    INTEGER, PARAMETER :: CASTLED_KING_BONUS_MG = 24
    INTEGER, PARAMETER :: UNCASTLED_KING_PENALTY_MG = 16
    INTEGER, PARAMETER :: DOUBLED_PAWN_PENALTY_MG = 12
    INTEGER, PARAMETER :: DOUBLED_PAWN_PENALTY_EG = 18
    INTEGER, PARAMETER :: ISOLATED_PAWN_PENALTY_MG = 14
    INTEGER, PARAMETER :: ISOLATED_PAWN_PENALTY_EG = 10
    INTEGER, PARAMETER :: CENTER_PAWN_BONUS_MG = 14
    INTEGER, PARAMETER :: FLANK_PAWN_ADVANCE_PENALTY_MG = 12
    INTEGER, PARAMETER :: KING_SHIELD_PAWN_MG = 10
    INTEGER, PARAMETER, DIMENSION(8) :: PASSED_PAWN_BONUS_MG = (/ 0, 0, 8, 18, 32, 52, 78, 0 /)
    INTEGER, PARAMETER, DIMENSION(8) :: PASSED_PAWN_BONUS_EG = (/ 0, 0, 12, 28, 52, 84, 128, 0 /)
    INTEGER, PARAMETER :: CENTRAL_PAWN_BLOCKER_PENALTY = 22
    INTEGER, PARAMETER, DIMENSION(6) :: MOBILITY_WEIGHT_MG = (/ 0, 4, 4, 2, 1, 0 /)
    INTEGER, PARAMETER, DIMENSION(6) :: MOBILITY_WEIGHT_EG = (/ 0, 3, 3, 2, 1, 0 /)
    INTEGER, PARAMETER, DIMENSION(6) :: LOOSE_PIECE_PENALTY_MG = (/ 0, 24, 24, 32, 48, 0 /)
    INTEGER, PARAMETER, DIMENSION(6) :: LOOSE_PIECE_PENALTY_EG = (/ 0, 18, 18, 24, 36, 0 /)

CONTAINS

    ! --- Get Piece Phase Value ---
    ! Returns the phase contribution for a piece type (used for tapered eval).
    PURE INTEGER FUNCTION get_piece_phase(piece)
        INTEGER, INTENT(IN) :: piece
        SELECT CASE(piece)
        CASE(KNIGHT); get_piece_phase = KNIGHT_PHASE
        CASE(BISHOP); get_piece_phase = BISHOP_PHASE
        CASE(ROOK);   get_piece_phase = ROOK_PHASE
        CASE(QUEEN);  get_piece_phase = QUEEN_PHASE
        CASE DEFAULT; get_piece_phase = 0
        END SELECT
    END FUNCTION get_piece_phase

    ! --- Calculate Tapered PST Value ---
    ! Blends middlegame and endgame PST values based on the game phase.
    !
    ! Parameters:
    !   mg_val (IN): Middlegame PST value
    !   eg_val (IN): Endgame PST value
    !   phase (IN): Current game phase (0 = endgame, TOTAL_PHASE = middlegame)
    !   total_phase (IN): Maximum possible game phase value
    !
    ! Returns:
    !   Blended PST value
    PURE INTEGER FUNCTION tapered_pst_value(mg_val, eg_val, phase, total_phase) RESULT(blended_value)
        INTEGER, INTENT(IN) :: mg_val, eg_val, phase, total_phase
        blended_value = ((mg_val * phase) + (eg_val * (total_phase - phase))) / total_phase
    END FUNCTION tapered_pst_value

    ! --- Evaluate Board ---
    ! Performs static evaluation of the current board position.
    !
    ! Evaluation combines material balance with positional factors:
    ! - Material: Piece values (pawn=100, knight=320, etc.)
    ! - Position: Piece-square table bonuses/penalties
    !
    ! The evaluation is calculated as: white_score - black_score
    ! Positive scores favor white, negative favor black.
    !
    ! Parameters:
    !   board (IN): Current board state to evaluate
    !
    ! Returns:
    !   Evaluation score in centipawns (positive = white advantage)
    !
    ! Notes:
    !   - Uses piece lists for efficient iteration
    !   - PSTs are flipped for black pieces (rank 8 becomes rank 1)
    !   - King value is high to ensure mate detection
    INTEGER FUNCTION evaluate_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: i, r, f, piece, eval_rank
        INTEGER :: phase
        TYPE(Square_Type) :: sq
        INTEGER :: mg_score, eg_score
        INTEGER :: mg_pst, eg_pst
        INTEGER, DIMENSION(BOARD_SIZE) :: white_pawn_files, black_pawn_files
        INTEGER :: white_bishops, black_bishops
        TYPE(Square_Type) :: white_king_sq, black_king_sq

        ! --- Calculate Game Phase ---
        phase = 0
        white_pawn_files = 0
        black_pawn_files = 0
        white_bishops = 0
        black_bishops = 0
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            phase = phase + get_piece_phase(piece)
            IF (piece == PAWN) white_pawn_files(sq%file) = white_pawn_files(sq%file) + 1
            IF (piece == BISHOP) white_bishops = white_bishops + 1
        END DO
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            phase = phase + get_piece_phase(piece)
            IF (piece == PAWN) black_pawn_files(sq%file) = black_pawn_files(sq%file) + 1
            IF (piece == BISHOP) black_bishops = black_bishops + 1
        END DO
        phase = MIN(phase, TOTAL_PHASE)

        ! --- Evaluate Pieces (mg/eg components) ---
        mg_score = 0
        eg_score = 0

        ! Evaluate white pieces
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = r  ! White: rank 1-8 as-is

            CALL get_piece_pst(piece, eval_rank, f, mg_pst, eg_pst)
            mg_score = mg_score + MATERIAL_MG(piece) + mg_pst
            eg_score = eg_score + MATERIAL_EG(piece) + eg_pst
        END DO

        ! Evaluate black pieces
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = BOARD_SIZE - r + 1  ! Black: flip ranks (8->1, 1->8)

            CALL get_piece_pst(piece, eval_rank, f, mg_pst, eg_pst)
            mg_score = mg_score - (MATERIAL_MG(piece) + mg_pst)
            eg_score = eg_score - (MATERIAL_EG(piece) + eg_pst)
        END DO

        white_king_sq = find_king(board, WHITE)
        black_king_sq = find_king(board, BLACK)

        CALL add_pawn_structure_terms(board, WHITE, white_pawn_files, white_king_sq, mg_score, eg_score)
        CALL add_pawn_structure_terms(board, BLACK, black_pawn_files, black_king_sq, mg_score, eg_score)
        CALL add_development_terms(board, white_king_sq, black_king_sq, white_bishops, black_bishops, mg_score, eg_score)
        CALL add_king_safety_terms(board, white_king_sq, black_king_sq, mg_score)
        CALL add_central_pawn_blocker_terms(board, mg_score)
        CALL add_piece_activity_terms(board, mg_score, eg_score)

        evaluate_board = tapered_pst_value(mg_score, eg_score, phase, TOTAL_PHASE)
        IF (board%current_player == BLACK) evaluate_board = -evaluate_board

    END FUNCTION evaluate_board

    SUBROUTINE get_piece_pst(piece, eval_rank, file, mg_pst, eg_pst)
        INTEGER, INTENT(IN) :: piece, eval_rank, file
        INTEGER, INTENT(OUT) :: mg_pst, eg_pst

        SELECT CASE(piece)
        CASE(PAWN)
            mg_pst = PAWN_PSTM(eval_rank, file)
            eg_pst = PAWN_PSTE(eval_rank, file)
        CASE(KNIGHT)
            mg_pst = KNIGHT_PSTM(eval_rank, file)
            eg_pst = KNIGHT_PSTE(eval_rank, file)
        CASE(BISHOP)
            mg_pst = BISHOP_PSTM(eval_rank, file)
            eg_pst = BISHOP_PSTE(eval_rank, file)
        CASE(ROOK)
            mg_pst = ROOK_PSTM(eval_rank, file)
            eg_pst = ROOK_PSTE(eval_rank, file)
        CASE(QUEEN)
            mg_pst = QUEEN_PSTM(eval_rank, file)
            eg_pst = QUEEN_PSTE(eval_rank, file)
        CASE(KING)
            mg_pst = KING_PSTM(eval_rank, file)
            eg_pst = KING_PSTE(eval_rank, file)
        CASE DEFAULT
            mg_pst = 0
            eg_pst = 0
        END SELECT
    END SUBROUTINE get_piece_pst

    SUBROUTINE add_pawn_structure_terms(board, color, own_pawn_files, king_sq, mg_score, eg_score)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: color
        INTEGER, DIMENSION(BOARD_SIZE), INTENT(IN) :: own_pawn_files
        TYPE(Square_Type), INTENT(IN) :: king_sq
        INTEGER, INTENT(INOUT) :: mg_score, eg_score

        INTEGER :: i, file_idx, rank_idx, eval_rank, mg_delta, eg_delta
        TYPE(Square_Type) :: sq

        IF (color == WHITE) THEN
            DO i = 1, board%num_white_pieces
                sq = board%white_pieces(i)
                IF (board%squares_piece(sq%rank, sq%file) /= PAWN) CYCLE
                file_idx = sq%file
                rank_idx = sq%rank
                eval_rank = rank_idx
                CALL pawn_feature_score(board, color, file_idx, rank_idx, eval_rank, own_pawn_files, king_sq, mg_delta, eg_delta)
                mg_score = mg_score + mg_delta
                eg_score = eg_score + eg_delta
            END DO
        ELSE
            DO i = 1, board%num_black_pieces
                sq = board%black_pieces(i)
                IF (board%squares_piece(sq%rank, sq%file) /= PAWN) CYCLE
                file_idx = sq%file
                rank_idx = sq%rank
                eval_rank = BOARD_SIZE - rank_idx + 1
                CALL pawn_feature_score(board, color, file_idx, rank_idx, eval_rank, own_pawn_files, king_sq, mg_delta, eg_delta)
                mg_score = mg_score - mg_delta
                eg_score = eg_score - eg_delta
            END DO
        END IF
    END SUBROUTINE add_pawn_structure_terms

    SUBROUTINE pawn_feature_score(board, color, file_idx, rank_idx, eval_rank, own_pawn_files, king_sq, mg_delta, eg_delta)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: color, file_idx, rank_idx, eval_rank
        INTEGER, DIMENSION(BOARD_SIZE), INTENT(IN) :: own_pawn_files
        TYPE(Square_Type), INTENT(IN) :: king_sq
        INTEGER, INTENT(OUT) :: mg_delta, eg_delta
        LOGICAL :: isolated

        mg_delta = 0
        eg_delta = 0

        IF (own_pawn_files(file_idx) > 1) THEN
            mg_delta = mg_delta - DOUBLED_PAWN_PENALTY_MG
            eg_delta = eg_delta - DOUBLED_PAWN_PENALTY_EG
        END IF

        isolated = .TRUE.
        IF (file_idx > 1) isolated = isolated .AND. own_pawn_files(file_idx - 1) == 0
        IF (file_idx < BOARD_SIZE) isolated = isolated .AND. own_pawn_files(file_idx + 1) == 0
        IF (isolated) THEN
            mg_delta = mg_delta - ISOLATED_PAWN_PENALTY_MG
            eg_delta = eg_delta - ISOLATED_PAWN_PENALTY_EG
        END IF

        IF (is_passed_pawn(board, color, rank_idx, file_idx)) THEN
            mg_delta = mg_delta + PASSED_PAWN_BONUS_MG(eval_rank)
            eg_delta = eg_delta + PASSED_PAWN_BONUS_EG(eval_rank)
        END IF

        IF ((file_idx == 4 .OR. file_idx == 5) .AND. eval_rank >= 4) THEN
            mg_delta = mg_delta + CENTER_PAWN_BONUS_MG + 4 * (eval_rank - 4)
        END IF

        IF ((file_idx <= 2 .OR. file_idx >= 7) .AND. eval_rank >= 4 .AND. .NOT. king_is_castled(king_sq, color)) THEN
            mg_delta = mg_delta - FLANK_PAWN_ADVANCE_PENALTY_MG * (eval_rank - 2)
        END IF
    END SUBROUTINE pawn_feature_score

    SUBROUTINE add_development_terms(board, white_king_sq, black_king_sq, white_bishops, black_bishops, mg_score, eg_score)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: white_king_sq, black_king_sq
        INTEGER, INTENT(IN) :: white_bishops, black_bishops
        INTEGER, INTENT(INOUT) :: mg_score, eg_score

        IF (board%squares_piece(1, 2) == KNIGHT .AND. board%squares_color(1, 2) == WHITE) mg_score = mg_score - MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(1, 7) == KNIGHT .AND. board%squares_color(1, 7) == WHITE) mg_score = mg_score - MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(1, 3) == BISHOP .AND. board%squares_color(1, 3) == WHITE) mg_score = mg_score - MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(1, 6) == BISHOP .AND. board%squares_color(1, 6) == WHITE) mg_score = mg_score - MINOR_UNDEVELOPED_PENALTY

        IF (board%squares_piece(8, 2) == KNIGHT .AND. board%squares_color(8, 2) == BLACK) mg_score = mg_score + MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(8, 7) == KNIGHT .AND. board%squares_color(8, 7) == BLACK) mg_score = mg_score + MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(8, 3) == BISHOP .AND. board%squares_color(8, 3) == BLACK) mg_score = mg_score + MINOR_UNDEVELOPED_PENALTY
        IF (board%squares_piece(8, 6) == BISHOP .AND. board%squares_color(8, 6) == BLACK) mg_score = mg_score + MINOR_UNDEVELOPED_PENALTY

        IF (white_bishops >= 2) THEN
            mg_score = mg_score + BISHOP_PAIR_BONUS_MG
            eg_score = eg_score + BISHOP_PAIR_BONUS_EG
        END IF
        IF (black_bishops >= 2) THEN
            mg_score = mg_score - BISHOP_PAIR_BONUS_MG
            eg_score = eg_score - BISHOP_PAIR_BONUS_EG
        END IF

        IF (king_is_castled(white_king_sq, WHITE)) THEN
            mg_score = mg_score + CASTLED_KING_BONUS_MG
        ELSE IF (white_king_sq%rank == 1 .AND. white_king_sq%file == 5) THEN
            mg_score = mg_score - UNCASTLED_KING_PENALTY_MG
        END IF

        IF (king_is_castled(black_king_sq, BLACK)) THEN
            mg_score = mg_score - CASTLED_KING_BONUS_MG
        ELSE IF (black_king_sq%rank == 8 .AND. black_king_sq%file == 5) THEN
            mg_score = mg_score + UNCASTLED_KING_PENALTY_MG
        END IF
    END SUBROUTINE add_development_terms

    SUBROUTINE add_king_safety_terms(board, white_king_sq, black_king_sq, mg_score)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: white_king_sq, black_king_sq
        INTEGER, INTENT(INOUT) :: mg_score

        mg_score = mg_score + pawn_shield_score(board, WHITE, white_king_sq)
        mg_score = mg_score - pawn_shield_score(board, BLACK, black_king_sq)
    END SUBROUTINE add_king_safety_terms

    PURE LOGICAL FUNCTION king_is_castled(king_sq, color)
        TYPE(Square_Type), INTENT(IN) :: king_sq
        INTEGER, INTENT(IN) :: color

        IF (color == WHITE) THEN
            king_is_castled = (king_sq%rank == 1 .AND. (king_sq%file == 3 .OR. king_sq%file == 7))
        ELSE
            king_is_castled = (king_sq%rank == 8 .AND. (king_sq%file == 3 .OR. king_sq%file == 7))
        END IF
    END FUNCTION king_is_castled

    INTEGER FUNCTION pawn_shield_score(board, color, king_sq) RESULT(score)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: color
        TYPE(Square_Type), INTENT(IN) :: king_sq

        INTEGER :: df, rank1, rank2, file1, dir

        score = 0
        IF (king_sq%rank == 0) RETURN

        IF (color == WHITE) THEN
            dir = 1
        ELSE
            dir = -1
        END IF

        rank1 = king_sq%rank + dir
        rank2 = king_sq%rank + 2 * dir
        DO df = -1, 1
            file1 = king_sq%file + df
            IF (.NOT. sq_is_valid(MAX(1, MIN(BOARD_SIZE, rank1)), file1)) CYCLE

            IF (sq_is_valid(rank1, file1) .AND. board%squares_piece(rank1, file1) == PAWN .AND. &
                board%squares_color(rank1, file1) == color) THEN
                score = score + KING_SHIELD_PAWN_MG
            ELSE IF (sq_is_valid(rank2, file1) .AND. board%squares_piece(rank2, file1) == PAWN .AND. &
                     board%squares_color(rank2, file1) == color) THEN
                score = score + KING_SHIELD_PAWN_MG / 2
            ELSE
                score = score - KING_SHIELD_PAWN_MG / 2
            END IF
        END DO
    END FUNCTION pawn_shield_score

    LOGICAL FUNCTION is_passed_pawn(board, color, rank_idx, file_idx) RESULT(passed)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: color, rank_idx, file_idx

        INTEGER :: r, f, start_rank, end_rank, step, enemy_color

        passed = .TRUE.
        IF (color == WHITE) THEN
            enemy_color = BLACK
            start_rank = rank_idx + 1
            end_rank = BOARD_SIZE
            step = 1
        ELSE
            enemy_color = WHITE
            start_rank = rank_idx - 1
            end_rank = 1
            step = -1
        END IF

        DO r = start_rank, end_rank, step
            DO f = MAX(1, file_idx - 1), MIN(BOARD_SIZE, file_idx + 1)
                IF (board%squares_piece(r, f) == PAWN .AND. board%squares_color(r, f) == enemy_color) THEN
                    passed = .FALSE.
                    RETURN
                END IF
            END DO
        END DO
    END FUNCTION is_passed_pawn

    SUBROUTINE add_central_pawn_blocker_terms(board, mg_score)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(INOUT) :: mg_score

        IF (board%squares_piece(2, 4) == PAWN .AND. board%squares_color(2, 4) == WHITE) THEN
            IF (board%squares_color(3, 4) == WHITE .AND. board%squares_piece(3, 4) /= NO_PIECE .AND. &
                board%squares_piece(3, 4) /= PAWN) THEN
                mg_score = mg_score - CENTRAL_PAWN_BLOCKER_PENALTY
            END IF
        END IF
        IF (board%squares_piece(2, 5) == PAWN .AND. board%squares_color(2, 5) == WHITE) THEN
            IF (board%squares_color(3, 5) == WHITE .AND. board%squares_piece(3, 5) /= NO_PIECE .AND. &
                board%squares_piece(3, 5) /= PAWN) THEN
                mg_score = mg_score - CENTRAL_PAWN_BLOCKER_PENALTY
            END IF
        END IF

        IF (board%squares_piece(7, 4) == PAWN .AND. board%squares_color(7, 4) == BLACK) THEN
            IF (board%squares_color(6, 4) == BLACK .AND. board%squares_piece(6, 4) /= NO_PIECE .AND. &
                board%squares_piece(6, 4) /= PAWN) THEN
                mg_score = mg_score + CENTRAL_PAWN_BLOCKER_PENALTY
            END IF
        END IF
        IF (board%squares_piece(7, 5) == PAWN .AND. board%squares_color(7, 5) == BLACK) THEN
            IF (board%squares_color(6, 5) == BLACK .AND. board%squares_piece(6, 5) /= NO_PIECE .AND. &
                board%squares_piece(6, 5) /= PAWN) THEN
                mg_score = mg_score + CENTRAL_PAWN_BLOCKER_PENALTY
            END IF
        END IF
    END SUBROUTINE add_central_pawn_blocker_terms

    SUBROUTINE add_piece_activity_terms(board, mg_score, eg_score)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(INOUT) :: mg_score, eg_score

        INTEGER :: i, piece, mobility, mg_delta, eg_delta
        TYPE(Square_Type) :: sq

        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            CALL piece_activity_score(board, sq, piece, WHITE, mobility, mg_delta, eg_delta)
            mg_score = mg_score + mg_delta
            eg_score = eg_score + eg_delta
        END DO

        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            CALL piece_activity_score(board, sq, piece, BLACK, mobility, mg_delta, eg_delta)
            mg_score = mg_score - mg_delta
            eg_score = eg_score - eg_delta
        END DO
    END SUBROUTINE add_piece_activity_terms

    SUBROUTINE piece_activity_score(board, sq, piece, color, mobility, mg_delta, eg_delta)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, INTENT(IN) :: piece, color
        INTEGER, INTENT(OUT) :: mobility, mg_delta, eg_delta

        LOGICAL :: attacked, defended

        mobility = count_piece_mobility(board, sq, piece, color)
        mg_delta = 0
        eg_delta = 0

        IF (piece >= KNIGHT .AND. piece <= QUEEN) THEN
            mg_delta = mg_delta + MOBILITY_WEIGHT_MG(piece) * mobility
            eg_delta = eg_delta + MOBILITY_WEIGHT_EG(piece) * mobility

            attacked = is_square_attacked(board, sq, get_opponent_color_eval(color))
            defended = is_square_attacked(board, sq, color)
            IF (attacked) THEN
                mg_delta = mg_delta - LOOSE_PIECE_PENALTY_MG(piece)
                eg_delta = eg_delta - LOOSE_PIECE_PENALTY_EG(piece)
                IF (.NOT. defended) THEN
                    mg_delta = mg_delta - LOOSE_PIECE_PENALTY_MG(piece)
                    eg_delta = eg_delta - LOOSE_PIECE_PENALTY_EG(piece)
                END IF
            END IF
        END IF
    END SUBROUTINE piece_activity_score

    INTEGER FUNCTION count_piece_mobility(board, sq, piece, color) RESULT(mobility)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, INTENT(IN) :: piece, color

        INTEGER :: i, nr, nf

        mobility = 0
        SELECT CASE(piece)
        CASE(KNIGHT)
            DO i = 1, 8
                nr = sq%rank + KNIGHT_DELTAS(i, 1)
                nf = sq%file + KNIGHT_DELTAS(i, 2)
                IF (sq_is_valid(nr, nf) .AND. board%squares_color(nr, nf) /= color) mobility = mobility + 1
            END DO
        CASE(BISHOP)
            mobility = count_sliding_mobility(board, sq, color, BISHOP_DIRS, 4)
        CASE(ROOK)
            mobility = count_sliding_mobility(board, sq, color, ROOK_DIRS, 4)
        CASE(QUEEN)
            mobility = count_sliding_mobility(board, sq, color, QUEEN_DIRS, 8)
        CASE DEFAULT
            mobility = 0
        END SELECT
    END FUNCTION count_piece_mobility

    INTEGER FUNCTION count_sliding_mobility(board, sq, color, directions, num_dirs) RESULT(mobility)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, DIMENSION(num_dirs, 2), INTENT(IN) :: directions
        INTEGER, INTENT(IN) :: color, num_dirs

        INTEGER :: i, nr, nf, dr, df

        mobility = 0
        DO i = 1, num_dirs
            dr = directions(i, 1)
            df = directions(i, 2)
            nr = sq%rank + dr
            nf = sq%file + df
            DO WHILE (sq_is_valid(nr, nf))
                IF (board%squares_color(nr, nf) == color) EXIT
                mobility = mobility + 1
                IF (board%squares_piece(nr, nf) /= NO_PIECE) EXIT
                nr = nr + dr
                nf = nf + df
            END DO
        END DO
    END FUNCTION count_sliding_mobility

    PURE INTEGER FUNCTION get_opponent_color_eval(color)
        INTEGER, INTENT(IN) :: color

        IF (color == WHITE) THEN
            get_opponent_color_eval = BLACK
        ELSE
            get_opponent_color_eval = WHITE
        END IF
    END FUNCTION get_opponent_color_eval

END MODULE Evaluation
