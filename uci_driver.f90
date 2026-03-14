MODULE UCI_Driver
    USE Chess_Types
    USE Board_Utils, ONLY: char_to_file, char_to_rank, init_board, set_board_from_fen
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move
    USE Search, ONLY: find_best_move
    USE Transposition_Table, ONLY: init_zobrist_keys
    USE Evaluation, ONLY: evaluate_board
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_uci_mode

CONTAINS

    SUBROUTINE run_uci_mode()
        TYPE(Board_Type) :: board
        CHARACTER(LEN=256) :: line
        INTEGER :: ios

        CALL init_zobrist_keys()
        CALL init_board(board)

        DO
            READ(*,'(A)',IOSTAT=ios) line
            IF (ios /= 0) EXIT
            CALL handle_command(board, TRIM(ADJUSTL(line)))
        END DO
    END SUBROUTINE run_uci_mode

    SUBROUTINE handle_command(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line

        IF (LEN_TRIM(line) == 0) RETURN

        IF (starts_with(line, 'uci')) THEN
            CALL uci_init()
        ELSE IF (starts_with(line, 'isready')) THEN
            WRITE(*,'(A)') 'readyok'
        ELSE IF (starts_with(line, 'ucinewgame')) THEN
            CALL init_board(board)
        ELSE IF (starts_with(line, 'position')) THEN
            CALL handle_position(board, line)
        ELSE IF (starts_with(line, 'go')) THEN
            CALL handle_go(board, line)
        ELSE IF (starts_with(line, 'stop')) THEN
            CONTINUE
        ELSE IF (starts_with(line, 'quit')) THEN
            STOP
        END IF
    END SUBROUTINE handle_command

    LOGICAL FUNCTION starts_with(text, prefix) RESULT(res)
        CHARACTER(LEN=*), INTENT(IN) :: text, prefix
        INTEGER :: lp
        lp = LEN_TRIM(prefix)
        IF (LEN_TRIM(text) < lp) THEN
            res = .FALSE.
        ELSE
            res = (text(1:lp) == prefix(1:lp))
        END IF
    END FUNCTION starts_with

    SUBROUTINE uci_init()
        WRITE(*,'(A)') 'id name FortranChess'
        WRITE(*,'(A)') 'id author Dean Menezes'
        WRITE(*,'(A)') 'uciok'
    END SUBROUTINE uci_init

    SUBROUTINE handle_position(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: pos_moves, lenline

        lenline = LEN_TRIM(line)
        pos_moves = INDEX(line, 'moves')

        IF (INDEX(line, 'startpos') > 0) THEN
            CALL init_board(board)
        ELSE IF (INDEX(line, 'fen') > 0) THEN
            CALL handle_fen(board, line)
        END IF

        IF (pos_moves > 0) THEN
            IF (pos_moves + 5 <= lenline) THEN
                CALL parse_moves(board, line(pos_moves+5:lenline))
            END IF
        END IF
    END SUBROUTINE handle_position

    SUBROUTINE parse_moves(board, moves_str)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: moves_str
        INTEGER :: start, finish, l
        CHARACTER(LEN=8) :: mv
        LOGICAL :: ok

        l = LEN_TRIM(moves_str)
        start = 1
        DO WHILE (start <= l)
            DO WHILE (start <= l .AND. moves_str(start:start) == ' ')
                start = start + 1
            END DO
            IF (start > l) EXIT
            finish = start
            DO WHILE (finish <= l)
                IF (moves_str(finish:finish) == ' ') EXIT
                finish = finish + 1
            END DO
            mv = moves_str(start:MIN(finish-1, LEN(moves_str)))
            ok = apply_uci_move(board, TRIM(mv))
            IF (.NOT. ok) EXIT
            start = finish + 1
        END DO
    END SUBROUTINE parse_moves

    SUBROUTINE handle_fen(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: fen_pos, moves_pos, fen_end
        CHARACTER(LEN=256) :: fen
        LOGICAL :: ok

        fen_pos = INDEX(line, 'fen')
        IF (fen_pos == 0) RETURN
        moves_pos = INDEX(line, 'moves')
        IF (moves_pos > 0) THEN
            fen_end = moves_pos - 1
        ELSE
            fen_end = LEN_TRIM(line)
        END IF
        fen = TRIM(ADJUSTL(line(fen_pos+3:fen_end)))
        ok = set_board_from_fen(board, fen)
    END SUBROUTINE handle_fen

    LOGICAL FUNCTION apply_uci_move(board, mv_str) RESULT(ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: mv_str
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        INTEGER :: num_legal, promo_piece
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: unmake_info

        ok = .FALSE.
        IF (LEN_TRIM(mv_str) < 4) RETURN

        CALL generate_moves(board, legal_moves, num_legal)

        promo_piece = NO_PIECE
        IF (LEN_TRIM(mv_str) >= 5) THEN
            SELECT CASE(mv_str(5:5))
            CASE('q','Q'); promo_piece = QUEEN
            CASE('r','R'); promo_piece = ROOK
            CASE('b','B'); promo_piece = BISHOP
            CASE('n','N'); promo_piece = KNIGHT
            END SELECT
        END IF

        IF (find_move_in_list(mv_str, promo_piece, legal_moves, num_legal, chosen_move)) THEN
            CALL make_move(board, chosen_move, unmake_info)
            ok = .TRUE.
        END IF
    END FUNCTION apply_uci_move

    LOGICAL FUNCTION find_move_in_list(mv_str, promo_piece, move_list, num_moves, chosen) RESULT(found)
        CHARACTER(LEN=*), INTENT(IN) :: mv_str
        INTEGER, INTENT(IN) :: promo_piece, num_moves
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: move_list
        TYPE(Move_Type), INTENT(OUT) :: chosen
        INTEGER :: ff, fr, tf, tr, i

        found = .FALSE.
        ff = char_to_file(mv_str(1:1)); fr = char_to_rank(mv_str(2:2))
        tf = char_to_file(mv_str(3:3)); tr = char_to_rank(mv_str(4:4))
        IF (ff == -1 .OR. fr == -1 .OR. tf == -1 .OR. tr == -1) RETURN

        DO i = 1, num_moves
            IF (move_list(i)%from_sq%file == ff .AND. move_list(i)%from_sq%rank == fr .AND. &
                move_list(i)%to_sq%file == tf   .AND. move_list(i)%to_sq%rank == tr .AND. &
                move_list(i)%promotion_piece == promo_piece) THEN
                chosen = move_list(i)
                found = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION find_move_in_list

    SUBROUTINE handle_go(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: depth, movetime, wtime, btime, winc, binc
        INTEGER :: chosen_depth, time_budget
        TYPE(Move_Type) :: best_move
        LOGICAL :: best_found
        CHARACTER(LEN=8) :: best_str
        INTEGER :: start_clock, end_clock, rate

        depth = -1; movetime = -1; wtime = -1; btime = -1; winc = 0; binc = 0
        CALL parse_go_tokens(line, depth, movetime, wtime, btime, winc, binc)

        chosen_depth = depth
        IF (chosen_depth <= 0) THEN
            time_budget = derive_time_ms(board%current_player, movetime, wtime, btime, winc, binc)
            chosen_depth = depth_from_time(time_budget)
        END IF

        CALL SYSTEM_CLOCK(start_clock, rate)
        CALL find_best_move(board, chosen_depth, best_found, best_move)
        CALL SYSTEM_CLOCK(end_clock)
        CALL print_uci_info(chosen_depth, start_clock, end_clock, rate, best_move, best_found)
        IF (best_found) THEN
            best_str = move_to_uci(best_move)
        ELSE
            best_str = '0000'
        END IF
        WRITE(*,'(A)') 'bestmove ' // TRIM(best_str)
    END SUBROUTINE handle_go

    SUBROUTINE print_uci_info(depth, start_clk, end_clk, rate, best_move, found)
        INTEGER, INTENT(IN) :: depth, start_clk, end_clk, rate
        TYPE(Move_Type), INTENT(IN) :: best_move
        LOGICAL, INTENT(IN) :: found
        REAL :: ms
        CHARACTER(LEN=8) :: mvstr
        CHARACTER(LEN=128) :: line

        ms = 1000.0 * REAL(end_clk - start_clk) / REAL(rate)
        IF (found) THEN
            mvstr = move_to_uci(best_move)
        ELSE
            mvstr = '0000'
        END IF
        WRITE(line,'(A,I0,A,F7.2,A,A)') 'info depth ', depth, ' time ', ms, ' pv ', TRIM(mvstr)
        WRITE(*,'(A)') TRIM(line)
    END SUBROUTINE print_uci_info

    SUBROUTINE parse_go_tokens(line, depth, movetime, wtime, btime, winc, binc)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER, INTENT(OUT) :: depth, movetime, wtime, btime, winc, binc
        INTEGER :: val, ios, pos
        CHARACTER(LEN=32) :: tok, tokval
        LOGICAL :: has

        depth = -1; movetime = -1; wtime = -1; btime = -1; winc = 0; binc = 0
        pos = 1
        DO
            CALL next_token(line, pos, tok, has)
            IF (.NOT. has) EXIT
            SELECT CASE (tok)
            CASE('depth')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) depth = val
                END IF
            CASE('movetime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) movetime = val
                END IF
            CASE('wtime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) wtime = val
                END IF
            CASE('btime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) btime = val
                END IF
            CASE('winc')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) winc = val
                END IF
            CASE('binc')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) binc = val
                END IF
            END SELECT
        END DO
    END SUBROUTINE parse_go_tokens

    SUBROUTINE next_token(line, pos, token, has)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER, INTENT(INOUT) :: pos
        CHARACTER(LEN=*), INTENT(OUT) :: token
        LOGICAL, INTENT(OUT) :: has
        INTEGER :: l, start, finish

        l = LEN(line)
        DO WHILE (pos <= l)
            IF (line(pos:pos) /= ' ') EXIT
            pos = pos + 1
        END DO
        IF (pos > l) THEN
            has = .FALSE.
            token = ''
            RETURN
        END IF
        start = pos
        finish = start
        DO WHILE (finish <= l)
            IF (line(finish:finish) == ' ') EXIT
            finish = finish + 1
        END DO
        token = line(start:finish-1)
        pos = finish + 1
        has = .TRUE.
    END SUBROUTINE next_token

    INTEGER FUNCTION derive_time_ms(side_to_move, movetime, wtime, btime, winc, binc) RESULT(ms)
        INTEGER, INTENT(IN) :: side_to_move, movetime, wtime, btime, winc, binc
        INTEGER :: pool, inc

        IF (movetime > 0) THEN
            ms = movetime
            RETURN
        END IF

        IF (side_to_move == WHITE) THEN
            pool = wtime
            inc = winc
        ELSE
            pool = btime
            inc = binc
        END IF

        IF (pool <= 0) THEN
            ms = 2000 ! fallback
        ELSE
            ms = pool / 30 + inc * 4 / 5
            IF (ms < 200) ms = 200
        END IF
    END FUNCTION derive_time_ms

    INTEGER FUNCTION depth_from_time(ms) RESULT(depth)
        INTEGER, INTENT(IN) :: ms
        IF (ms <= 300) THEN
            depth = 2
        ELSE IF (ms <= 700) THEN
            depth = 3
        ELSE IF (ms <= 1500) THEN
            depth = 4
        ELSE IF (ms <= 3500) THEN
            depth = 5
        ELSE IF (ms <= 7000) THEN
            depth = 6
        ELSE
            depth = 7
        END IF
    END FUNCTION depth_from_time

    CHARACTER(LEN=8) FUNCTION move_to_uci(mv) RESULT(str)
        TYPE(Move_Type), INTENT(IN) :: mv
        CHARACTER(LEN=1) :: promo_char
        str = ''
        str(1:1) = CHAR(ICHAR('a') + mv%from_sq%file - 1)
        str(2:2) = CHAR(ICHAR('0') + mv%from_sq%rank)
        str(3:3) = CHAR(ICHAR('a') + mv%to_sq%file - 1)
        str(4:4) = CHAR(ICHAR('0') + mv%to_sq%rank)
        IF (mv%promotion_piece /= NO_PIECE) THEN
            SELECT CASE(mv%promotion_piece)
            CASE(QUEEN); promo_char = 'q'
            CASE(ROOK);  promo_char = 'r'
            CASE(BISHOP); promo_char = 'b'
            CASE(KNIGHT); promo_char = 'n'
            CASE DEFAULT; promo_char = 'q'
            END SELECT
            str(5:5) = promo_char
        END IF
    END FUNCTION move_to_uci

END MODULE UCI_Driver
