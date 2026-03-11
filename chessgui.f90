MODULE Chess_GUI_App
    USE, INTRINSIC :: iso_c_binding
    USE App_Defaults, ONLY: DEFAULT_SEARCH_DEPTH
    USE Chess_Types
    USE Board_Utils, ONLY: init_board, get_opponent_color
    USE Evaluation, ONLY: evaluate_board
    USE Game_State_Checker, ONLY: is_game_over
    USE Game_Time_Utils, ONLY: parse_time_control, elapsed_milliseconds, apply_clock_after_move, &
        get_ai_time_budget_seconds, format_clock
    USE Make_Unmake, ONLY: make_move, unmake_move
    USE Move_Generation, ONLY: generate_moves
    USE Notation_Utils, ONLY: move_matches_input, move_to_coordinate, move_to_san, to_lower_string, write_pgn_file
    USE Move_Suggestion, ONLY: suggest_move_for_position
    USE Opening_Book, ONLY: Opening_Book_Type, load_opening_book, choose_book_move
    USE GUI_Debug_Log, ONLY: start_gui_debug_session, finish_gui_debug_session, log_gui_event, log_gui_message, &
        log_gui_checkpoint, log_gui_value, log_gui_failure
    USE Search_Debug_Log, ONLY: start_ai_turn_debug_log, log_opening_book_hit, log_opening_book_miss, &
        finish_ai_turn_debug_log
    USE Search, ONLY: find_best_move
    USE Transposition_Table, ONLY: init_zobrist_keys
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_chess_gui

    INTEGER(c_int), PARAMETER :: WM_CREATE = 1_c_int
    INTEGER(c_int), PARAMETER :: WM_DESTROY = 2_c_int
    INTEGER(c_int), PARAMETER :: WM_PAINT = 15_c_int
    INTEGER(c_int), PARAMETER :: WM_COMMAND = 273_c_int
    INTEGER(c_int), PARAMETER :: WM_TIMER = 275_c_int
    INTEGER(c_int), PARAMETER :: WM_KEYDOWN = 256_c_int
    INTEGER(c_int), PARAMETER :: SW_SHOWNORMAL = 1_c_int
    INTEGER(c_int), PARAMETER :: CW_USEDEFAULT = -2147483647_c_int - 1_c_int
    INTEGER(c_int), PARAMETER :: TRANSPARENT = 1_c_int
    INTEGER(c_intptr_t), PARAMETER :: TIMER_CLOCK = 1_c_intptr_t
    INTEGER(c_int), PARAMETER :: GDI_PLUS_OK = 0_c_int
    INTEGER(c_long), PARAMETER :: WS_OVERLAPPEDWINDOW = INT(Z'00CF0000', c_long)
    INTEGER(c_long), PARAMETER :: WS_CHILD = INT(Z'40000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_VISIBLE = INT(Z'10000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_TABSTOP = INT(Z'00010000', c_long)
    INTEGER(c_long), PARAMETER :: WS_BORDER = INT(Z'00800000', c_long)
    INTEGER(c_long), PARAMETER :: ES_AUTOHSCROLL = INT(Z'00000080', c_long)
    INTEGER(c_long), PARAMETER :: WS_EX_CLIENTEDGE = INT(Z'00000200', c_long)
    INTEGER(c_int), PARAMETER :: MB_OK = 0_c_int
    INTEGER(c_int), PARAMETER :: MB_ICONERROR = 16_c_int
    INTEGER(c_int), PARAMETER :: VK_RETURN = 13_c_int
    INTEGER(c_int), PARAMETER :: GWL_WNDPROC = -4_c_int
    INTEGER, PARAMETER :: DRAW_ACCEPTANCE_CP = 50
    INTEGER, PARAMETER :: MAX_GUI_TEXT = 256
    INTEGER(c_int), PARAMETER :: WINDOW_WIDTH = 980_c_int
    INTEGER(c_int), PARAMETER :: WINDOW_HEIGHT = 760_c_int
    INTEGER(c_int), PARAMETER :: BOARD_LEFT = 24_c_int
    INTEGER(c_int), PARAMETER :: BOARD_TOP = 84_c_int
    INTEGER(c_int), PARAMETER :: BOARD_PIXELS = 512_c_int
    INTEGER(c_int), PARAMETER :: SQUARE_PIXELS = BOARD_PIXELS / 8_c_int
    INTEGER(c_int), PARAMETER :: PANEL_LEFT = 580_c_int

    TYPE, BIND(C) :: POINT
        INTEGER(c_long) :: x
        INTEGER(c_long) :: y
    END TYPE POINT

    TYPE, BIND(C) :: RECT
        INTEGER(c_long) :: left
        INTEGER(c_long) :: top
        INTEGER(c_long) :: right
        INTEGER(c_long) :: bottom
    END TYPE RECT

    TYPE, BIND(C) :: MSG
        TYPE(c_ptr) :: hwnd
        INTEGER(c_int) :: message
        INTEGER(c_intptr_t) :: wParam
        INTEGER(c_intptr_t) :: lParam
        INTEGER(c_long) :: time
        TYPE(POINT) :: pt
    END TYPE MSG

    TYPE, BIND(C) :: PAINTSTRUCT
        TYPE(c_ptr) :: hdc
        INTEGER(c_int) :: fErase
        TYPE(RECT) :: rcPaint
        INTEGER(c_int) :: fRestore
        INTEGER(c_int) :: fIncUpdate
        CHARACTER(KIND=c_char), DIMENSION(32) :: rgbReserved
    END TYPE PAINTSTRUCT

    TYPE, BIND(C) :: WNDCLASSEXA
        INTEGER(c_int) :: cbSize
        INTEGER(c_int) :: style
        TYPE(c_funptr) :: lpfnWndProc
        INTEGER(c_int) :: cbClsExtra
        INTEGER(c_int) :: cbWndExtra
        TYPE(c_ptr) :: hInstance
        TYPE(c_ptr) :: hIcon
        TYPE(c_ptr) :: hCursor
        TYPE(c_ptr) :: hbrBackground
        TYPE(c_ptr) :: lpszMenuName
        TYPE(c_ptr) :: lpszClassName
        TYPE(c_ptr) :: hIconSm
    END TYPE WNDCLASSEXA

    TYPE, BIND(C) :: GdiplusStartupInput
        INTEGER(c_int) :: GdiplusVersion
        TYPE(c_funptr) :: DebugEventCallback
        INTEGER(c_int) :: SuppressBackgroundThread
        INTEGER(c_int) :: SuppressExternalCodecs
    END TYPE GdiplusStartupInput

    TYPE(Board_Type), SAVE :: gui_board
    TYPE(c_ptr), SAVE :: gui_main_hwnd = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_color_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_time_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_depth_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_book_limit_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_move_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_start_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_submit_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_suggest_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_autoplay_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_takeback_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_status_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_white_clock = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_black_clock = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_turn_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_eval_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_setup_help = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_draw_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_resign_button = c_null_ptr
    TYPE(c_ptr), SAVE :: piece_images(2, 6)
    TYPE(c_ptr), SAVE :: gui_instance = c_null_ptr
    INTEGER(c_size_t), SAVE :: gdiplus_token = 0_c_size_t
    LOGICAL, SAVE :: gdiplus_started = .FALSE.
    LOGICAL, SAVE :: piece_images_loaded = .FALSE.
    LOGICAL, SAVE :: gui_game_started = .FALSE.
    LOGICAL, SAVE :: gui_game_over = .FALSE.
    LOGICAL, SAVE :: gui_time_control_enabled = .FALSE.
    LOGICAL, SAVE :: gui_game_lost_on_time = .FALSE.
    LOGICAL, SAVE :: gui_autoplay_mode = .FALSE.
    INTEGER, SAVE :: gui_human_color = WHITE
    INTEGER, SAVE :: gui_ai_color = BLACK
    INTEGER, SAVE :: gui_search_depth = DEFAULT_SEARCH_DEPTH
    INTEGER, SAVE :: gui_book_max_moves_per_side = -1
    INTEGER, SAVE :: gui_game_winner_color = NO_COLOR
    INTEGER, SAVE :: gui_current_game_status = GAME_ONGOING
    INTEGER, SAVE :: gui_time_forfeit_winner_color = NO_COLOR
    INTEGER(KIND=8), SAVE :: gui_white_time_ms = 0
    INTEGER(KIND=8), SAVE :: gui_black_time_ms = 0
    INTEGER(KIND=8), SAVE :: gui_increment_ms = 0
    INTEGER(KIND=8), SAVE :: gui_turn_start_count = 0
    INTEGER(KIND=8), SAVE :: gui_count_rate = 0
    INTEGER, SAVE :: gui_num_half_moves = 0
    CHARACTER(LEN=32), DIMENSION(512), SAVE :: gui_move_history = ''
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_move_elapsed_ms_history = -1_8
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_white_time_before_move = 0_8
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_black_time_before_move = 0_8
    INTEGER(KIND=8), DIMENSION(513), SAVE :: gui_position_key_history = 0_8
    TYPE(Move_Type), DIMENSION(512), SAVE :: gui_played_moves
    TYPE(UnmakeInfo_Type), DIMENSION(512), SAVE :: gui_unmake_history
    CHARACTER(LEN=7), SAVE :: gui_pgn_result = '*'
    CHARACTER(LEN=32), SAVE :: gui_pgn_time_control_tag = '-'
    CHARACTER(LEN=256), SAVE :: gui_status_text = 'Enter color and time control, then press Start.'
    TYPE(Opening_Book_Type), SAVE :: gui_white_book, gui_black_book
    INTEGER(c_intptr_t), SAVE :: original_move_edit_proc = 0_c_intptr_t

    INTERFACE
        FUNCTION RegisterClassExA(window_class) BIND(C, NAME="RegisterClassExA")
            IMPORT :: WNDCLASSEXA, c_int
            TYPE(WNDCLASSEXA), INTENT(IN) :: window_class
            INTEGER(c_int) :: RegisterClassExA
        END FUNCTION RegisterClassExA

        FUNCTION CreateWindowExA(ex_style, class_name, window_name, style, x, y, width, height, parent, menu, instance, param) &
            BIND(C, NAME="CreateWindowExA")
            IMPORT :: c_long, c_int, c_ptr
            INTEGER(c_long), VALUE :: ex_style
            TYPE(c_ptr), VALUE :: class_name
            TYPE(c_ptr), VALUE :: window_name
            INTEGER(c_long), VALUE :: style
            INTEGER(c_int), VALUE :: x, y, width, height
            TYPE(c_ptr), VALUE :: parent, menu, instance, param
            TYPE(c_ptr) :: CreateWindowExA
        END FUNCTION CreateWindowExA

        FUNCTION DefWindowProcA(hwnd, msg, wparam, lparam) BIND(C, NAME="DefWindowProcA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: msg
            INTEGER(c_intptr_t), VALUE :: wparam, lparam
            INTEGER(c_intptr_t) :: DefWindowProcA
        END FUNCTION DefWindowProcA

        FUNCTION ShowWindow(hwnd, ncmdshow) BIND(C, NAME="ShowWindow")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: ncmdshow
            INTEGER(c_int) :: ShowWindow
        END FUNCTION ShowWindow

        FUNCTION UpdateWindow(hwnd) BIND(C, NAME="UpdateWindow")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int) :: UpdateWindow
        END FUNCTION UpdateWindow

        FUNCTION GetMessageA(message_buffer, hwnd, min_filter, max_filter) BIND(C, NAME="GetMessageA")
            IMPORT :: MSG, c_ptr, c_int
            TYPE(MSG), INTENT(OUT) :: message_buffer
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: min_filter, max_filter
            INTEGER(c_int) :: GetMessageA
        END FUNCTION GetMessageA

        FUNCTION TranslateMessage(message_buffer) BIND(C, NAME="TranslateMessage")
            IMPORT :: MSG, c_int
            TYPE(MSG), INTENT(IN) :: message_buffer
            INTEGER(c_int) :: TranslateMessage
        END FUNCTION TranslateMessage

        FUNCTION DispatchMessageA(message_buffer) BIND(C, NAME="DispatchMessageA")
            IMPORT :: MSG, c_intptr_t
            TYPE(MSG), INTENT(IN) :: message_buffer
            INTEGER(c_intptr_t) :: DispatchMessageA
        END FUNCTION DispatchMessageA

        SUBROUTINE PostQuitMessage(exit_code) BIND(C, NAME="PostQuitMessage")
            IMPORT :: c_int
            INTEGER(c_int), VALUE :: exit_code
        END SUBROUTINE PostQuitMessage

        FUNCTION BeginPaint(hwnd, paint_struct) BIND(C, NAME="BeginPaint")
            IMPORT :: c_ptr, PAINTSTRUCT
            TYPE(c_ptr), VALUE :: hwnd
            TYPE(PAINTSTRUCT), INTENT(OUT) :: paint_struct
            TYPE(c_ptr) :: BeginPaint
        END FUNCTION BeginPaint

        FUNCTION EndPaint(hwnd, paint_struct) BIND(C, NAME="EndPaint")
            IMPORT :: c_ptr, PAINTSTRUCT, c_int
            TYPE(c_ptr), VALUE :: hwnd
            TYPE(PAINTSTRUCT), INTENT(IN) :: paint_struct
            INTEGER(c_int) :: EndPaint
        END FUNCTION EndPaint

        FUNCTION FillRect(hdc, rect_buffer, brush) BIND(C, NAME="FillRect")
            IMPORT :: c_ptr, RECT, c_int
            TYPE(c_ptr), VALUE :: hdc
            TYPE(RECT), INTENT(IN) :: rect_buffer
            TYPE(c_ptr), VALUE :: brush
            INTEGER(c_int) :: FillRect
        END FUNCTION FillRect

        FUNCTION CreateSolidBrush(color) BIND(C, NAME="CreateSolidBrush")
            IMPORT :: c_long, c_ptr
            INTEGER(c_long), VALUE :: color
            TYPE(c_ptr) :: CreateSolidBrush
        END FUNCTION CreateSolidBrush

        FUNCTION DeleteObject(object) BIND(C, NAME="DeleteObject")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: object
            INTEGER(c_int) :: DeleteObject
        END FUNCTION DeleteObject

        FUNCTION SetTextColor(hdc, color) BIND(C, NAME="SetTextColor")
            IMPORT :: c_ptr, c_long
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_long), VALUE :: color
            INTEGER(c_long) :: SetTextColor
        END FUNCTION SetTextColor

        FUNCTION SetBkMode(hdc, mode) BIND(C, NAME="SetBkMode")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: mode
            INTEGER(c_int) :: SetBkMode
        END FUNCTION SetBkMode

        FUNCTION TextOutA(hdc, x, y, text, text_len) BIND(C, NAME="TextOutA")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: x, y
            TYPE(c_ptr), VALUE :: text
            INTEGER(c_int), VALUE :: text_len
            INTEGER(c_int) :: TextOutA
        END FUNCTION TextOutA

        FUNCTION GetClientRect(hwnd, rect_buffer) BIND(C, NAME="GetClientRect")
            IMPORT :: c_ptr, RECT, c_int
            TYPE(c_ptr), VALUE :: hwnd
            TYPE(RECT), INTENT(OUT) :: rect_buffer
            INTEGER(c_int) :: GetClientRect
        END FUNCTION GetClientRect

        FUNCTION GetModuleHandleA(module_name) BIND(C, NAME="GetModuleHandleA")
            IMPORT :: c_ptr
            TYPE(c_ptr), VALUE :: module_name
            TYPE(c_ptr) :: GetModuleHandleA
        END FUNCTION GetModuleHandleA

        FUNCTION SetWindowTextA(hwnd, text_ptr) BIND(C, NAME="SetWindowTextA")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, text_ptr
            INTEGER(c_int) :: SetWindowTextA
        END FUNCTION SetWindowTextA

        FUNCTION GetWindowTextA(hwnd, text_ptr, max_count) BIND(C, NAME="GetWindowTextA")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, text_ptr
            INTEGER(c_int), VALUE :: max_count
            INTEGER(c_int) :: GetWindowTextA
        END FUNCTION GetWindowTextA

        FUNCTION EnableWindow(hwnd, enable_flag) BIND(C, NAME="EnableWindow")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: enable_flag
            INTEGER(c_int) :: EnableWindow
        END FUNCTION EnableWindow

        FUNCTION InvalidateRect(hwnd, rect_ptr, erase_flag) BIND(C, NAME="InvalidateRect")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, rect_ptr
            INTEGER(c_int), VALUE :: erase_flag
            INTEGER(c_int) :: InvalidateRect
        END FUNCTION InvalidateRect

        FUNCTION SetTimer(hwnd, timer_id, elapsed_ms, timer_proc) BIND(C, NAME="SetTimer")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd, timer_proc
            INTEGER(c_intptr_t), VALUE :: timer_id
            INTEGER(c_int), VALUE :: elapsed_ms
            INTEGER(c_intptr_t) :: SetTimer
        END FUNCTION SetTimer

        FUNCTION KillTimer(hwnd, timer_id) BIND(C, NAME="KillTimer")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_intptr_t), VALUE :: timer_id
            INTEGER(c_int) :: KillTimer
        END FUNCTION KillTimer

        FUNCTION MessageBoxA(hwnd, text_ptr, caption_ptr, box_type) BIND(C, NAME="MessageBoxA")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, text_ptr, caption_ptr
            INTEGER(c_int), VALUE :: box_type
            INTEGER(c_int) :: MessageBoxA
        END FUNCTION MessageBoxA

        FUNCTION SetWindowLongPtrA(hwnd, index_value, new_value) BIND(C, NAME="SetWindowLongPtrA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: index_value
            INTEGER(c_intptr_t), VALUE :: new_value
            INTEGER(c_intptr_t) :: SetWindowLongPtrA
        END FUNCTION SetWindowLongPtrA

        FUNCTION CallWindowProcA(prev_proc, hwnd, msg, wparam, lparam) BIND(C, NAME="CallWindowProcA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            INTEGER(c_intptr_t), VALUE :: prev_proc
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: msg
            INTEGER(c_intptr_t), VALUE :: wparam, lparam
            INTEGER(c_intptr_t) :: CallWindowProcA
        END FUNCTION CallWindowProcA

        FUNCTION GdiplusStartup(token, input, output) BIND(C, NAME="GdiplusStartup")
            IMPORT :: c_size_t, GdiplusStartupInput, c_ptr, c_int
            INTEGER(c_size_t), INTENT(OUT) :: token
            TYPE(GdiplusStartupInput), INTENT(IN) :: input
            TYPE(c_ptr), VALUE :: output
            INTEGER(c_int) :: GdiplusStartup
        END FUNCTION GdiplusStartup

        SUBROUTINE GdiplusShutdown(token) BIND(C, NAME="GdiplusShutdown")
            IMPORT :: c_size_t
            INTEGER(c_size_t), VALUE :: token
        END SUBROUTINE GdiplusShutdown

        FUNCTION GdipCreateFromHDC(hdc, graphics) BIND(C, NAME="GdipCreateFromHDC")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            TYPE(c_ptr), INTENT(OUT) :: graphics
            INTEGER(c_int) :: GdipCreateFromHDC
        END FUNCTION GdipCreateFromHDC

        FUNCTION GdipDeleteGraphics(graphics) BIND(C, NAME="GdipDeleteGraphics")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: graphics
            INTEGER(c_int) :: GdipDeleteGraphics
        END FUNCTION GdipDeleteGraphics

        FUNCTION GdipCreateBitmapFromFile(filename_ptr, image) BIND(C, NAME="GdipCreateBitmapFromFile")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: filename_ptr
            TYPE(c_ptr), INTENT(OUT) :: image
            INTEGER(c_int) :: GdipCreateBitmapFromFile
        END FUNCTION GdipCreateBitmapFromFile

        FUNCTION GdipDrawImageRectI(graphics, image, x, y, width, height) BIND(C, NAME="GdipDrawImageRectI")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: graphics, image
            INTEGER(c_int), VALUE :: x, y, width, height
            INTEGER(c_int) :: GdipDrawImageRectI
        END FUNCTION GdipDrawImageRectI

        FUNCTION GdipDisposeImage(image) BIND(C, NAME="GdipDisposeImage")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: image
            INTEGER(c_int) :: GdipDisposeImage
        END FUNCTION GdipDisposeImage
    END INTERFACE

CONTAINS

    SUBROUTINE run_chess_gui()
        TYPE(WNDCLASSEXA) :: window_class
        TYPE(MSG) :: message_data
        TYPE(c_ptr) :: window_handle
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_name
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: window_title
        INTEGER(c_int) :: message_status, ignored_int
        INTEGER(c_intptr_t) :: ignored_result

        CALL start_gui_debug_session()
        CALL log_gui_checkpoint('run_chess_gui', 'entered')
        CALL SYSTEM_CLOCK(count_rate=gui_count_rate)
        CALL log_gui_value('gui_count_rate', int64_to_text(gui_count_rate))
        CALL init_zobrist_keys()
        CALL log_gui_checkpoint('run_chess_gui', 'zobrist initialized')
        CALL init_board(gui_board)
        CALL log_gui_checkpoint('run_chess_gui', 'board initialized')

        CALL set_c_string('FortranChessGUI', class_name)
        CALL set_c_string('Fortran Chess GUI', window_title)

        gui_instance = GetModuleHandleA(c_null_ptr)
        CALL log_gui_value('gui_instance', intptr_to_text(transfer_ptr(gui_instance)))
        CALL initialize_gdiplus_and_images()

        window_class%cbSize = c_sizeof(window_class)
        window_class%style = 0_c_int
        window_class%lpfnWndProc = c_funloc(window_proc)
        window_class%cbClsExtra = 0_c_int
        window_class%cbWndExtra = 0_c_int
        window_class%hInstance = gui_instance
        window_class%hIcon = c_null_ptr
        window_class%hCursor = c_null_ptr
        window_class%hbrBackground = c_null_ptr
        window_class%lpszMenuName = c_null_ptr
        window_class%lpszClassName = c_loc(class_name(1))
        window_class%hIconSm = c_null_ptr

        IF (RegisterClassExA(window_class) == 0_c_int) THEN
            CALL log_gui_failure('run_chess_gui', 'RegisterClassExA failed')
            CALL finish_gui_debug_session('RegisterClassExA failed.')
            ERROR STOP 'Could not register the GUI window class.'
        END IF
        CALL log_gui_checkpoint('run_chess_gui', 'window class registered')

        window_handle = CreateWindowExA(0_c_long, c_loc(class_name(1)), c_loc(window_title(1)), WS_OVERLAPPEDWINDOW, &
            CW_USEDEFAULT, CW_USEDEFAULT, WINDOW_WIDTH, WINDOW_HEIGHT, c_null_ptr, c_null_ptr, gui_instance, c_null_ptr)
        IF (.NOT. C_ASSOCIATED(window_handle)) THEN
            CALL log_gui_failure('run_chess_gui', 'CreateWindowExA failed')
            CALL finish_gui_debug_session('CreateWindowExA failed.')
            ERROR STOP 'Could not create the GUI window.'
        END IF
        gui_main_hwnd = window_handle
        CALL log_gui_value('gui_main_hwnd', intptr_to_text(transfer_ptr(gui_main_hwnd)))

        ignored_int = ShowWindow(window_handle, SW_SHOWNORMAL)
        ignored_int = UpdateWindow(window_handle)
        CALL log_gui_checkpoint('run_chess_gui', 'window shown')

        DO
            message_status = GetMessageA(message_data, c_null_ptr, 0_c_int, 0_c_int)
            IF (message_status <= 0_c_int) EXIT
            ignored_int = TranslateMessage(message_data)
            ignored_result = DispatchMessageA(message_data)
        END DO

        CALL log_gui_value('GetMessageA final status', int_to_text(message_status))
        CALL log_gui_checkpoint('run_chess_gui', 'message loop ended')
        CALL release_piece_images()
        IF (gdiplus_started) CALL GdiplusShutdown(gdiplus_token)
        CALL finish_gui_debug_session('GUI loop exited normally.')
    END SUBROUTINE run_chess_gui

    RECURSIVE INTEGER(c_intptr_t) FUNCTION window_proc(hwnd, msg, wparam, lparam) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: msg
        INTEGER(c_intptr_t), VALUE :: wparam, lparam
        TYPE(PAINTSTRUCT) :: paint_struct
        TYPE(c_ptr) :: device_context, command_hwnd
        INTEGER(c_int) :: ignored_int
        INTEGER(c_intptr_t) :: ignored_result

        SELECT CASE (msg)
        CASE (WM_CREATE)
            CALL log_gui_message('WM_CREATE', wparam, lparam)
            CALL create_gui_controls(hwnd)
            ignored_result = SetTimer(hwnd, TIMER_CLOCK, 1000_c_int, c_null_ptr)
            CALL log_gui_checkpoint('window_proc', 'timer started')
            window_proc = 0_c_intptr_t
        CASE (WM_PAINT)
            CALL log_gui_message('WM_PAINT', wparam, lparam)
            device_context = BeginPaint(hwnd, paint_struct)
            CALL draw_gui(device_context, hwnd)
            ignored_int = EndPaint(hwnd, paint_struct)
            window_proc = 0_c_intptr_t
        CASE (WM_COMMAND)
            CALL log_gui_message('WM_COMMAND', wparam, lparam)
            command_hwnd = ptr_from_intptr(lparam)
            IF (C_ASSOCIATED(command_hwnd, hwnd_start_button)) THEN
                CALL log_gui_event('Start / Reset button pressed')
                CALL start_new_game(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_submit_button)) THEN
                CALL log_gui_event('Play Move button pressed')
                CALL submit_human_move(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_suggest_button)) THEN
                CALL log_gui_event('Suggest Move button pressed')
                CALL suggest_gui_move(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_autoplay_button)) THEN
                CALL log_gui_event('Autoplay button pressed')
                CALL autoplay_gui_game(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_takeback_button)) THEN
                CALL log_gui_event('Takeback button pressed')
                CALL takeback_gui_move(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_move_edit)) THEN
                CALL log_gui_event('Move edit notification')
                CALL maybe_submit_move_from_edit(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_draw_button)) THEN
                CALL log_gui_event('Offer Draw button pressed')
                CALL offer_draw(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_resign_button)) THEN
                CALL log_gui_event('Resign button pressed')
                CALL resign_game(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            END IF
            window_proc = DefWindowProcA(hwnd, msg, wparam, lparam)
        CASE (WM_TIMER)
            CALL log_gui_message('WM_TIMER', wparam, lparam)
            CALL on_gui_timer(hwnd, wparam)
            window_proc = 0_c_intptr_t
        CASE (WM_DESTROY)
            CALL log_gui_message('WM_DESTROY', wparam, lparam)
            ignored_int = KillTimer(hwnd, TIMER_CLOCK)
            CALL PostQuitMessage(0_c_int)
            window_proc = 0_c_intptr_t
        CASE DEFAULT
            window_proc = DefWindowProcA(hwnd, msg, wparam, lparam)
        END SELECT
    END FUNCTION window_proc

    RECURSIVE INTEGER(c_intptr_t) FUNCTION move_edit_proc(hwnd, msg, wparam, lparam) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: msg
        INTEGER(c_intptr_t), VALUE :: wparam, lparam

        IF (msg == WM_KEYDOWN .AND. wparam == VK_RETURN) THEN
            CALL log_gui_event('Move edit Enter key pressed')
            CALL submit_human_move(gui_main_hwnd)
            move_edit_proc = 0_c_intptr_t
            RETURN
        END IF

        move_edit_proc = CallWindowProcA(original_move_edit_proc, hwnd, msg, wparam, lparam)
    END FUNCTION move_edit_proc

    SUBROUTINE create_gui_controls(parent_hwnd)
        TYPE(c_ptr), VALUE :: parent_hwnd
        CHARACTER(LEN=16) :: default_depth_text

        CALL log_gui_checkpoint('create_gui_controls', 'entered')
        WRITE(default_depth_text, '(I0)') DEFAULT_SEARCH_DEPTH
        hwnd_setup_help = create_control('STATIC', 'Color, time, depth, and book max (blank = unlimited).', &
            0_c_long, WS_CHILD + WS_VISIBLE, PANEL_LEFT, 24_c_int, 380_c_int, 20_c_int, parent_hwnd)
        hwnd_color_edit = create_control('EDIT', 'white', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, PANEL_LEFT, 64_c_int, 76_c_int, 24_c_int, parent_hwnd)
        hwnd_time_edit = create_control('EDIT', '3+2', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, PANEL_LEFT + 88_c_int, 64_c_int, 72_c_int, 24_c_int, parent_hwnd)
        hwnd_depth_edit = create_control('EDIT', TRIM(default_depth_text), WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, PANEL_LEFT + 172_c_int, 64_c_int, 44_c_int, 24_c_int, parent_hwnd)
        hwnd_book_limit_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, PANEL_LEFT + 228_c_int, 64_c_int, 60_c_int, 24_c_int, parent_hwnd)
        hwnd_start_button = create_control('BUTTON', 'Start / Reset', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 300_c_int, 62_c_int, 100_c_int, 28_c_int, parent_hwnd)
        hwnd_turn_static = create_control('STATIC', 'Game not started', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 112_c_int, 320_c_int, 20_c_int, parent_hwnd)
        hwnd_white_clock = create_control('STATIC', 'White: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 144_c_int, 180_c_int, 20_c_int, parent_hwnd)
        hwnd_black_clock = create_control('STATIC', 'Black: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT + 190_c_int, 144_c_int, 180_c_int, 20_c_int, parent_hwnd)
        hwnd_eval_static = create_control('STATIC', 'Eval (White): 0.00', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 170_c_int, 220_c_int, 20_c_int, parent_hwnd)
        hwnd_move_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, &
            PANEL_LEFT, 198_c_int, 220_c_int, 28_c_int, parent_hwnd)
        hwnd_submit_button = create_control('BUTTON', 'Play Move', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 232_c_int, 196_c_int, 104_c_int, 30_c_int, parent_hwnd)
        hwnd_suggest_button = create_control('BUTTON', 'Suggest Move', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 238_c_int, 108_c_int, 30_c_int, parent_hwnd)
        hwnd_autoplay_button = create_control('BUTTON', 'Autoplay', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 120_c_int, 238_c_int, 100_c_int, 30_c_int, parent_hwnd)
        hwnd_takeback_button = create_control('BUTTON', 'Takeback', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 232_c_int, 238_c_int, 100_c_int, 30_c_int, parent_hwnd)
        hwnd_draw_button = create_control('BUTTON', 'Offer Draw', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 276_c_int, 108_c_int, 30_c_int, parent_hwnd)
        hwnd_resign_button = create_control('BUTTON', 'Resign', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 120_c_int, 276_c_int, 90_c_int, 30_c_int, parent_hwnd)
        hwnd_status_static = create_control('STATIC', TRIM(gui_status_text), 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 320_c_int, 340_c_int, 120_c_int, parent_hwnd)

        original_move_edit_proc = SetWindowLongPtrA(hwnd_move_edit, GWL_WNDPROC, funptr_to_intptr(c_funloc(move_edit_proc)))
        CALL log_gui_value('original_move_edit_proc', intptr_to_text(original_move_edit_proc))

        CALL enable_game_controls(.FALSE.)
        CALL refresh_gui_labels()
        CALL log_gui_checkpoint('create_gui_controls', 'completed')
    END SUBROUTINE create_gui_controls

    FUNCTION create_control(class_name, text, ex_style, style, x, y, width, height, parent_hwnd) RESULT(control_hwnd)
        CHARACTER(LEN=*), INTENT(IN) :: class_name, text
        INTEGER(c_long), INTENT(IN) :: ex_style, style
        INTEGER(c_int), INTENT(IN) :: x, y, width, height
        TYPE(c_ptr), VALUE :: parent_hwnd
        TYPE(c_ptr) :: control_hwnd
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_buffer
        CHARACTER(KIND=c_char), DIMENSION(256), TARGET :: text_buffer

        CALL set_c_string(class_name, class_buffer)
        CALL set_c_string(text, text_buffer)
        control_hwnd = CreateWindowExA(ex_style, c_loc(class_buffer(1)), c_loc(text_buffer(1)), style, x, y, width, height, &
            parent_hwnd, c_null_ptr, gui_instance, c_null_ptr)
        IF (.NOT. C_ASSOCIATED(control_hwnd)) THEN
            CALL log_gui_failure('create_control', 'failed to create ' // TRIM(class_name) // ' [' // TRIM(text) // ']')
        END IF
    END FUNCTION create_control

    SUBROUTINE initialize_gdiplus_and_images()
        TYPE(GdiplusStartupInput) :: gdiplus_input

        CALL log_gui_checkpoint('initialize_gdiplus_and_images', 'entered')
        piece_images = c_null_ptr
        gdiplus_input%GdiplusVersion = 1_c_int
        gdiplus_input%DebugEventCallback = c_null_funptr
        gdiplus_input%SuppressBackgroundThread = 0_c_int
        gdiplus_input%SuppressExternalCodecs = 0_c_int

        IF (GdiplusStartup(gdiplus_token, gdiplus_input, c_null_ptr) == GDI_PLUS_OK) THEN
            gdiplus_started = .TRUE.
            CALL log_gui_checkpoint('initialize_gdiplus_and_images', 'gdiplus started')
            CALL load_piece_images()
        ELSE
            CALL log_gui_failure('initialize_gdiplus_and_images', 'GdiplusStartup failed')
        END IF
    END SUBROUTINE initialize_gdiplus_and_images

    SUBROUTINE load_piece_images()
        CALL log_gui_checkpoint('load_piece_images', 'entered')
        piece_images_loaded = .FALSE.
        IF (.NOT. gdiplus_started) RETURN

        CALL load_one_piece_image('assets\pieces\sashite-western\wk.png', piece_images(1, KING))
        CALL load_one_piece_image('assets\pieces\sashite-western\wq.png', piece_images(1, QUEEN))
        CALL load_one_piece_image('assets\pieces\sashite-western\wr.png', piece_images(1, ROOK))
        CALL load_one_piece_image('assets\pieces\sashite-western\wb.png', piece_images(1, BISHOP))
        CALL load_one_piece_image('assets\pieces\sashite-western\wn.png', piece_images(1, KNIGHT))
        CALL load_one_piece_image('assets\pieces\sashite-western\wp.png', piece_images(1, PAWN))
        CALL load_one_piece_image('assets\pieces\sashite-western\bk.png', piece_images(2, KING))
        CALL load_one_piece_image('assets\pieces\sashite-western\bq.png', piece_images(2, QUEEN))
        CALL load_one_piece_image('assets\pieces\sashite-western\br.png', piece_images(2, ROOK))
        CALL load_one_piece_image('assets\pieces\sashite-western\bb.png', piece_images(2, BISHOP))
        CALL load_one_piece_image('assets\pieces\sashite-western\bn.png', piece_images(2, KNIGHT))
        CALL load_one_piece_image('assets\pieces\sashite-western\bp.png', piece_images(2, PAWN))

        piece_images_loaded = C_ASSOCIATED(piece_images(1, KING))
        CALL log_gui_value('piece_images_loaded', logical_to_text(piece_images_loaded))
    END SUBROUTINE load_piece_images

    SUBROUTINE load_one_piece_image(filename, image_out)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(c_ptr), INTENT(OUT) :: image_out
        INTEGER(c_int16_t), DIMENSION(260), TARGET :: wide_filename

        image_out = c_null_ptr
        CALL set_wide_string(filename, wide_filename)
        IF (GdipCreateBitmapFromFile(c_loc(wide_filename(1)), image_out) /= GDI_PLUS_OK) THEN
            image_out = c_null_ptr
            CALL log_gui_failure('load_one_piece_image', filename)
        ELSE
            CALL log_gui_event('Loaded piece image ' // TRIM(filename))
        END IF
    END SUBROUTINE load_one_piece_image

    SUBROUTINE release_piece_images()
        INTEGER :: color_idx, piece_idx
        INTEGER(c_int) :: ignored_int

        CALL log_gui_checkpoint('release_piece_images', 'entered')
        DO color_idx = 1, 2
            DO piece_idx = 1, 6
                IF (C_ASSOCIATED(piece_images(color_idx, piece_idx))) THEN
                    ignored_int = GdipDisposeImage(piece_images(color_idx, piece_idx))
                    piece_images(color_idx, piece_idx) = c_null_ptr
                END IF
            END DO
        END DO
    END SUBROUTINE release_piece_images

    SUBROUTINE draw_gui(device_context, hwnd)
        TYPE(c_ptr), VALUE :: device_context, hwnd
        TYPE(RECT) :: client_rect, square_rect
        TYPE(c_ptr) :: light_brush, dark_brush, background_brush, graphics
        INTEGER(c_int) :: ignored_int
        INTEGER(c_long) :: ignored_color
        INTEGER :: rank, file, board_rank, board_file, color_idx, piece_idx
        CHARACTER(LEN=1) :: piece_char
        CHARACTER(KIND=c_char), DIMENSION(2), TARGET :: piece_text

        CALL log_gui_checkpoint('draw_gui', 'entered')
        ignored_int = GetClientRect(hwnd, client_rect)
        background_brush = CreateSolidBrush(rgb_color(247, 244, 237))
        square_rect%left = 0_c_long
        square_rect%top = 0_c_long
        square_rect%right = client_rect%right
        square_rect%bottom = client_rect%bottom
        ignored_int = FillRect(device_context, square_rect, background_brush)
        ignored_int = DeleteObject(background_brush)

        light_brush = CreateSolidBrush(rgb_color(240, 217, 181))
        dark_brush = CreateSolidBrush(rgb_color(181, 136, 99))
        ignored_int = SetBkMode(device_context, TRANSPARENT)
        ignored_color = SetTextColor(device_context, rgb_color(40, 40, 40))
        CALL draw_text(device_context, BOARD_LEFT, 32_c_int, 'Fortran Chess GUI')
        CALL draw_text(device_context, BOARD_LEFT, 52_c_int, 'Move entry accepts SAN or coordinate notation.')

        graphics = c_null_ptr
        IF (gdiplus_started) ignored_int = GdipCreateFromHDC(device_context, graphics)

        DO rank = 8, 1, -1
            DO file = 1, 8
                square_rect%left = INT(BOARD_LEFT + (file - 1) * SQUARE_PIXELS, c_long)
                square_rect%top = INT(BOARD_TOP + (8 - rank) * SQUARE_PIXELS, c_long)
                square_rect%right = square_rect%left + SQUARE_PIXELS
                square_rect%bottom = square_rect%top + SQUARE_PIXELS

                IF (gui_human_color == WHITE) THEN
                    board_rank = rank
                    board_file = file
                ELSE
                    board_rank = 9 - rank
                    board_file = 9 - file
                END IF

                IF (MOD(rank + file, 2) == 1) THEN
                    ignored_int = FillRect(device_context, square_rect, light_brush)
                ELSE
                    ignored_int = FillRect(device_context, square_rect, dark_brush)
                END IF

                piece_idx = gui_board%squares_piece(board_rank, board_file)
                IF (piece_idx == NO_PIECE) CYCLE
                IF (gui_board%squares_color(board_rank, board_file) == WHITE) THEN
                    color_idx = 1
                ELSE
                    color_idx = 2
                END IF

                IF (C_ASSOCIATED(graphics) .AND. C_ASSOCIATED(piece_images(color_idx, piece_idx))) THEN
                    ignored_int = GdipDrawImageRectI(graphics, piece_images(color_idx, piece_idx), &
                        INT(square_rect%left, c_int) + 6_c_int, INT(square_rect%top, c_int) + 6_c_int, &
                        SQUARE_PIXELS - 12_c_int, SQUARE_PIXELS - 12_c_int)
                ELSE
                    piece_char = board_piece_char(gui_board, board_rank, board_file)
                    piece_text(1) = ACHAR(IACHAR(piece_char), KIND=c_char)
                    piece_text(2) = c_null_char
                    IF (gui_board%squares_color(board_rank, board_file) == WHITE) THEN
                        ignored_color = SetTextColor(device_context, rgb_color(252, 252, 252))
                    ELSE
                        ignored_color = SetTextColor(device_context, rgb_color(20, 20, 20))
                    END IF
                    ignored_int = TextOutA(device_context, INT(square_rect%left, c_int) + 26_c_int, &
                        INT(square_rect%top, c_int) + 20_c_int, c_loc(piece_text(1)), 1_c_int)
                END IF
            END DO
        END DO

        IF (C_ASSOCIATED(graphics)) ignored_int = GdipDeleteGraphics(graphics)
        ignored_int = DeleteObject(light_brush)
        ignored_int = DeleteObject(dark_brush)
        CALL log_gui_checkpoint('draw_gui', 'completed')
    END SUBROUTINE draw_gui

    SUBROUTINE start_new_game(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=MAX_GUI_TEXT) :: color_text, time_text, depth_text, book_limit_text
        LOGICAL :: parsed_ok
        INTEGER :: parsed_book_limit, parsed_depth

        CALL log_gui_checkpoint('start_new_game', 'entered')
        IF (.NOT. validate_gui_opening_books(main_hwnd)) RETURN

        color_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_color_edit))))
        time_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_time_edit))))
        depth_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_depth_edit))))
        book_limit_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_book_limit_edit))))
        CALL log_gui_value('start_new_game color', color_text)
        CALL log_gui_value('start_new_game time', time_text)
        CALL log_gui_value('start_new_game depth', depth_text)
        CALL log_gui_value('start_new_game book max', book_limit_text)

        IF (color_text == 'white' .OR. color_text == 'w') THEN
            gui_human_color = WHITE
            gui_ai_color = BLACK
        ELSE IF (color_text == 'black' .OR. color_text == 'b') THEN
            gui_human_color = BLACK
            gui_ai_color = WHITE
        ELSE
            CALL set_status('Color must be white or black.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('start_new_game', 'invalid color input')
            RETURN
        END IF

        gui_time_control_enabled = .FALSE.
        gui_white_time_ms = 0
        gui_black_time_ms = 0
        gui_increment_ms = 0
        gui_search_depth = DEFAULT_SEARCH_DEPTH
        gui_book_max_moves_per_side = -1
        gui_pgn_time_control_tag = '-'
        IF (LEN_TRIM(time_text) > 0 .AND. time_text /= 'off' .AND. time_text /= 'none') THEN
            parsed_ok = parse_time_control(time_text, gui_white_time_ms, gui_increment_ms)
            IF (.NOT. parsed_ok) THEN
                CALL set_status('Time control must be blank/off or in the form 3+2.')
                CALL refresh_gui_labels()
                CALL log_gui_failure('start_new_game', 'invalid time control input')
                RETURN
            END IF
            gui_time_control_enabled = .TRUE.
            gui_black_time_ms = gui_white_time_ms
            gui_pgn_time_control_tag = format_pgn_time_control_tag(gui_white_time_ms, gui_increment_ms)
        END IF

        parsed_ok = parse_nonnegative_integer(depth_text, parsed_depth)
        IF (.NOT. parsed_ok .OR. parsed_depth < 1 .OR. parsed_depth > 10) THEN
            CALL set_status('Depth must be an integer from 1 to 10. Depth is in ply.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('start_new_game', 'invalid depth input')
            RETURN
        END IF
        gui_search_depth = parsed_depth

        IF (LEN_TRIM(book_limit_text) > 0 .AND. book_limit_text /= 'off' .AND. book_limit_text /= 'none' .AND. &
            book_limit_text /= 'unlimited') THEN
            parsed_ok = parse_nonnegative_integer(book_limit_text, parsed_book_limit)
            IF (.NOT. parsed_ok) THEN
                CALL set_status('Book max must be blank/unlimited or a nonnegative integer.')
                CALL refresh_gui_labels()
                CALL log_gui_failure('start_new_game', 'invalid book limit input')
                RETURN
            END IF
            gui_book_max_moves_per_side = parsed_book_limit
        END IF

        CALL init_board(gui_board)
        gui_game_started = .TRUE.
        gui_game_over = .FALSE.
        gui_game_lost_on_time = .FALSE.
        gui_autoplay_mode = .FALSE.
        gui_game_winner_color = NO_COLOR
        gui_current_game_status = GAME_ONGOING
        gui_time_forfeit_winner_color = NO_COLOR
        gui_num_half_moves = 0
        gui_move_history = ''
        gui_move_elapsed_ms_history = -1_8
        gui_white_time_before_move = 0_8
        gui_black_time_before_move = 0_8
        gui_position_key_history = 0_8
        gui_position_key_history(1) = gui_board%zobrist_key
        gui_pgn_result = '*'
        CALL enable_game_controls(.TRUE.)
        CALL set_control_text(hwnd_move_edit, '')
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL log_gui_checkpoint('start_new_game', 'game initialized')

        CALL SYSTEM_CLOCK(gui_turn_start_count)
        IF (gui_board%current_player == gui_ai_color) THEN
            CALL set_status('Computer is thinking...')
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
            CALL log_gui_event('AI moves first')
            CALL handle_post_move(main_hwnd, .FALSE., '')
        ELSE
            CALL set_status('Game started. Your move.')
            CALL refresh_gui_labels()
        END IF
        CALL log_gui_checkpoint('start_new_game', 'completed')
    END SUBROUTINE start_new_game

    SUBROUTINE submit_human_move(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: move_info
        INTEGER :: num_legal_moves, i
        LOGICAL :: move_found
        CHARACTER(LEN=MAX_GUI_TEXT) :: move_text
        CHARACTER(LEN=32) :: san_text
        INTEGER(KIND=8) :: turn_end_count, spent_ms

        CALL log_gui_checkpoint('submit_human_move', 'entered')
        IF (.NOT. gui_game_started) THEN
            CALL set_status('Press Start first.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'game not started')
            RETURN
        END IF
        IF (gui_game_over) THEN
            CALL set_status('Game is over. Press Start to begin again.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'game already over')
            RETURN
        END IF
        IF (gui_autoplay_mode) THEN
            CALL set_status('Autoplay is running.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'autoplay running')
            RETURN
        END IF
        IF (gui_board%current_player /= gui_human_color) THEN
            CALL set_status('Wait for the computer to move.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'not human turn')
            RETURN
        END IF

        move_text = TRIM(sanitize_move_text(get_control_text(hwnd_move_edit)))
        CALL log_gui_value('submit_human_move text', move_text)
        IF (LEN_TRIM(move_text) == 0) RETURN

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        move_found = .FALSE.
        DO i = 1, num_legal_moves
            IF (move_matches_input(gui_board, legal_moves(i), legal_moves, num_legal_moves, move_text)) THEN
                chosen_move = legal_moves(i)
                move_found = .TRUE.
                EXIT
            END IF
        END DO

        IF (.NOT. move_found) THEN
            CALL set_status('Invalid or illegal move.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'illegal move ' // TRIM(move_text))
            RETURN
        END IF

        CALL SYSTEM_CLOCK(turn_end_count)
        spent_ms = elapsed_milliseconds(gui_turn_start_count, turn_end_count, gui_count_rate)

        IF (gui_num_half_moves + 1 <= SIZE(gui_move_history)) THEN
            gui_white_time_before_move(gui_num_half_moves + 1) = gui_white_time_ms
            gui_black_time_before_move(gui_num_half_moves + 1) = gui_black_time_ms
        END IF

        IF (gui_time_control_enabled) THEN
            IF (.NOT. apply_clock_after_move(gui_board%current_player, spent_ms, gui_white_time_ms, gui_black_time_ms, gui_increment_ms)) THEN
                gui_game_lost_on_time = .TRUE.
                gui_time_forfeit_winner_color = get_opponent_color(gui_board%current_player)
                CALL log_gui_failure('submit_human_move', 'human flagged on time')
                CALL finish_game(main_hwnd, 'You lost on time.')
                RETURN
            END IF
        END IF

        san_text = move_to_san(gui_board, chosen_move, legal_moves, num_legal_moves)
        CALL log_gui_value('submit_human_move san', san_text)
        CALL append_move_to_history(san_text, spent_ms)
        CALL make_move(gui_board, chosen_move, move_info)
        gui_played_moves(gui_num_half_moves) = chosen_move
        gui_unmake_history(gui_num_half_moves) = move_info
        gui_position_key_history(gui_num_half_moves + 1) = gui_board%zobrist_key
        CALL set_control_text(hwnd_move_edit, '')
        CALL set_status('You played ' // TRIM(san_text) // '.')
        CALL handle_post_move(main_hwnd, .TRUE., san_text)
        CALL log_gui_checkpoint('submit_human_move', 'completed')
    END SUBROUTINE submit_human_move

    SUBROUTINE maybe_submit_move_from_edit(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=MAX_GUI_TEXT) :: move_text

        move_text = get_control_text(hwnd_move_edit)
        IF (INDEX(move_text, CHAR(10)) == 0 .AND. INDEX(move_text, CHAR(13)) == 0) RETURN
        move_text = sanitize_move_text(move_text)
        CALL log_gui_value('maybe_submit_move_from_edit sanitized', move_text)
        CALL set_control_text(hwnd_move_edit, TRIM(move_text))
        IF (LEN_TRIM(move_text) > 0) CALL submit_human_move(main_hwnd)
    END SUBROUTINE maybe_submit_move_from_edit

    SUBROUTINE process_ai_turn(main_hwnd, played_move_text)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=*), INTENT(OUT) :: played_move_text
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: move_info
        INTEGER :: num_legal_moves
        LOGICAL :: move_found, book_move_found
        CHARACTER(LEN=32) :: san_text, book_move_text
        INTEGER(KIND=8) :: turn_start_count, turn_end_count, spent_ms, clock_spent_ms
        TYPE(Opening_Book_Type) :: active_book
        REAL :: ai_time_budget_seconds

        CALL log_gui_checkpoint('process_ai_turn', 'entered')
        played_move_text = ''
        IF (gui_game_over) RETURN
        IF (.NOT. gui_autoplay_mode .AND. gui_board%current_player /= gui_ai_color) RETURN

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        IF (num_legal_moves == 0) THEN
            CALL handle_post_move(main_hwnd, .FALSE., '')
            RETURN
        END IF

        CALL set_status('Computer is thinking...')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL SYSTEM_CLOCK(turn_start_count)

        book_move_found = .FALSE.
        IF (gui_board%current_player == WHITE) THEN
            active_book = gui_white_book
        ELSE
            active_book = gui_black_book
        END IF

        ai_time_budget_seconds = get_ai_time_budget_seconds(gui_board%current_player, gui_white_time_ms, gui_black_time_ms, &
            gui_increment_ms, gui_num_half_moves, gui_time_control_enabled)
        CALL log_gui_value('process_ai_turn budget_seconds', real_to_text(ai_time_budget_seconds))
        CALL start_ai_turn_debug_log('gui', gui_board%current_player, gui_move_history, gui_num_half_moves, &
            gui_search_depth, ai_time_budget_seconds)

        IF (active_book%valid .AND. opening_book_allowed(gui_board%current_player, gui_num_half_moves, gui_book_max_moves_per_side)) THEN
            book_move_found = choose_book_move(active_book, gui_move_history, gui_num_half_moves, gui_board, &
                legal_moves, num_legal_moves, chosen_move, book_move_text)
        END IF

        IF (book_move_found) THEN
            move_found = .TRUE.
        ELSE
            IF (active_book%valid) CALL log_opening_book_miss(active_book%filename)
            CALL find_best_move(gui_board, gui_search_depth, move_found, chosen_move, &
                time_limit_seconds=ai_time_budget_seconds)
        END IF

        CALL SYSTEM_CLOCK(turn_end_count)
        spent_ms = elapsed_milliseconds(turn_start_count, turn_end_count, gui_count_rate)
        IF (book_move_found) THEN
            clock_spent_ms = 0_8
        ELSE
            clock_spent_ms = spent_ms
        END IF

        IF (.NOT. move_found) THEN
            CALL finish_ai_turn_debug_log('No move found.')
            CALL log_gui_failure('process_ai_turn', 'no move found')
            gui_autoplay_mode = .FALSE.
            CALL finish_game(main_hwnd, 'The computer could not find a move.')
            RETURN
        END IF

        IF (gui_num_half_moves + 1 <= SIZE(gui_move_history)) THEN
            gui_white_time_before_move(gui_num_half_moves + 1) = gui_white_time_ms
            gui_black_time_before_move(gui_num_half_moves + 1) = gui_black_time_ms
        END IF

        IF (gui_time_control_enabled .AND. .NOT. book_move_found) THEN
            IF (.NOT. apply_clock_after_move(gui_board%current_player, clock_spent_ms, gui_white_time_ms, gui_black_time_ms, gui_increment_ms)) THEN
                gui_game_lost_on_time = .TRUE.
                gui_time_forfeit_winner_color = get_opponent_color(gui_board%current_player)
                CALL finish_ai_turn_debug_log('Computer lost on time before completing the move.')
                CALL log_gui_failure('process_ai_turn', 'computer flagged on time')
                gui_autoplay_mode = .FALSE.
                CALL finish_game(main_hwnd, 'Computer lost on time.')
                RETURN
            END IF
        END IF

        san_text = move_to_san(gui_board, chosen_move, legal_moves, num_legal_moves)
        CALL log_gui_value('process_ai_turn san', san_text)
        IF (book_move_found) THEN
            CALL log_opening_book_hit(active_book%filename, book_move_text, san_text, move_to_coordinate(chosen_move))
        END IF
        CALL append_move_to_history(san_text, spent_ms)
        CALL make_move(gui_board, chosen_move, move_info)
        gui_played_moves(gui_num_half_moves) = chosen_move
        gui_unmake_history(gui_num_half_moves) = move_info
        gui_position_key_history(gui_num_half_moves + 1) = gui_board%zobrist_key
        IF (book_move_found) THEN
            CALL finish_ai_turn_debug_log('Opening-book move played.')
        ELSE
            CALL finish_ai_turn_debug_log('Search move played.')
        END IF
        played_move_text = san_text
        CALL set_status('Computer played ' // TRIM(san_text) // '.')
        CALL log_gui_checkpoint('process_ai_turn', 'completed')
    END SUBROUTINE process_ai_turn

    SUBROUTINE handle_post_move(main_hwnd, human_moved, move_text)
        TYPE(c_ptr), VALUE :: main_hwnd
        LOGICAL, INTENT(IN) :: human_moved
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        LOGICAL :: last_move_was_human, automatic_turn_pending
        CHARACTER(LEN=32) :: last_move_text

        CALL log_gui_checkpoint('handle_post_move', 'entered')
        CALL log_gui_value('handle_post_move move', move_text)
        last_move_was_human = human_moved
        last_move_text = move_text

        DO
            IF (is_game_over(gui_board, gui_game_winner_color, gui_current_game_status, &
                gui_position_key_history, gui_num_half_moves + 1)) THEN
                IF (gui_current_game_status == GAME_CHECKMATE) THEN
                    IF (gui_game_winner_color == WHITE) THEN
                        CALL finish_game(main_hwnd, 'Checkmate. White wins.')
                    ELSE
                        CALL finish_game(main_hwnd, 'Checkmate. Black wins.')
                    END IF
                ELSE IF (gui_current_game_status == GAME_THREEFOLD_REPETITION) THEN
                    CALL finish_game(main_hwnd, 'Draw by threefold repetition.', '1/2-1/2')
                ELSE
                    CALL finish_game(main_hwnd, 'Stalemate.')
                END IF
                RETURN
            END IF

            CALL SYSTEM_CLOCK(gui_turn_start_count)
            IF (gui_autoplay_mode) THEN
                CALL set_status('Autoplay running...')
            ELSE IF (gui_board%current_player == gui_human_color) THEN
                IF (.NOT. last_move_was_human) THEN
                    CALL set_status('Computer played ' // TRIM(last_move_text) // '. Your move.')
                ELSE
                    CALL set_status('Your move.')
                END IF
            ELSE
                CALL set_status('Computer is thinking...')
            END IF
            CALL refresh_gui_labels()
            CALL invalidate_main_window()

            automatic_turn_pending = (gui_autoplay_mode .OR. gui_board%current_player == gui_ai_color)
            IF (.NOT. automatic_turn_pending) EXIT

            CALL force_window_refresh(main_hwnd)
            CALL process_ai_turn(main_hwnd, last_move_text)
            last_move_was_human = .FALSE.
            IF (gui_game_over) EXIT
        END DO

        CALL log_gui_checkpoint('handle_post_move', 'completed')
    END SUBROUTINE handle_post_move

    SUBROUTINE suggest_gui_move(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: suggested_move
        INTEGER :: num_legal_moves
        LOGICAL :: suggestion_found
        CHARACTER(LEN=32) :: suggestion_san, suggestion_source

        CALL log_gui_checkpoint('suggest_gui_move', 'entered')
        IF (.NOT. gui_game_started) THEN
            CALL set_status('Press Start first.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'game not started')
            RETURN
        END IF
        IF (gui_game_over) THEN
            CALL set_status('Game is over. Press Start to begin again.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'game already over')
            RETURN
        END IF
        IF (gui_autoplay_mode) THEN
            CALL set_status('Autoplay is running.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'autoplay running')
            RETURN
        END IF
        IF (gui_board%current_player /= gui_human_color) THEN
            CALL set_status('Wait for the computer to move.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'not human turn')
            RETURN
        END IF

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        IF (num_legal_moves <= 0) THEN
            CALL set_status('No legal moves available.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'no legal moves')
            RETURN
        END IF

        CALL suggest_move_for_position(gui_board, legal_moves, num_legal_moves, gui_move_history, gui_num_half_moves, &
            gui_white_book, gui_black_book, gui_search_depth, suggestion_found, suggested_move, suggestion_san, &
            suggestion_source, opening_book_allowed(gui_board%current_player, gui_num_half_moves, gui_book_max_moves_per_side))
        IF (.NOT. suggestion_found) THEN
            CALL set_status('No suggestion available.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'no suggestion found')
            RETURN
        END IF

        CALL set_control_text(hwnd_move_edit, TRIM(suggestion_san))
        CALL set_status('Suggestion: ' // TRIM(suggestion_san) // ' (' // TRIM(suggestion_source) // ').')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_value('suggest_gui_move san', suggestion_san)
        CALL log_gui_checkpoint('suggest_gui_move', 'completed')
    END SUBROUTINE suggest_gui_move

    SUBROUTINE autoplay_gui_game(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('autoplay_gui_game', 'entered')
        IF (.NOT. gui_game_started) THEN
            CALL set_status('Press Start first.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('autoplay_gui_game', 'game not started')
            RETURN
        END IF
        IF (gui_game_over) THEN
            CALL set_status('Game is over. Press Start to begin again.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('autoplay_gui_game', 'game already over')
            RETURN
        END IF

        gui_autoplay_mode = .TRUE.
        CALL set_status('Autoplay running...')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL handle_post_move(main_hwnd, .FALSE., '')
        CALL log_gui_checkpoint('autoplay_gui_game', 'completed')
    END SUBROUTINE autoplay_gui_game

    SUBROUTINE takeback_gui_move(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER :: undo_count, undone_count

        CALL log_gui_checkpoint('takeback_gui_move', 'entered')
        IF (.NOT. gui_game_started) THEN
            CALL set_status('Press Start first.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('takeback_gui_move', 'game not started')
            RETURN
        END IF
        IF (gui_num_half_moves <= 0) THEN
            CALL set_status('No moves available to take back.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('takeback_gui_move', 'no move history')
            RETURN
        END IF

        gui_autoplay_mode = .FALSE.
        undo_count = MIN(2, gui_num_half_moves)
        undone_count = 0
        DO WHILE (undo_count > 0)
            CALL unmake_move(gui_board, gui_played_moves(gui_num_half_moves), gui_unmake_history(gui_num_half_moves))
            gui_white_time_ms = gui_white_time_before_move(gui_num_half_moves)
            gui_black_time_ms = gui_black_time_before_move(gui_num_half_moves)
            gui_move_history(gui_num_half_moves) = ''
            gui_move_elapsed_ms_history(gui_num_half_moves) = -1_8
            gui_num_half_moves = gui_num_half_moves - 1
            undo_count = undo_count - 1
            undone_count = undone_count + 1
        END DO

        gui_game_over = .FALSE.
        gui_game_lost_on_time = .FALSE.
        gui_game_winner_color = NO_COLOR
        gui_current_game_status = GAME_ONGOING
        gui_time_forfeit_winner_color = NO_COLOR
        gui_pgn_result = '*'
        CALL enable_game_controls(.TRUE.)
        CALL set_status('Took back ' // TRIM(int_to_text(undone_count)) // ' move(s).')
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_checkpoint('takeback_gui_move', 'completed')
    END SUBROUTINE takeback_gui_move

    SUBROUTINE offer_draw(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('offer_draw', 'entered')
        IF (.NOT. gui_game_started .OR. gui_game_over) THEN
            CALL set_status('No active game.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('offer_draw', 'no active game')
            RETURN
        END IF

        IF (ABS(current_eval_white_cp()) <= DRAW_ACCEPTANCE_CP) THEN
            CALL log_gui_event('Draw accepted by evaluation rule')
            CALL finish_game(main_hwnd, 'Draw agreed.', '1/2-1/2')
        ELSE
            CALL set_status('Draw offer declined.')
            CALL refresh_gui_labels()
            CALL log_gui_event('Draw declined by evaluation rule')
        END IF
    END SUBROUTINE offer_draw

    SUBROUTINE resign_game(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('resign_game', 'entered')
        IF (.NOT. gui_game_started .OR. gui_game_over) THEN
            CALL set_status('No active game.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('resign_game', 'no active game')
            RETURN
        END IF

        IF (gui_human_color == WHITE) THEN
            CALL finish_game(main_hwnd, 'White resigned. Black wins.', '0-1')
        ELSE
            CALL finish_game(main_hwnd, 'Black resigned. White wins.', '1-0')
        END IF
    END SUBROUTINE resign_game

    SUBROUTINE finish_game(main_hwnd, final_status, result_override)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=*), INTENT(IN) :: final_status
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: result_override

        CALL log_gui_checkpoint('finish_game', 'entered')
        CALL log_gui_value('finish_game status', final_status)
        gui_game_over = .TRUE.
        gui_autoplay_mode = .FALSE.
        CALL enable_game_controls(.FALSE.)

        IF (PRESENT(result_override)) THEN
            gui_pgn_result = result_override
        ELSE IF (gui_game_lost_on_time) THEN
            IF (gui_time_forfeit_winner_color == WHITE) THEN
                gui_pgn_result = '1-0'
            ELSE
                gui_pgn_result = '0-1'
            END IF
        ELSE
            SELECT CASE (gui_current_game_status)
            CASE (GAME_CHECKMATE)
                IF (gui_game_winner_color == WHITE) THEN
                    gui_pgn_result = '1-0'
                ELSE
                    gui_pgn_result = '0-1'
                END IF
            CASE (GAME_STALEMATE)
                gui_pgn_result = '1/2-1/2'
            CASE (GAME_THREEFOLD_REPETITION)
                gui_pgn_result = '1/2-1/2'
            CASE DEFAULT
                gui_pgn_result = '*'
            END SELECT
        END IF

        IF (gui_num_half_moves > 0) THEN
            CALL write_pgn_file('games.pgn', gui_move_history, gui_num_half_moves, gui_pgn_result, &
                gui_move_elapsed_ms_history, gui_pgn_time_control_tag)
        END IF
        CALL set_status(TRIM(final_status))
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_checkpoint('finish_game', 'completed')
    END SUBROUTINE finish_game

    SUBROUTINE on_gui_timer(main_hwnd, timer_id)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER(c_intptr_t), VALUE :: timer_id
        INTEGER(KIND=8) :: now_count, elapsed_now, remaining_now

        CALL log_gui_checkpoint('on_gui_timer', 'entered')
        IF (timer_id /= TIMER_CLOCK) RETURN
        IF (gui_time_control_enabled .AND. gui_game_started .AND. .NOT. gui_game_over .AND. gui_board%current_player == gui_human_color) THEN
            CALL SYSTEM_CLOCK(now_count)
            elapsed_now = elapsed_milliseconds(gui_turn_start_count, now_count, gui_count_rate)
            IF (gui_board%current_player == WHITE) THEN
                remaining_now = gui_white_time_ms - elapsed_now
                IF (remaining_now <= 0_8) THEN
                    gui_white_time_ms = 0
                    gui_game_lost_on_time = .TRUE.
                    gui_time_forfeit_winner_color = BLACK
                    CALL log_gui_failure('on_gui_timer', 'white human flagged on time')
                    CALL finish_game(main_hwnd, 'You lost on time.')
                    RETURN
                END IF
            ELSE
                remaining_now = gui_black_time_ms - elapsed_now
                IF (remaining_now <= 0_8) THEN
                    gui_black_time_ms = 0
                    gui_game_lost_on_time = .TRUE.
                    gui_time_forfeit_winner_color = WHITE
                    CALL log_gui_failure('on_gui_timer', 'black human flagged on time')
                    CALL finish_game(main_hwnd, 'You lost on time.')
                    RETURN
                END IF
            END IF
        END IF
        CALL refresh_gui_labels()
    END SUBROUTINE on_gui_timer

    SUBROUTINE refresh_gui_labels()
        CHARACTER(LEN=64) :: turn_text, white_text, black_text, eval_text
        INTEGER :: eval_white_cp

        IF (.NOT. gui_game_started) THEN
            turn_text = 'Game not started'
        ELSE IF (gui_game_over) THEN
            turn_text = 'Game over'
        ELSE IF (gui_board%current_player == WHITE) THEN
            turn_text = 'Turn: White'
        ELSE
            turn_text = 'Turn: Black'
        END IF

        IF (gui_time_control_enabled) THEN
            white_text = 'White: ' // TRIM(format_clock(display_clock_value(WHITE)))
            black_text = 'Black: ' // TRIM(format_clock(display_clock_value(BLACK)))
        ELSE
            white_text = 'White: Off'
            black_text = 'Black: Off'
        END IF

        eval_white_cp = current_eval_white_cp()
        WRITE(eval_text, '(A,F7.2)') 'Eval (White): ', REAL(eval_white_cp) / 100.0

        CALL set_control_text(hwnd_turn_static, TRIM(turn_text))
        CALL set_control_text(hwnd_white_clock, TRIM(white_text))
        CALL set_control_text(hwnd_black_clock, TRIM(black_text))
        CALL set_control_text(hwnd_eval_static, TRIM(eval_text))
        CALL set_control_text(hwnd_status_static, TRIM(gui_status_text))
    END SUBROUTINE refresh_gui_labels

    INTEGER(KIND=8) FUNCTION display_clock_value(player_color) RESULT(clock_value)
        INTEGER, INTENT(IN) :: player_color
        INTEGER(KIND=8) :: now_count, elapsed_now

        IF (player_color == WHITE) THEN
            clock_value = gui_white_time_ms
        ELSE
            clock_value = gui_black_time_ms
        END IF
        IF (.NOT. gui_time_control_enabled) RETURN
        IF (.NOT. gui_game_started .OR. gui_game_over) RETURN
        IF (gui_board%current_player == player_color) THEN
            CALL SYSTEM_CLOCK(now_count)
            elapsed_now = elapsed_milliseconds(gui_turn_start_count, now_count, gui_count_rate)
            clock_value = MAX(0_8, clock_value - elapsed_now)
        END IF
    END FUNCTION display_clock_value

    INTEGER FUNCTION current_eval_white_cp() RESULT(eval_white_cp)
        INTEGER :: eval_cp

        IF (.NOT. gui_game_started) THEN
            eval_white_cp = 0
            RETURN
        END IF
        eval_cp = evaluate_board(gui_board)
        IF (gui_board%current_player == BLACK) THEN
            eval_white_cp = -eval_cp
        ELSE
            eval_white_cp = eval_cp
        END IF
    END FUNCTION current_eval_white_cp

    SUBROUTINE enable_game_controls(enable_flag)
        LOGICAL, INTENT(IN) :: enable_flag
        INTEGER(c_int) :: ignored_int, enabled_int

        IF (enable_flag) THEN
            enabled_int = 1_c_int
        ELSE
            enabled_int = 0_c_int
        END IF
        IF (C_ASSOCIATED(hwnd_move_edit)) ignored_int = EnableWindow(hwnd_move_edit, enabled_int)
        IF (C_ASSOCIATED(hwnd_submit_button)) ignored_int = EnableWindow(hwnd_submit_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_suggest_button)) ignored_int = EnableWindow(hwnd_suggest_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_autoplay_button)) ignored_int = EnableWindow(hwnd_autoplay_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_takeback_button)) ignored_int = EnableWindow(hwnd_takeback_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_draw_button)) ignored_int = EnableWindow(hwnd_draw_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_resign_button)) ignored_int = EnableWindow(hwnd_resign_button, enabled_int)
    END SUBROUTINE enable_game_controls

    SUBROUTINE append_move_to_history(move_text, elapsed_ms)
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        INTEGER(KIND=8), INTENT(IN), OPTIONAL :: elapsed_ms

        IF (gui_num_half_moves < SIZE(gui_move_history)) THEN
            gui_num_half_moves = gui_num_half_moves + 1
            gui_move_history(gui_num_half_moves) = TRIM(move_text)
            IF (PRESENT(elapsed_ms)) gui_move_elapsed_ms_history(gui_num_half_moves) = elapsed_ms
        END IF
    END SUBROUTINE append_move_to_history

    SUBROUTINE set_status(text)
        CHARACTER(LEN=*), INTENT(IN) :: text

        gui_status_text = ''
        gui_status_text = text
        CALL log_gui_value('status', text)
    END SUBROUTINE set_status

    FUNCTION format_pgn_time_control_tag(initial_ms, increment_ms_value) RESULT(tag)
        INTEGER(KIND=8), INTENT(IN) :: initial_ms, increment_ms_value
        CHARACTER(LEN=32) :: tag
        INTEGER(KIND=8) :: initial_seconds, increment_seconds

        initial_seconds = MAX(0_8, initial_ms / 1000_8)
        increment_seconds = MAX(0_8, increment_ms_value / 1000_8)
        WRITE(tag, '(I0,A,I0)') initial_seconds, '+', increment_seconds
    END FUNCTION format_pgn_time_control_tag

    SUBROUTINE set_control_text(hwnd, text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(256), TARGET :: text_buffer
        INTEGER(c_int) :: ignored_int

        IF (.NOT. C_ASSOCIATED(hwnd)) RETURN
        CALL set_c_string(text, text_buffer)
        ignored_int = SetWindowTextA(hwnd, c_loc(text_buffer(1)))
    END SUBROUTINE set_control_text

    FUNCTION get_control_text(hwnd) RESULT(text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=MAX_GUI_TEXT) :: text
        CHARACTER(KIND=c_char), DIMENSION(MAX_GUI_TEXT), TARGET :: text_buffer
        INTEGER(c_int) :: copied_len
        INTEGER :: i

        text = ''
        IF (.NOT. C_ASSOCIATED(hwnd)) RETURN
        text_buffer = c_null_char
        copied_len = GetWindowTextA(hwnd, c_loc(text_buffer(1)), MAX_GUI_TEXT - 1)
        IF (copied_len <= 0) RETURN
        DO i = 1, MIN(INT(copied_len), LEN(text))
            text(i:i) = ACHAR(IACHAR(text_buffer(i)), KIND=kind(text))
        END DO
    END FUNCTION get_control_text

    SUBROUTINE invalidate_main_window()
        INTEGER(c_int) :: ignored_int

        IF (C_ASSOCIATED(gui_main_hwnd)) ignored_int = InvalidateRect(gui_main_hwnd, c_null_ptr, 1_c_int)
    END SUBROUTINE invalidate_main_window

    SUBROUTINE force_window_refresh(hwnd)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int) :: ignored_int

        ignored_int = InvalidateRect(hwnd, c_null_ptr, 1_c_int)
        ignored_int = UpdateWindow(hwnd)
    END SUBROUTINE force_window_refresh

    CHARACTER(LEN=1) FUNCTION board_piece_char(board, rank, file)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: rank, file

        SELECT CASE (board%squares_piece(rank, file))
        CASE (PAWN)
            board_piece_char = 'P'
        CASE (KNIGHT)
            board_piece_char = 'N'
        CASE (BISHOP)
            board_piece_char = 'B'
        CASE (ROOK)
            board_piece_char = 'R'
        CASE (QUEEN)
            board_piece_char = 'Q'
        CASE (KING)
            board_piece_char = 'K'
        CASE DEFAULT
            board_piece_char = ' '
            RETURN
        END SELECT

        IF (board%squares_color(rank, file) == BLACK) board_piece_char = ACHAR(IACHAR(board_piece_char) + 32)
    END FUNCTION board_piece_char

    SUBROUTINE draw_text(device_context, x_pos, y_pos, text)
        TYPE(c_ptr), VALUE :: device_context
        INTEGER(c_int), VALUE :: x_pos, y_pos
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(256), TARGET :: text_buffer
        INTEGER(c_int) :: text_len, ignored_int

        CALL set_c_string(text, text_buffer)
        text_len = LEN_TRIM(text)
        ignored_int = TextOutA(device_context, x_pos, y_pos, c_loc(text_buffer(1)), text_len)
    END SUBROUTINE draw_text

    INTEGER(c_long) FUNCTION rgb_color(red, green, blue)
        INTEGER, INTENT(IN) :: red, green, blue

        rgb_color = INT(red + 256 * green + 65536 * blue, c_long)
    END FUNCTION rgb_color

    TYPE(c_ptr) FUNCTION ptr_from_intptr(raw_value) RESULT(pointer_value)
        INTEGER(c_intptr_t), INTENT(IN) :: raw_value
        TYPE(c_ptr) :: template_ptr

        template_ptr = c_null_ptr
        pointer_value = TRANSFER(raw_value, template_ptr)
    END FUNCTION ptr_from_intptr

    INTEGER(c_intptr_t) FUNCTION funptr_to_intptr(raw_value) RESULT(pointer_value)
        TYPE(c_funptr), INTENT(IN) :: raw_value
        INTEGER(c_intptr_t) :: template_value

        template_value = 0_c_intptr_t
        pointer_value = TRANSFER(raw_value, template_value)
    END FUNCTION funptr_to_intptr

    INTEGER(c_intptr_t) FUNCTION transfer_ptr(raw_ptr) RESULT(pointer_value)
        TYPE(c_ptr), INTENT(IN) :: raw_ptr
        INTEGER(c_intptr_t) :: template_value

        template_value = 0_c_intptr_t
        pointer_value = TRANSFER(raw_ptr, template_value)
    END FUNCTION transfer_ptr

    FUNCTION int_to_text(value) RESULT(text)
        INTEGER, INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION int_to_text

    FUNCTION int64_to_text(value) RESULT(text)
        INTEGER(KIND=8), INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION int64_to_text

    FUNCTION intptr_to_text(value) RESULT(text)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION intptr_to_text

    FUNCTION real_to_text(value) RESULT(text)
        REAL, INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(F10.3)') value
        text = ADJUSTL(text)
    END FUNCTION real_to_text

    FUNCTION logical_to_text(flag) RESULT(text)
        LOGICAL, INTENT(IN) :: flag
        CHARACTER(LEN=5) :: text

        IF (flag) THEN
            text = 'true'
        ELSE
            text = 'false'
        END IF
    END FUNCTION logical_to_text

    LOGICAL FUNCTION parse_nonnegative_integer(text, value_out) RESULT(parsed_ok)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER, INTENT(OUT) :: value_out
        INTEGER :: ios

        value_out = -1
        READ(text, *, IOSTAT=ios) value_out
        parsed_ok = (ios == 0 .AND. value_out >= 0)
    END FUNCTION parse_nonnegative_integer

    LOGICAL FUNCTION opening_book_allowed(player_color, half_moves_played, move_limit_per_side) RESULT(allowed)
        INTEGER, INTENT(IN) :: player_color, half_moves_played, move_limit_per_side
        INTEGER :: side_moves_played

        IF (move_limit_per_side < 0) THEN
            allowed = .TRUE.
            RETURN
        END IF

        IF (player_color == WHITE) THEN
            side_moves_played = (half_moves_played + 1) / 2
        ELSE
            side_moves_played = half_moves_played / 2
        END IF
        allowed = (side_moves_played < move_limit_per_side)
    END FUNCTION opening_book_allowed

    LOGICAL FUNCTION validate_gui_opening_books(main_hwnd) RESULT(ok)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('validate_gui_opening_books', 'entered')
        ok = .FALSE.
        CALL load_opening_book('book_white.txt', gui_white_book)
        IF (gui_white_book%found .AND. .NOT. gui_white_book%valid) THEN
            CALL log_gui_failure('validate_gui_opening_books', 'white book invalid')
            CALL show_book_error(main_hwnd, 'White', gui_white_book)
            RETURN
        END IF

        CALL load_opening_book('book_black.txt', gui_black_book)
        IF (gui_black_book%found .AND. .NOT. gui_black_book%valid) THEN
            CALL log_gui_failure('validate_gui_opening_books', 'black book invalid')
            CALL show_book_error(main_hwnd, 'Black', gui_black_book)
            RETURN
        END IF

        ok = .TRUE.
        CALL log_gui_checkpoint('validate_gui_opening_books', 'completed')
    END FUNCTION validate_gui_opening_books

    SUBROUTINE show_book_error(main_hwnd, side_name, book)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=*), INTENT(IN) :: side_name
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        CHARACTER(LEN=512) :: message_text
        CHARACTER(LEN=128) :: title_text

        message_text = TRIM(side_name) // ' opening book is invalid.'
        IF (book%error_line > 0) THEN
            WRITE(message_text, '(A,A,I0)') TRIM(side_name) // ' opening book is invalid. Line ', '', book%error_line
        END IF
        IF (LEN_TRIM(book%error_text) > 0) message_text = TRIM(message_text) // CHAR(10) // 'Bad line: ' // TRIM(book%error_text)
        IF (LEN_TRIM(book%error_message) > 0) message_text = TRIM(message_text) // CHAR(10) // TRIM(book%error_message)
        CALL log_gui_failure('show_book_error', message_text)
        CALL set_status(TRIM(side_name) // ' opening book is invalid.')
        CALL refresh_gui_labels()
        title_text = 'Opening Book Error'
        CALL show_message_box(main_hwnd, TRIM(message_text), TRIM(title_text), MB_OK + MB_ICONERROR)
    END SUBROUTINE show_book_error

    SUBROUTINE show_message_box(hwnd, text, title, box_type)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text, title
        INTEGER(c_int), INTENT(IN) :: box_type
        CHARACTER(KIND=c_char), DIMENSION(512), TARGET :: text_buffer
        CHARACTER(KIND=c_char), DIMENSION(128), TARGET :: title_buffer
        INTEGER(c_int) :: ignored_int

        CALL log_gui_event('Message box [' // TRIM(title) // ']: ' // TRIM(text))
        CALL set_c_string(text, text_buffer)
        CALL set_c_string(title, title_buffer)
        ignored_int = MessageBoxA(hwnd, c_loc(text_buffer(1)), c_loc(title_buffer(1)), box_type)
    END SUBROUTINE show_message_box

    FUNCTION sanitize_move_text(text_in) RESULT(clean_text)
        CHARACTER(LEN=*), INTENT(IN) :: text_in
        CHARACTER(LEN=MAX_GUI_TEXT) :: clean_text
        INTEGER :: i, out_pos

        clean_text = ''
        out_pos = 0
        DO i = 1, LEN_TRIM(text_in)
            IF (text_in(i:i) == CHAR(10) .OR. text_in(i:i) == CHAR(13)) CYCLE
            out_pos = out_pos + 1
            IF (out_pos <= LEN(clean_text)) clean_text(out_pos:out_pos) = text_in(i:i)
        END DO
        clean_text = ADJUSTL(clean_text)
    END FUNCTION sanitize_move_text

    SUBROUTINE set_c_string(text, buffer)
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(:), INTENT(OUT), TARGET :: buffer
        INTEGER :: i, usable_len

        buffer = c_null_char
        usable_len = MIN(LEN_TRIM(text), SIZE(buffer) - 1)
        DO i = 1, usable_len
            buffer(i) = ACHAR(IACHAR(text(i:i)), KIND=c_char)
        END DO
        buffer(usable_len + 1) = c_null_char
    END SUBROUTINE set_c_string

    SUBROUTINE set_wide_string(text, buffer)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER(c_int16_t), DIMENSION(:), INTENT(OUT), TARGET :: buffer
        INTEGER :: i, usable_len

        buffer = 0_c_int16_t
        usable_len = MIN(LEN_TRIM(text), SIZE(buffer) - 1)
        DO i = 1, usable_len
            buffer(i) = INT(IACHAR(text(i:i)), c_int16_t)
        END DO
        buffer(usable_len + 1) = 0_c_int16_t
    END SUBROUTINE set_wide_string

END MODULE Chess_GUI_App

PROGRAM Fortran_Chess_GUI
    USE Chess_GUI_App, ONLY: run_chess_gui
    IMPLICIT NONE

    CALL run_chess_gui()
END PROGRAM Fortran_Chess_GUI
