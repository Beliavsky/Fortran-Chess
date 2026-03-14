MODULE Chess_Click_GUI_App
    USE, INTRINSIC :: iso_c_binding
    USE App_Defaults, ONLY: DEFAULT_SEARCH_DEPTH
    USE Chess_Types
    USE Board_Utils, ONLY: init_board, get_opponent_color, set_board_from_fen, board_to_fen
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
    USE Search, ONLY: find_best_move, display_eval_white_cp
    USE Transposition_Table, ONLY: init_zobrist_keys
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_chess_click_gui

    INTEGER(c_int), PARAMETER :: WM_CREATE = 1_c_int
    INTEGER(c_int), PARAMETER :: WM_DESTROY = 2_c_int
    INTEGER(c_int), PARAMETER :: WM_PAINT = 15_c_int
    INTEGER(c_int), PARAMETER :: WM_CLOSE = 16_c_int
    INTEGER(c_int), PARAMETER :: WM_COMMAND = 273_c_int
    INTEGER(c_int), PARAMETER :: WM_TIMER = 275_c_int
    INTEGER(c_int), PARAMETER :: WM_KEYDOWN = 256_c_int
    INTEGER(c_int), PARAMETER :: WM_LBUTTONDOWN = 513_c_int
    INTEGER(c_int), PARAMETER :: SW_SHOWNORMAL = 1_c_int
    INTEGER(c_int), PARAMETER :: SW_HIDE = 0_c_int
    INTEGER(c_int), PARAMETER :: CW_USEDEFAULT = -2147483647_c_int - 1_c_int
    INTEGER(c_int), PARAMETER :: TRANSPARENT = 1_c_int
    INTEGER(c_intptr_t), PARAMETER :: TIMER_CLOCK = 1_c_intptr_t
    INTEGER(c_int), PARAMETER :: GDI_PLUS_OK = 0_c_int
    INTEGER(c_long), PARAMETER :: WS_OVERLAPPEDWINDOW = INT(Z'00CF0000', c_long)
    INTEGER(c_long), PARAMETER :: WS_CHILD = INT(Z'40000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_VISIBLE = INT(Z'10000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_TABSTOP = INT(Z'00010000', c_long)
    INTEGER(c_long), PARAMETER :: WS_BORDER = INT(Z'00800000', c_long)
    INTEGER(c_long), PARAMETER :: WS_VSCROLL = INT(Z'00200000', c_long)
    INTEGER(c_long), PARAMETER :: CBS_DROPDOWNLIST = INT(Z'00000003', c_long)
    INTEGER(c_long), PARAMETER :: ES_AUTOHSCROLL = INT(Z'00000080', c_long)
    INTEGER(c_long), PARAMETER :: ES_MULTILINE = INT(Z'00000004', c_long)
    INTEGER(c_long), PARAMETER :: ES_AUTOVSCROLL = INT(Z'00000040', c_long)
    INTEGER(c_long), PARAMETER :: ES_READONLY = INT(Z'00000800', c_long)
    INTEGER(c_long), PARAMETER :: WS_EX_CLIENTEDGE = INT(Z'00000200', c_long)
    INTEGER(c_int), PARAMETER :: MB_OK = 0_c_int
    INTEGER(c_int), PARAMETER :: MB_YESNO = 4_c_int
    INTEGER(c_int), PARAMETER :: MB_ICONERROR = 16_c_int
    INTEGER(c_int), PARAMETER :: MB_ICONQUESTION = 32_c_int
    INTEGER(c_int), PARAMETER :: IDYES = 6_c_int
    INTEGER(c_int), PARAMETER :: VK_RETURN = 13_c_int
    INTEGER(c_int), PARAMETER :: GWL_WNDPROC = -4_c_int
    INTEGER(c_int), PARAMETER :: CB_ADDSTRING = 323_c_int
    INTEGER(c_int), PARAMETER :: CB_SETCURSEL = 334_c_int
    INTEGER(c_int), PARAMETER :: EM_SETSEL = 177_c_int
    INTEGER(c_int), PARAMETER :: EM_SCROLLCARET = 183_c_int
    INTEGER(c_int), PARAMETER :: PS_SOLID = 0_c_int
    INTEGER(c_int), PARAMETER :: CF_TEXT = 1_c_int
    INTEGER(c_int), PARAMETER :: GMEM_MOVEABLE = 2_c_int
    INTEGER, PARAMETER :: DRAW_ACCEPTANCE_CP = 50
    INTEGER, PARAMETER :: MOVE_BLUNDER_CP = 250
    INTEGER, PARAMETER :: MOVE_MISTAKE_CP = 120
    INTEGER, PARAMETER :: MAX_GUI_TEXT = 256
    INTEGER(c_int), PARAMETER :: WINDOW_WIDTH = 1040_c_int
    INTEGER(c_int), PARAMETER :: WINDOW_HEIGHT = 760_c_int
    INTEGER(c_int), PARAMETER :: GRAPH_WINDOW_WIDTH = 860_c_int
    INTEGER(c_int), PARAMETER :: GRAPH_WINDOW_HEIGHT = 760_c_int
    INTEGER(c_int), PARAMETER :: BOARD_LEFT = 24_c_int
    INTEGER(c_int), PARAMETER :: BOARD_TOP = 84_c_int
    INTEGER(c_int), PARAMETER :: BOARD_PIXELS = 512_c_int
    INTEGER(c_int), PARAMETER :: SQUARE_PIXELS = BOARD_PIXELS / 8_c_int
    INTEGER(c_int), PARAMETER :: PANEL_LEFT = 580_c_int
    REAL, PARAMETER :: ANALYSIS_SUGGESTION_TIME_LIMIT_SECONDS = 1.0
    INTEGER, PARAMETER :: MAX_CONTROL_TEXT = 4096

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
    TYPE(Board_Type), SAVE :: gui_replay_board
    TYPE(c_ptr), SAVE :: gui_main_hwnd = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_color_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_mode_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_time_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_depth_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_book_limit_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_move_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_start_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_help_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_flip_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_review_start_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_review_back_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_review_forward_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_review_end_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_graph_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_submit_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_auto_submit_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_suggest_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_autoplay_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_takeback_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_switch_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_status_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_white_clock = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_black_clock = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_turn_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_eval_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_eval_toggle_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_setup_help = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_draw_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_resign_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_pause_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_pgn_toggle_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_pgn_eval_toggle_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_pgn_time_toggle_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_fan_toggle_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_pgn_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_fen_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_load_fen_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_copy_fen_button = c_null_ptr
    TYPE(c_ptr), SAVE :: piece_images(2, 6)
    TYPE(c_ptr), SAVE :: gui_instance = c_null_ptr
    TYPE(c_ptr), SAVE :: gui_graph_hwnd = c_null_ptr
    INTEGER(c_size_t), SAVE :: gdiplus_token = 0_c_size_t
    LOGICAL, SAVE :: gdiplus_started = .FALSE.
    LOGICAL, SAVE :: piece_images_loaded = .FALSE.
    LOGICAL, SAVE :: gui_game_started = .FALSE.
    LOGICAL, SAVE :: gui_game_over = .FALSE.
    LOGICAL, SAVE :: gui_time_control_enabled = .FALSE.
    LOGICAL, SAVE :: gui_game_lost_on_time = .FALSE.
    LOGICAL, SAVE :: gui_autoplay_mode = .FALSE.
    LOGICAL, SAVE :: gui_analysis_mode = .FALSE.
    LOGICAL, SAVE :: gui_paused = .FALSE.
    LOGICAL, SAVE :: gui_auto_submit_moves = .FALSE.
    LOGICAL, SAVE :: gui_show_eval = .TRUE.
    LOGICAL, SAVE :: gui_show_pgn = .TRUE.
    LOGICAL, SAVE :: gui_show_pgn_eval = .FALSE.
    LOGICAL, SAVE :: gui_show_pgn_time = .FALSE.
    LOGICAL, SAVE :: gui_show_fan = .FALSE.
    LOGICAL, SAVE :: gui_board_flipped = .FALSE.
    LOGICAL, SAVE :: gui_replay_mode = .FALSE.
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
    INTEGER, SAVE :: gui_replay_half_move_index = 0
    CHARACTER(LEN=32), DIMENSION(512), SAVE :: gui_move_history = ''
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_move_elapsed_ms_history = -1_8
    INTEGER, DIMENSION(512), SAVE :: gui_move_eval_cp_history = 0
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_white_time_before_move = 0_8
    INTEGER(KIND=8), DIMENSION(512), SAVE :: gui_black_time_before_move = 0_8
    INTEGER(KIND=8), DIMENSION(513), SAVE :: gui_position_key_history = 0_8
    INTEGER, DIMENSION(512), SAVE :: gui_move_display_start = -1
    INTEGER, DIMENSION(512), SAVE :: gui_move_display_end = -1
    TYPE(Move_Type), DIMENSION(512), SAVE :: gui_played_moves
    TYPE(UnmakeInfo_Type), DIMENSION(512), SAVE :: gui_unmake_history
    CHARACTER(LEN=7), SAVE :: gui_pgn_result = '*'
    CHARACTER(LEN=32), SAVE :: gui_pgn_time_control_tag = '-'
    CHARACTER(LEN=256), SAVE :: gui_status_text = 'Enter color and time control, then press Start.'
    TYPE(Opening_Book_Type), SAVE :: gui_white_book, gui_black_book
    INTEGER(c_intptr_t), SAVE :: original_move_edit_proc = 0_c_intptr_t
    LOGICAL, SAVE :: gui_suppress_move_edit_submit = .FALSE.
    LOGICAL, SAVE :: gui_start_from_fen_pending = .FALSE.
    INTEGER, SAVE :: gui_selected_from_rank = 0
    INTEGER, SAVE :: gui_selected_from_file = 0

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

        FUNCTION DestroyWindow(hwnd) BIND(C, NAME="DestroyWindow")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int) :: DestroyWindow
        END FUNCTION DestroyWindow

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

        FUNCTION CreatePen(style, width, color) BIND(C, NAME="CreatePen")
            IMPORT :: c_int, c_long, c_ptr
            INTEGER(c_int), VALUE :: style, width
            INTEGER(c_long), VALUE :: color
            TYPE(c_ptr) :: CreatePen
        END FUNCTION CreatePen

        FUNCTION SelectObject(hdc, object) BIND(C, NAME="SelectObject")
            IMPORT :: c_ptr
            TYPE(c_ptr), VALUE :: hdc, object
            TYPE(c_ptr) :: SelectObject
        END FUNCTION SelectObject

        FUNCTION MoveToEx(hdc, x, y, old_point) BIND(C, NAME="MoveToEx")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc, old_point
            INTEGER(c_int), VALUE :: x, y
            INTEGER(c_int) :: MoveToEx
        END FUNCTION MoveToEx

        FUNCTION LineTo(hdc, x, y) BIND(C, NAME="LineTo")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: x, y
            INTEGER(c_int) :: LineTo
        END FUNCTION LineTo

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

        FUNCTION SendMessageA(hwnd, msg, wparam, lparam) BIND(C, NAME="SendMessageA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: msg
            INTEGER(c_intptr_t), VALUE :: wparam, lparam
            INTEGER(c_intptr_t) :: SendMessageA
        END FUNCTION SendMessageA

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

        FUNCTION OpenClipboard(hwnd) BIND(C, NAME="OpenClipboard")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int) :: OpenClipboard
        END FUNCTION OpenClipboard

        FUNCTION CloseClipboard() BIND(C, NAME="CloseClipboard")
            IMPORT :: c_int
            INTEGER(c_int) :: CloseClipboard
        END FUNCTION CloseClipboard

        FUNCTION EmptyClipboard() BIND(C, NAME="EmptyClipboard")
            IMPORT :: c_int
            INTEGER(c_int) :: EmptyClipboard
        END FUNCTION EmptyClipboard

        FUNCTION SetClipboardData(format_value, memory_handle) BIND(C, NAME="SetClipboardData")
            IMPORT :: c_int, c_ptr
            INTEGER(c_int), VALUE :: format_value
            TYPE(c_ptr), VALUE :: memory_handle
            TYPE(c_ptr) :: SetClipboardData
        END FUNCTION SetClipboardData

        FUNCTION GlobalAlloc(flags, bytes) BIND(C, NAME="GlobalAlloc")
            IMPORT :: c_int, c_size_t, c_ptr
            INTEGER(c_int), VALUE :: flags
            INTEGER(c_size_t), VALUE :: bytes
            TYPE(c_ptr) :: GlobalAlloc
        END FUNCTION GlobalAlloc

        FUNCTION GlobalLock(memory_handle) BIND(C, NAME="GlobalLock")
            IMPORT :: c_ptr
            TYPE(c_ptr), VALUE :: memory_handle
            TYPE(c_ptr) :: GlobalLock
        END FUNCTION GlobalLock

        FUNCTION GlobalUnlock(memory_handle) BIND(C, NAME="GlobalUnlock")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: memory_handle
            INTEGER(c_int) :: GlobalUnlock
        END FUNCTION GlobalUnlock

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

    RECURSIVE SUBROUTINE run_chess_click_gui()
        TYPE(WNDCLASSEXA) :: window_class
        TYPE(MSG) :: message_data
        TYPE(c_ptr) :: window_handle
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_name
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: graph_class_name
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
        gui_replay_board = gui_board
        CALL log_gui_checkpoint('run_chess_gui', 'board initialized')

        CALL set_c_string('FortranChessGUI', class_name)
        CALL set_c_string('FortranChessEvalGraph', graph_class_name)
        CALL set_c_string('Fortran Chess Click GUI', window_title)

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
        window_class%lpfnWndProc = c_funloc(eval_graph_proc)
        window_class%lpszClassName = c_loc(graph_class_name(1))
        IF (RegisterClassExA(window_class) == 0_c_int) THEN
            CALL log_gui_failure('run_chess_gui', 'RegisterClassExA failed for eval graph window')
            CALL finish_gui_debug_session('RegisterClassExA failed for eval graph window.')
            ERROR STOP 'Could not register the eval graph window class.'
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
    END SUBROUTINE run_chess_click_gui

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
                gui_start_from_fen_pending = .FALSE.
                CALL start_new_game(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_help_button)) THEN
                CALL log_gui_event('Help button pressed')
                CALL show_gui_help(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_flip_button)) THEN
                CALL log_gui_event('Flip button pressed')
                CALL toggle_board_flip(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_review_start_button)) THEN
                CALL log_gui_event('Review start button pressed')
                CALL review_to_start(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_review_back_button)) THEN
                CALL log_gui_event('Review back button pressed')
                CALL review_step(hwnd, -1)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_review_forward_button)) THEN
                CALL log_gui_event('Review forward button pressed')
                CALL review_step(hwnd, 1)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_review_end_button)) THEN
                CALL log_gui_event('Review end button pressed')
                CALL review_to_end(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_graph_button)) THEN
                CALL log_gui_event('Graph button pressed')
                CALL show_eval_graph_window()
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_load_fen_button)) THEN
                CALL log_gui_event('Load FEN button pressed')
                gui_start_from_fen_pending = .TRUE.
                CALL start_new_game(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_copy_fen_button)) THEN
                CALL log_gui_event('Copy FEN button pressed')
                CALL copy_current_fen(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_submit_button)) THEN
                CALL log_gui_event('Play Move button pressed')
                CALL submit_human_move(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_auto_submit_button)) THEN
                CALL log_gui_event('Auto submit button pressed')
                CALL toggle_auto_submit(hwnd)
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
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_pause_button)) THEN
                CALL log_gui_event('Pause button pressed')
                CALL toggle_pause(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_switch_button)) THEN
                CALL log_gui_event('Switch Sides button pressed')
                CALL switch_gui_sides(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_eval_toggle_button)) THEN
                CALL log_gui_event('Eval toggle button pressed')
                CALL toggle_eval_display(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_pgn_toggle_button)) THEN
                CALL log_gui_event('PGN toggle button pressed')
                CALL toggle_pgn_display(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_pgn_eval_toggle_button)) THEN
                CALL log_gui_event('PGN eval toggle button pressed')
                CALL toggle_pgn_eval_display(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_pgn_time_toggle_button)) THEN
                CALL log_gui_event('PGN time toggle button pressed')
                CALL toggle_pgn_time_display(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_fan_toggle_button)) THEN
                CALL log_gui_event('FAN toggle button pressed')
                CALL toggle_fan_display(hwnd)
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
        CASE (WM_LBUTTONDOWN)
            CALL log_gui_message('WM_LBUTTONDOWN', wparam, lparam)
            CALL handle_board_click(hwnd, lparam)
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

    RECURSIVE INTEGER(c_intptr_t) FUNCTION eval_graph_proc(hwnd, msg, wparam, lparam) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: msg
        INTEGER(c_intptr_t), VALUE :: wparam, lparam
        TYPE(PAINTSTRUCT) :: paint_struct
        TYPE(c_ptr) :: device_context
        INTEGER(c_int) :: ignored_int

        SELECT CASE (msg)
        CASE (WM_PAINT)
            device_context = BeginPaint(hwnd, paint_struct)
            CALL draw_eval_graph(device_context, hwnd)
            ignored_int = EndPaint(hwnd, paint_struct)
            eval_graph_proc = 0_c_intptr_t
        CASE (WM_LBUTTONDOWN)
            CALL set_review_position(gui_main_hwnd, graph_half_move_from_click(hwnd, lparam))
            eval_graph_proc = 0_c_intptr_t
        CASE (WM_CLOSE)
            ignored_int = DestroyWindow(hwnd)
            eval_graph_proc = 0_c_intptr_t
        CASE (WM_DESTROY)
            gui_graph_hwnd = c_null_ptr
            eval_graph_proc = 0_c_intptr_t
        CASE DEFAULT
            eval_graph_proc = DefWindowProcA(hwnd, msg, wparam, lparam)
        END SELECT
    END FUNCTION eval_graph_proc

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

    RECURSIVE SUBROUTINE create_gui_controls(parent_hwnd)
        TYPE(c_ptr), VALUE :: parent_hwnd
        TYPE(c_ptr) :: label_hwnd
        CHARACTER(LEN=16) :: default_depth_text

        CALL log_gui_checkpoint('create_gui_controls', 'entered')
        WRITE(default_depth_text, '(I0)') DEFAULT_SEARCH_DEPTH
        hwnd_setup_help = create_control('STATIC', 'Color', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT, 24_c_int, 58_c_int, 20_c_int, parent_hwnd)
        label_hwnd = create_control('STATIC', 'Mode', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT + 64_c_int, 24_c_int, 72_c_int, 20_c_int, parent_hwnd)
        label_hwnd = create_control('STATIC', 'Time', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT + 142_c_int, 24_c_int, 58_c_int, 20_c_int, parent_hwnd)
        label_hwnd = create_control('STATIC', 'Depth', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT + 206_c_int, 24_c_int, 38_c_int, 20_c_int, parent_hwnd)
        label_hwnd = create_control('STATIC', 'Book Max', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT + 246_c_int, 24_c_int, 76_c_int, 20_c_int, parent_hwnd)
        hwnd_color_edit = create_control('COMBOBOX', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_VSCROLL + CBS_DROPDOWNLIST, PANEL_LEFT, 64_c_int, 58_c_int, 200_c_int, parent_hwnd)
        CALL add_combo_item(hwnd_color_edit, 'white')
        CALL add_combo_item(hwnd_color_edit, 'black')
        CALL set_combo_selection(hwnd_color_edit, 0)
        hwnd_mode_edit = create_control('COMBOBOX', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_VSCROLL + CBS_DROPDOWNLIST, PANEL_LEFT + 64_c_int, 64_c_int, 72_c_int, 200_c_int, parent_hwnd)
        CALL add_combo_item(hwnd_mode_edit, 'play')
        CALL add_combo_item(hwnd_mode_edit, 'analysis')
        CALL set_combo_selection(hwnd_mode_edit, 0)
        hwnd_time_edit = create_control('EDIT', '3+2', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, PANEL_LEFT + 142_c_int, 64_c_int, 58_c_int, 24_c_int, parent_hwnd)
        hwnd_depth_edit = create_control('COMBOBOX', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_VSCROLL + CBS_DROPDOWNLIST, PANEL_LEFT + 206_c_int, 64_c_int, 50_c_int, 220_c_int, parent_hwnd)
        CALL populate_integer_combo(hwnd_depth_edit, 1, 10)
        CALL set_combo_selection(hwnd_depth_edit, DEFAULT_SEARCH_DEPTH - 1)
        hwnd_book_limit_edit = create_control('COMBOBOX', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_VSCROLL + CBS_DROPDOWNLIST, PANEL_LEFT + 250_c_int, 64_c_int, 52_c_int, 600_c_int, parent_hwnd)
        CALL populate_integer_combo(hwnd_book_limit_edit, 0, 40)
        CALL set_combo_selection(hwnd_book_limit_edit, 40)
        hwnd_start_button = create_control('BUTTON', 'Start / Reset', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 300_c_int, 62_c_int, 92_c_int, 28_c_int, parent_hwnd)
        hwnd_help_button = create_control('BUTTON', 'Help', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, BOARD_LEFT + 186_c_int, 24_c_int, 60_c_int, 28_c_int, parent_hwnd)
        hwnd_flip_button = create_control('BUTTON', 'Flip', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, BOARD_LEFT + 254_c_int, 24_c_int, 60_c_int, 28_c_int, parent_hwnd)
        hwnd_review_start_button = create_control('BUTTON', '|<', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 408_c_int, 44_c_int, 28_c_int, parent_hwnd)
        hwnd_review_back_button = create_control('BUTTON', '<', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 52_c_int, 408_c_int, 44_c_int, 28_c_int, parent_hwnd)
        hwnd_review_forward_button = create_control('BUTTON', '>', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 104_c_int, 408_c_int, 44_c_int, 28_c_int, parent_hwnd)
        hwnd_review_end_button = create_control('BUTTON', '>|', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 156_c_int, 408_c_int, 44_c_int, 28_c_int, parent_hwnd)
        hwnd_graph_button = create_control('BUTTON', 'Graph', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 208_c_int, 408_c_int, 64_c_int, 28_c_int, parent_hwnd)
        hwnd_turn_static = create_control('STATIC', 'Game not started', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 112_c_int, 320_c_int, 20_c_int, parent_hwnd)
        hwnd_white_clock = create_control('STATIC', 'White: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 144_c_int, 180_c_int, 20_c_int, parent_hwnd)
        hwnd_black_clock = create_control('STATIC', 'Black: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT + 190_c_int, 144_c_int, 180_c_int, 20_c_int, parent_hwnd)
        hwnd_eval_static = create_control('STATIC', 'Eval (White): 0.00', 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 170_c_int, 150_c_int, 20_c_int, parent_hwnd)
        hwnd_eval_toggle_button = create_control('BUTTON', 'Eval: On', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 156_c_int, 166_c_int, 80_c_int, 28_c_int, parent_hwnd)
        hwnd_pause_button = create_control('BUTTON', 'Pause', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 242_c_int, 166_c_int, 78_c_int, 28_c_int, parent_hwnd)
        hwnd_move_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, &
            PANEL_LEFT, 198_c_int, 148_c_int, 28_c_int, parent_hwnd)
        hwnd_auto_submit_button = create_control('BUTTON', 'Auto: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 154_c_int, 196_c_int, 72_c_int, 30_c_int, parent_hwnd)
        hwnd_submit_button = create_control('BUTTON', 'Play Move', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 232_c_int, 196_c_int, 104_c_int, 30_c_int, parent_hwnd)
        hwnd_suggest_button = create_control('BUTTON', 'Suggest Move', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 238_c_int, 108_c_int, 30_c_int, parent_hwnd)
        hwnd_autoplay_button = create_control('BUTTON', 'Autoplay', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 120_c_int, 238_c_int, 100_c_int, 30_c_int, parent_hwnd)
        hwnd_takeback_button = create_control('BUTTON', 'Takeback', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 232_c_int, 238_c_int, 100_c_int, 30_c_int, parent_hwnd)
        hwnd_switch_button = create_control('BUTTON', 'Switch Sides', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 232_c_int, 276_c_int, 116_c_int, 30_c_int, parent_hwnd)
        hwnd_draw_button = create_control('BUTTON', 'Offer Draw', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 276_c_int, 108_c_int, 30_c_int, parent_hwnd)
        hwnd_resign_button = create_control('BUTTON', 'Resign', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 120_c_int, 276_c_int, 90_c_int, 30_c_int, parent_hwnd)
        hwnd_status_static = create_control('STATIC', TRIM(gui_status_text), 0_c_long, &
            WS_CHILD + WS_VISIBLE, PANEL_LEFT, 320_c_int, 340_c_int, 48_c_int, parent_hwnd)
        hwnd_pgn_toggle_button = create_control('BUTTON', 'PGN: On', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT, 372_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_pgn_eval_toggle_button = create_control('BUTTON', 'Eval: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 100_c_int, 372_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_pgn_time_toggle_button = create_control('BUTTON', 'Time: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 200_c_int, 372_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_fan_toggle_button = create_control('BUTTON', 'FAN: Off', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 300_c_int, 372_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_pgn_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_BORDER + WS_VSCROLL + ES_MULTILINE + ES_AUTOVSCROLL + ES_READONLY, &
            PANEL_LEFT, 444_c_int, 340_c_int, 82_c_int, parent_hwnd)
        label_hwnd = create_control('STATIC', 'FEN', 0_c_long, WS_CHILD + WS_VISIBLE, &
            PANEL_LEFT, 538_c_int, 36_c_int, 20_c_int, parent_hwnd)
        hwnd_fen_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, &
            PANEL_LEFT, 560_c_int, 222_c_int, 24_c_int, parent_hwnd)
        hwnd_load_fen_button = create_control('BUTTON', 'Load FEN', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 230_c_int, 558_c_int, 74_c_int, 28_c_int, parent_hwnd)
        hwnd_copy_fen_button = create_control('BUTTON', 'Copy FEN', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, PANEL_LEFT + 310_c_int, 558_c_int, 78_c_int, 28_c_int, parent_hwnd)

        original_move_edit_proc = SetWindowLongPtrA(hwnd_move_edit, GWL_WNDPROC, funptr_to_intptr(c_funloc(move_edit_proc)))
        CALL log_gui_value('original_move_edit_proc', intptr_to_text(original_move_edit_proc))

        CALL enable_game_controls(.FALSE.)
        CALL refresh_gui_labels()
        CALL log_gui_checkpoint('create_gui_controls', 'completed')
    END SUBROUTINE create_gui_controls

    RECURSIVE FUNCTION create_control(class_name, text, ex_style, style, x, y, width, height, parent_hwnd) RESULT(control_hwnd)
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

    RECURSIVE SUBROUTINE add_combo_item(combo_hwnd, text)
        TYPE(c_ptr), VALUE :: combo_hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(256), TARGET :: text_buffer
        INTEGER(c_intptr_t) :: ignored_result

        IF (.NOT. C_ASSOCIATED(combo_hwnd)) RETURN
        CALL set_c_string(text, text_buffer)
        ignored_result = SendMessageA(combo_hwnd, CB_ADDSTRING, 0_c_intptr_t, transfer_ptr(c_loc(text_buffer(1))))
    END SUBROUTINE add_combo_item

    RECURSIVE SUBROUTINE populate_integer_combo(combo_hwnd, first_value, last_value)
        TYPE(c_ptr), VALUE :: combo_hwnd
        INTEGER, INTENT(IN) :: first_value, last_value
        INTEGER :: value

        DO value = first_value, last_value
            CALL add_combo_item(combo_hwnd, TRIM(int_to_text(value)))
        END DO
    END SUBROUTINE populate_integer_combo

    RECURSIVE SUBROUTINE set_combo_selection(combo_hwnd, selection_index)
        TYPE(c_ptr), VALUE :: combo_hwnd
        INTEGER, INTENT(IN) :: selection_index
        INTEGER(c_intptr_t) :: ignored_result

        IF (.NOT. C_ASSOCIATED(combo_hwnd)) RETURN
        ignored_result = SendMessageA(combo_hwnd, CB_SETCURSEL, INT(selection_index, c_intptr_t), 0_c_intptr_t)
    END SUBROUTINE set_combo_selection

    RECURSIVE SUBROUTINE initialize_gdiplus_and_images()
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

    RECURSIVE SUBROUTINE load_piece_images()
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

    RECURSIVE SUBROUTINE load_one_piece_image(filename, image_out)
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

    RECURSIVE SUBROUTINE release_piece_images()
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

    RECURSIVE SUBROUTINE draw_gui(device_context, hwnd)
        TYPE(c_ptr), VALUE :: device_context, hwnd
        TYPE(Board_Type) :: display_board
        TYPE(RECT) :: client_rect, square_rect
        TYPE(c_ptr) :: light_brush, dark_brush, background_brush, graphics, highlight_pen, old_object, ignored_object
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
        highlight_pen = CreatePen(PS_SOLID, 3_c_int, rgb_color(210, 60, 40))
        ignored_int = SetBkMode(device_context, TRANSPARENT)
        ignored_color = SetTextColor(device_context, rgb_color(40, 40, 40))
        CALL draw_text(device_context, BOARD_LEFT, 32_c_int, 'Fortran Chess Click GUI')
        CALL draw_text(device_context, BOARD_LEFT, 52_c_int, 'Click a piece, then click its destination square.')
        CALL get_display_board(display_board)

        graphics = c_null_ptr
        IF (gdiplus_started) ignored_int = GdipCreateFromHDC(device_context, graphics)

        DO rank = 8, 1, -1
            DO file = 1, 8
                square_rect%left = INT(BOARD_LEFT + (file - 1) * SQUARE_PIXELS, c_long)
                square_rect%top = INT(BOARD_TOP + (8 - rank) * SQUARE_PIXELS, c_long)
                square_rect%right = square_rect%left + SQUARE_PIXELS
                square_rect%bottom = square_rect%top + SQUARE_PIXELS

                IF (.NOT. gui_board_flipped) THEN
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

                IF (.NOT. gui_replay_mode .AND. board_rank == gui_selected_from_rank .AND. board_file == gui_selected_from_file) THEN
                    old_object = SelectObject(device_context, highlight_pen)
                    ignored_int = MoveToEx(device_context, INT(square_rect%left, c_int), INT(square_rect%top, c_int), c_null_ptr)
                    ignored_int = LineTo(device_context, INT(square_rect%right, c_int), INT(square_rect%top, c_int))
                    ignored_int = LineTo(device_context, INT(square_rect%right, c_int), INT(square_rect%bottom, c_int))
                    ignored_int = LineTo(device_context, INT(square_rect%left, c_int), INT(square_rect%bottom, c_int))
                    ignored_int = LineTo(device_context, INT(square_rect%left, c_int), INT(square_rect%top, c_int))
                    ignored_object = SelectObject(device_context, old_object)
                END IF

                piece_idx = display_board%squares_piece(board_rank, board_file)
                IF (piece_idx == NO_PIECE) CYCLE
                IF (display_board%squares_color(board_rank, board_file) == WHITE) THEN
                    color_idx = 1
                ELSE
                    color_idx = 2
                END IF

                IF (C_ASSOCIATED(graphics) .AND. C_ASSOCIATED(piece_images(color_idx, piece_idx))) THEN
                    ignored_int = GdipDrawImageRectI(graphics, piece_images(color_idx, piece_idx), &
                        INT(square_rect%left, c_int) + 6_c_int, INT(square_rect%top, c_int) + 6_c_int, &
                        SQUARE_PIXELS - 12_c_int, SQUARE_PIXELS - 12_c_int)
                ELSE
                    piece_char = board_piece_char(display_board, board_rank, board_file)
                    piece_text(1) = ACHAR(IACHAR(piece_char), KIND=c_char)
                    piece_text(2) = c_null_char
                    IF (display_board%squares_color(board_rank, board_file) == WHITE) THEN
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
        ignored_int = DeleteObject(highlight_pen)
        ignored_int = DeleteObject(light_brush)
        ignored_int = DeleteObject(dark_brush)
        CALL log_gui_checkpoint('draw_gui', 'completed')
    END SUBROUTINE draw_gui

    RECURSIVE SUBROUTINE show_eval_graph_window()
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_name, window_title
        INTEGER(c_int) :: ignored_int

        IF (.NOT. gui_game_started .OR. gui_num_half_moves <= 0) THEN
            CALL set_status('No moves available for graphing yet.')
            CALL refresh_gui_labels()
            RETURN
        END IF

        IF (C_ASSOCIATED(gui_graph_hwnd)) THEN
            ignored_int = ShowWindow(gui_graph_hwnd, SW_SHOWNORMAL)
            ignored_int = UpdateWindow(gui_graph_hwnd)
            CALL force_window_refresh(gui_graph_hwnd)
            RETURN
        END IF

        CALL set_c_string('FortranChessEvalGraph', class_name)
        CALL set_c_string('Evaluation Graph', window_title)
        gui_graph_hwnd = CreateWindowExA(0_c_long, c_loc(class_name(1)), c_loc(window_title(1)), WS_OVERLAPPEDWINDOW, &
            CW_USEDEFAULT, CW_USEDEFAULT, GRAPH_WINDOW_WIDTH, GRAPH_WINDOW_HEIGHT, c_null_ptr, c_null_ptr, gui_instance, c_null_ptr)
        IF (.NOT. C_ASSOCIATED(gui_graph_hwnd)) THEN
            CALL set_status('Could not create the evaluation graph window.')
            CALL refresh_gui_labels()
            RETURN
        END IF

        ignored_int = ShowWindow(gui_graph_hwnd, SW_SHOWNORMAL)
        ignored_int = UpdateWindow(gui_graph_hwnd)
    END SUBROUTINE show_eval_graph_window

    RECURSIVE SUBROUTINE draw_eval_graph(device_context, hwnd)
        TYPE(c_ptr), VALUE :: device_context, hwnd
        TYPE(RECT) :: client_rect, fill_rect
        TYPE(c_ptr) :: background_brush, axis_pen, line_pen, marker_pen, row_pen, old_object, ignored_object
        INTEGER(c_int) :: ignored_int
        INTEGER :: plot_left, plot_top, plot_right, plot_bottom
        INTEGER :: plot_width, plot_height, idx, display_idx
        INTEGER :: x_pos, y_pos, max_abs_cp, clipped_cp, total_full_moves
        INTEGER :: move_num, white_idx, black_idx, cp_value, row_height, row_top, row_bottom
        INTEGER :: point_y(0:512), point_x(0:512)
        REAL :: value_scale
        CHARACTER(LEN=64) :: label_text
        CHARACTER(LEN=128) :: move_pair_text

        ignored_int = GetClientRect(hwnd, client_rect)
        background_brush = CreateSolidBrush(rgb_color(250, 250, 250))
        fill_rect%left = 0_c_long
        fill_rect%top = 0_c_long
        fill_rect%right = client_rect%right
        fill_rect%bottom = client_rect%bottom
        ignored_int = FillRect(device_context, fill_rect, background_brush)
        ignored_int = DeleteObject(background_brush)

        plot_left = 300
        plot_top = 40
        plot_right = INT(client_rect%right) - 48
        plot_bottom = INT(client_rect%bottom) - 40
        plot_width = MAX(1, plot_right - plot_left)
        plot_height = MAX(1, plot_bottom - plot_top)
        display_idx = current_display_half_move_index()
        total_full_moves = MAX(1, (gui_num_half_moves + 1) / 2)
        row_height = MAX(14, plot_height / total_full_moves)
        plot_bottom = plot_top + row_height * total_full_moves
        plot_height = MAX(1, plot_bottom - plot_top)

        max_abs_cp = 200
        DO idx = 1, gui_num_half_moves
            max_abs_cp = MAX(max_abs_cp, ABS(gui_move_eval_cp_history(idx)))
        END DO
        max_abs_cp = MIN(2000, ((max_abs_cp + 199) / 200) * 200)
        value_scale = REAL(max_abs_cp)

        axis_pen = CreatePen(PS_SOLID, 1_c_int, rgb_color(120, 120, 120))
        old_object = SelectObject(device_context, axis_pen)
        ignored_int = MoveToEx(device_context, plot_left, plot_top, c_null_ptr)
        ignored_int = LineTo(device_context, plot_left, plot_bottom)
        ignored_int = MoveToEx(device_context, plot_left, plot_bottom, c_null_ptr)
        ignored_int = LineTo(device_context, plot_right, plot_bottom)

        DO cp_value = -max_abs_cp, max_abs_cp, 200
            x_pos = plot_left + NINT((REAL(cp_value) + value_scale) * REAL(plot_width) / (2.0 * value_scale))
            ignored_int = MoveToEx(device_context, x_pos, plot_top, c_null_ptr)
            ignored_int = LineTo(device_context, x_pos, plot_bottom)
            CALL draw_text(device_context, x_pos - 10, plot_bottom + 8, TRIM(int_to_text(cp_value / 100)))
        END DO

        row_pen = CreatePen(PS_SOLID, 1_c_int, rgb_color(225, 225, 225))
        ignored_object = SelectObject(device_context, row_pen)
        DO move_num = 1, total_full_moves
            row_top = plot_top + (move_num - 1) * row_height
            row_bottom = row_top + row_height
            y_pos = row_top + row_height / 2
            ignored_int = MoveToEx(device_context, 16_c_int, y_pos, c_null_ptr)
            ignored_int = LineTo(device_context, plot_right, y_pos)
        END DO
        ignored_int = DeleteObject(row_pen)
        ignored_object = SelectObject(device_context, old_object)

        line_pen = CreatePen(PS_SOLID, 2_c_int, rgb_color(36, 92, 182))
        old_object = SelectObject(device_context, line_pen)
        point_x(0) = plot_left + plot_width / 2
        point_y(0) = plot_top
        ignored_int = MoveToEx(device_context, point_x(0), point_y(0), c_null_ptr)
        DO idx = 1, gui_num_half_moves
            clipped_cp = MAX(-max_abs_cp, MIN(max_abs_cp, gui_move_eval_cp_history(idx)))
            point_x(idx) = plot_left + NINT((REAL(clipped_cp) + value_scale) * REAL(plot_width) / (2.0 * value_scale))
            move_num = (idx + 1) / 2
            row_top = plot_top + (move_num - 1) * row_height
            IF (MOD(idx, 2) == 1) THEN
                point_y(idx) = row_top + row_height / 3
            ELSE
                point_y(idx) = row_top + (2 * row_height) / 3
            END IF
            ignored_int = LineTo(device_context, point_x(idx), point_y(idx))
        END DO
        ignored_object = SelectObject(device_context, old_object)
        ignored_int = DeleteObject(line_pen)

        marker_pen = CreatePen(PS_SOLID, 1_c_int, rgb_color(200, 40, 40))
        old_object = SelectObject(device_context, marker_pen)
        IF (display_idx <= 0) THEN
            x_pos = point_x(0)
            y_pos = point_y(0)
        ELSE
            x_pos = point_x(display_idx)
            y_pos = point_y(display_idx)
        END IF
        ignored_int = MoveToEx(device_context, plot_left, y_pos, c_null_ptr)
        ignored_int = LineTo(device_context, plot_right, y_pos)
        ignored_int = MoveToEx(device_context, x_pos - 4, y_pos - 4, c_null_ptr)
        ignored_int = LineTo(device_context, x_pos + 4, y_pos + 4)
        ignored_int = MoveToEx(device_context, x_pos - 4, y_pos + 4, c_null_ptr)
        ignored_int = LineTo(device_context, x_pos + 4, y_pos - 4)
        ignored_object = SelectObject(device_context, old_object)
        ignored_int = DeleteObject(marker_pen)

        WRITE(label_text, '(A,F0.2,A)') 'Scale: +/-', REAL(max_abs_cp) / 100.0, ' pawns'
        CALL draw_text(device_context, 16_c_int, 8_c_int, 'Evaluation Graph')
        CALL draw_text(device_context, 16_c_int, plot_top - 20, 'Move')
        CALL draw_text(device_context, plot_left, plot_bottom + 24, 'Evaluation (pawns)')
        CALL draw_text(device_context, plot_right - 120, 8_c_int, TRIM(label_text))

        DO move_num = 1, total_full_moves
            row_top = plot_top + (move_num - 1) * row_height
            y_pos = row_top + row_height / 2 - 8
            white_idx = 2 * move_num - 1
            black_idx = 2 * move_num
            move_pair_text = format_graph_move_pair(move_num)
            CALL draw_text(device_context, 16_c_int, y_pos, TRIM(move_pair_text))
        END DO
        ignored_object = SelectObject(device_context, old_object)
        ignored_int = DeleteObject(axis_pen)
    END SUBROUTINE draw_eval_graph

    RECURSIVE INTEGER FUNCTION graph_half_move_from_click(hwnd, lparam) RESULT(target_half_move)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_intptr_t), VALUE :: lparam
        TYPE(RECT) :: client_rect
        INTEGER(c_int) :: ignored_int
        INTEGER :: plot_left, plot_top, plot_right, plot_bottom, plot_height
        INTEGER :: click_x, click_y, row_height, total_full_moves, move_num
        INTEGER :: white_idx, black_idx, white_x, black_x, max_abs_cp, idx, clipped_cp
        REAL :: value_scale

        ignored_int = GetClientRect(hwnd, client_rect)
        plot_left = 300
        plot_top = 40
        plot_right = INT(client_rect%right) - 48
        plot_bottom = INT(client_rect%bottom) - 40
        plot_height = MAX(1, plot_bottom - plot_top)
        total_full_moves = MAX(1, (gui_num_half_moves + 1) / 2)
        row_height = MAX(14, plot_height / total_full_moves)
        plot_bottom = plot_top + row_height * total_full_moves

        click_x = INT(IAND(lparam, INT(Z'FFFF', c_intptr_t)))
        click_y = INT(IAND(ISHFT(lparam, -16), INT(Z'FFFF', c_intptr_t)))
        move_num = 1 + (MAX(plot_top, MIN(plot_bottom - 1, click_y)) - plot_top) / row_height
        move_num = MAX(1, MIN(total_full_moves, move_num))

        white_idx = 2 * move_num - 1
        black_idx = 2 * move_num
        IF (white_idx > gui_num_half_moves) THEN
            target_half_move = gui_num_half_moves
            RETURN
        END IF

        max_abs_cp = 200
        DO idx = 1, gui_num_half_moves
            max_abs_cp = MAX(max_abs_cp, ABS(gui_move_eval_cp_history(idx)))
        END DO
        max_abs_cp = MIN(2000, ((max_abs_cp + 199) / 200) * 200)
        value_scale = REAL(max_abs_cp)

        clipped_cp = MAX(-max_abs_cp, MIN(max_abs_cp, gui_move_eval_cp_history(white_idx)))
        white_x = plot_left + NINT((REAL(clipped_cp) + value_scale) * REAL(MAX(1, plot_right - plot_left)) / (2.0 * value_scale))
        IF (black_idx <= gui_num_half_moves) THEN
            clipped_cp = MAX(-max_abs_cp, MIN(max_abs_cp, gui_move_eval_cp_history(black_idx)))
            black_x = plot_left + NINT((REAL(clipped_cp) + value_scale) * REAL(MAX(1, plot_right - plot_left)) / (2.0 * value_scale))
            IF (ABS(click_x - white_x) <= ABS(click_x - black_x)) THEN
                target_half_move = white_idx
            ELSE
                target_half_move = black_idx
            END IF
        ELSE
            target_half_move = white_idx
        END IF
    END FUNCTION graph_half_move_from_click

    RECURSIVE SUBROUTINE start_new_game(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=MAX_CONTROL_TEXT) :: fen_text
        CHARACTER(LEN=MAX_GUI_TEXT) :: color_text, mode_text, time_text, depth_text, book_limit_text
        LOGICAL :: parsed_ok, fen_ok
        INTEGER :: parsed_book_limit, parsed_depth

        CALL log_gui_checkpoint('start_new_game', 'entered')
        IF (.NOT. validate_gui_opening_books(main_hwnd)) RETURN

        color_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_color_edit))))
        mode_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_mode_edit))))
        time_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_time_edit))))
        depth_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_depth_edit))))
        book_limit_text = TRIM(ADJUSTL(to_lower_string(get_control_text(hwnd_book_limit_edit))))
        fen_text = TRIM(ADJUSTL(get_control_text(hwnd_fen_edit)))
        CALL log_gui_value('start_new_game color', color_text)
        CALL log_gui_value('start_new_game mode', mode_text)
        CALL log_gui_value('start_new_game time', time_text)
        CALL log_gui_value('start_new_game depth', depth_text)
        CALL log_gui_value('start_new_game book max', book_limit_text)
        IF (gui_start_from_fen_pending) CALL log_gui_value('start_new_game fen', fen_text)

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
        gui_board_flipped = (gui_human_color == BLACK)

        IF (LEN_TRIM(mode_text) == 0 .OR. mode_text == 'play' .OR. mode_text == 'p') THEN
            gui_analysis_mode = .FALSE.
        ELSE IF (mode_text == 'analysis' .OR. mode_text == 'a' .OR. mode_text == 'analyze') THEN
            gui_analysis_mode = .TRUE.
        ELSE
            CALL set_status('Mode must be play or analysis.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('start_new_game', 'invalid mode input')
            RETURN
        END IF

        gui_time_control_enabled = .FALSE.
        gui_white_time_ms = 0
        gui_black_time_ms = 0
        gui_increment_ms = 0
        gui_search_depth = DEFAULT_SEARCH_DEPTH
        gui_book_max_moves_per_side = -1
        gui_pgn_time_control_tag = '-'
        IF (.NOT. gui_analysis_mode) THEN
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
        END IF

        parsed_ok = parse_nonnegative_integer(depth_text, parsed_depth)
        IF (.NOT. parsed_ok .OR. parsed_depth < 1 .OR. parsed_depth > 10) THEN
            CALL set_status('Depth must be an integer from 1 to 10. Depth is in ply.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('start_new_game', 'invalid depth input')
            RETURN
        END IF
        gui_search_depth = parsed_depth

        parsed_ok = parse_nonnegative_integer(book_limit_text, parsed_book_limit)
        IF (.NOT. parsed_ok .OR. parsed_book_limit > 40) THEN
            CALL set_status('Book max must be an integer from 0 to 40.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('start_new_game', 'invalid book limit input')
            RETURN
        END IF
        gui_book_max_moves_per_side = parsed_book_limit

        CALL init_board(gui_board)
        IF (gui_start_from_fen_pending) THEN
            fen_ok = set_board_from_fen(gui_board, fen_text)
            gui_start_from_fen_pending = .FALSE.
            IF (.NOT. fen_ok) THEN
                CALL set_status('Invalid FEN. Position was not loaded.')
                CALL refresh_gui_labels()
                CALL log_gui_failure('start_new_game', 'invalid FEN input')
                RETURN
            END IF
        END IF
        gui_replay_board = gui_board
        gui_game_started = .TRUE.
        gui_game_over = .FALSE.
        gui_game_lost_on_time = .FALSE.
        gui_autoplay_mode = .FALSE.
        gui_paused = .FALSE.
        gui_replay_mode = .FALSE.
        gui_selected_from_rank = 0
        gui_selected_from_file = 0
        gui_game_winner_color = NO_COLOR
        gui_current_game_status = GAME_ONGOING
        gui_time_forfeit_winner_color = NO_COLOR
        gui_num_half_moves = 0
        gui_replay_half_move_index = 0
        gui_move_history = ''
        gui_move_elapsed_ms_history = -1_8
        gui_move_eval_cp_history = 0
        gui_white_time_before_move = 0_8
        gui_black_time_before_move = 0_8
        gui_position_key_history = 0_8
        gui_position_key_history(1) = gui_board%zobrist_key
        gui_pgn_result = '*'
        CALL enable_game_controls(.TRUE.)
        CALL set_move_edit_text('')
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL log_gui_checkpoint('start_new_game', 'game initialized')

        CALL SYSTEM_CLOCK(gui_turn_start_count)
        IF (gui_analysis_mode) THEN
            CALL handle_post_move(main_hwnd, .FALSE., '')
        ELSE IF (gui_board%current_player == gui_ai_color) THEN
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

    RECURSIVE SUBROUTINE submit_human_move(main_hwnd)
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
        IF ((.NOT. gui_analysis_mode) .AND. gui_board%current_player /= gui_human_color) THEN
            CALL set_status('Wait for the computer to move.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('submit_human_move', 'not human turn')
            RETURN
        END IF

        move_text = TRIM(sanitize_move_text(get_control_text(hwnd_move_edit)))
        CALL log_gui_value('submit_human_move text', move_text)
        IF (LEN_TRIM(move_text) == 0) RETURN

        IF (gui_paused) THEN
            gui_paused = .FALSE.
            CALL SYSTEM_CLOCK(gui_turn_start_count)
            CALL set_status('Game resumed.')
            CALL log_gui_event('Game resumed by move entry')
        END IF

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

        IF (gui_time_control_enabled .AND. .NOT. gui_analysis_mode) THEN
            IF (.NOT. apply_clock_after_move(gui_board%current_player, spent_ms, gui_white_time_ms, gui_black_time_ms, gui_increment_ms)) THEN
                IF (continue_after_flag_fall(main_hwnd, gui_board%current_player, .TRUE.)) THEN
                    CALL log_gui_event('Human flagged, continuing untimed.')
                ELSE
                    gui_game_lost_on_time = .TRUE.
                    gui_time_forfeit_winner_color = get_opponent_color(gui_board%current_player)
                    CALL log_gui_failure('submit_human_move', 'human flagged on time')
                    CALL finish_game(main_hwnd, 'You lost on time.')
                    RETURN
                END IF
            END IF
        END IF

        san_text = move_to_san(gui_board, chosen_move, legal_moves, num_legal_moves)
        CALL log_gui_value('submit_human_move san', san_text)
        CALL append_move_to_history(san_text, spent_ms)
        CALL make_move(gui_board, chosen_move, move_info)
        gui_selected_from_rank = 0
        gui_selected_from_file = 0
        gui_played_moves(gui_num_half_moves) = chosen_move
        gui_unmake_history(gui_num_half_moves) = move_info
        gui_position_key_history(gui_num_half_moves + 1) = gui_board%zobrist_key
        gui_move_eval_cp_history(gui_num_half_moves) = display_eval_white_cp(gui_board)
        CALL set_move_edit_text('')
        IF (gui_analysis_mode) THEN
            CALL set_status('Played ' // TRIM(display_move_text(san_text)) // '.')
        ELSE
            CALL set_status('You played ' // TRIM(display_move_text(san_text)) // '.')
        END IF
        CALL handle_post_move(main_hwnd, .TRUE., san_text)
        CALL log_gui_checkpoint('submit_human_move', 'completed')
    END SUBROUTINE submit_human_move

    RECURSIVE SUBROUTINE maybe_submit_move_from_edit(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        INTEGER :: num_legal_moves, i, match_count
        CHARACTER(LEN=MAX_GUI_TEXT) :: move_text

        IF (gui_suppress_move_edit_submit) RETURN
        move_text = get_control_text(hwnd_move_edit)
        move_text = sanitize_move_text(move_text)
        CALL log_gui_value('maybe_submit_move_from_edit sanitized', move_text)
        IF (TRIM(move_text) /= TRIM(get_control_text(hwnd_move_edit))) THEN
            CALL set_move_edit_text(TRIM(move_text))
        END IF
        IF (LEN_TRIM(move_text) == 0) RETURN

        IF (.NOT. gui_auto_submit_moves) RETURN
        IF (.NOT. gui_game_started .OR. gui_game_over .OR. gui_autoplay_mode) RETURN
        IF ((.NOT. gui_analysis_mode) .AND. gui_board%current_player /= gui_human_color) RETURN

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        match_count = 0
        DO i = 1, num_legal_moves
            IF (move_matches_input(gui_board, legal_moves(i), legal_moves, num_legal_moves, move_text)) THEN
                match_count = match_count + 1
                IF (match_count > 1) EXIT
            END IF
        END DO
        IF (match_count == 1) THEN
            CALL log_gui_event('Auto-submitting unique completed move')
            CALL submit_human_move(main_hwnd)
        END IF
    END SUBROUTINE maybe_submit_move_from_edit

    RECURSIVE SUBROUTINE toggle_auto_submit(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_auto_submit_moves = .NOT. gui_auto_submit_moves
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_auto_submit

    RECURSIVE SUBROUTINE show_gui_help(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=768) :: help_text

        help_text = 'Start / Reset begins a new game using the setup boxes.' // CHAR(10) // CHAR(10) // &
            'Modes: play = normal game, analysis = no auto-reply and best-move suggestions.' // CHAR(10) // &
            'Moves can be entered by clicking a piece and then its destination square.' // CHAR(10) // &
            'The move box still accepts SAN, coordinate moves, and pawn shorthand like dc.' // CHAR(10) // &
            'Auto: On submits a uniquely matched typed move without Enter.' // CHAR(10) // &
            'Book Max sets opening-book half-moves per side; 0 turns book off.' // CHAR(10) // &
            'Paste a FEN in the bottom box and click Load FEN; Copy FEN copies the displayed position.' // CHAR(10) // &
            'Flip rotates the board display only.' // CHAR(10) // &
            'PGN / Eval / Time / FAN control what the movelist box shows.' // CHAR(10) // &
            '|<  <  >  >| review the finished game move by move.' // CHAR(10) // &
            'Graph opens an evaluation plot; ? and ?? mark large eval swings.'
        CALL show_message_box(main_hwnd, TRIM(help_text), 'Fortran Chess Click GUI Help', MB_OK)
    END SUBROUTINE show_gui_help

    RECURSIVE SUBROUTINE toggle_board_flip(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_board_flipped = .NOT. gui_board_flipped
        CALL set_status('Board flipped.')
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_board_flip

    RECURSIVE SUBROUTINE copy_current_fen(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        TYPE(Board_Type) :: display_board
        CHARACTER(LEN=128) :: fen_text
        INTEGER :: display_half_moves

        IF (gui_game_started) THEN
            CALL get_display_board(display_board)
            display_half_moves = current_display_half_move_index()
        ELSE
            CALL init_board(display_board)
            display_half_moves = 0
        END IF

        fen_text = board_to_fen(display_board, 0, display_half_moves / 2 + 1)
        CALL set_control_text(hwnd_fen_edit, TRIM(fen_text))
        IF (copy_text_to_clipboard(main_hwnd, TRIM(fen_text))) THEN
            CALL set_status('FEN copied to clipboard.')
        ELSE
            CALL set_status('FEN copied into the box. Clipboard copy failed.')
        END IF
        CALL refresh_gui_labels()
    END SUBROUTINE copy_current_fen

    RECURSIVE LOGICAL FUNCTION board_square_from_click(lparam, sq) RESULT(hit_board)
        INTEGER(c_intptr_t), VALUE :: lparam
        TYPE(Square_Type), INTENT(OUT) :: sq
        INTEGER :: click_x, click_y, display_file, display_rank

        click_x = INT(IAND(lparam, INT(Z'FFFF', c_intptr_t)))
        click_y = INT(IAND(ISHFT(lparam, -16), INT(Z'FFFF', c_intptr_t)))
        hit_board = .FALSE.
        sq%rank = 0
        sq%file = 0

        IF (click_x < BOARD_LEFT .OR. click_x >= BOARD_LEFT + BOARD_PIXELS) RETURN
        IF (click_y < BOARD_TOP .OR. click_y >= BOARD_TOP + BOARD_PIXELS) RETURN

        display_file = 1 + (click_x - BOARD_LEFT) / SQUARE_PIXELS
        display_rank = 8 - (click_y - BOARD_TOP) / SQUARE_PIXELS
        IF (.NOT. gui_board_flipped) THEN
            sq%file = display_file
            sq%rank = display_rank
        ELSE
            sq%file = 9 - display_file
            sq%rank = 9 - display_rank
        END IF
        hit_board = .TRUE.
    END FUNCTION board_square_from_click

    RECURSIVE SUBROUTINE handle_board_click(main_hwnd, lparam)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER(c_intptr_t), VALUE :: lparam
        TYPE(Square_Type) :: clicked_sq
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: chosen_move
        INTEGER :: num_legal_moves, i, match_count, queen_match_index
        CHARACTER(LEN=32) :: move_text

        IF (.NOT. board_square_from_click(lparam, clicked_sq)) RETURN
        IF (.NOT. gui_game_started) RETURN
        IF (gui_game_over .OR. gui_replay_mode) RETURN
        IF (gui_autoplay_mode) RETURN
        IF ((.NOT. gui_analysis_mode) .AND. gui_board%current_player /= gui_human_color) RETURN

        IF (gui_selected_from_rank == 0 .OR. gui_selected_from_file == 0) THEN
            IF (gui_board%squares_color(clicked_sq%rank, clicked_sq%file) == gui_board%current_player) THEN
                gui_selected_from_rank = clicked_sq%rank
                gui_selected_from_file = clicked_sq%file
                CALL set_status('Origin square selected.')
                CALL refresh_gui_labels()
                CALL invalidate_main_window()
            END IF
            RETURN
        END IF

        IF (clicked_sq%rank == gui_selected_from_rank .AND. clicked_sq%file == gui_selected_from_file) THEN
            gui_selected_from_rank = 0
            gui_selected_from_file = 0
            CALL set_status('Selection cleared.')
            CALL refresh_gui_labels()
            CALL invalidate_main_window()
            RETURN
        END IF

        IF (gui_board%squares_color(clicked_sq%rank, clicked_sq%file) == gui_board%current_player) THEN
            gui_selected_from_rank = clicked_sq%rank
            gui_selected_from_file = clicked_sq%file
            CALL set_status('Origin square changed.')
            CALL refresh_gui_labels()
            CALL invalidate_main_window()
            RETURN
        END IF

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        match_count = 0
        queen_match_index = 0
        DO i = 1, num_legal_moves
            IF (legal_moves(i)%from_sq%rank == gui_selected_from_rank .AND. &
                legal_moves(i)%from_sq%file == gui_selected_from_file .AND. &
                legal_moves(i)%to_sq%rank == clicked_sq%rank .AND. &
                legal_moves(i)%to_sq%file == clicked_sq%file) THEN
                match_count = match_count + 1
                chosen_move = legal_moves(i)
                IF (legal_moves(i)%promotion_piece == QUEEN) queen_match_index = i
            END IF
        END DO

        IF (match_count == 0) THEN
            CALL set_status('That destination is not legal for the selected piece.')
            CALL refresh_gui_labels()
            RETURN
        END IF

        IF (queen_match_index > 0) chosen_move = legal_moves(queen_match_index)
        move_text = move_to_coordinate(chosen_move)
        gui_selected_from_rank = 0
        gui_selected_from_file = 0
        CALL set_move_edit_text(TRIM(move_text))
        CALL submit_human_move(main_hwnd)
    END SUBROUTINE handle_board_click

    RECURSIVE SUBROUTINE review_to_start(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL set_review_position(main_hwnd, 0)
    END SUBROUTINE review_to_start

    RECURSIVE SUBROUTINE review_to_end(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL set_review_position(main_hwnd, gui_num_half_moves)
    END SUBROUTINE review_to_end

    RECURSIVE SUBROUTINE review_step(main_hwnd, step)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER, INTENT(IN) :: step

        CALL set_review_position(main_hwnd, current_display_half_move_index() + step)
    END SUBROUTINE review_step

    RECURSIVE SUBROUTINE set_review_position(main_hwnd, target_half_move)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER, INTENT(IN) :: target_half_move

        IF (.NOT. gui_game_over) THEN
            CALL set_status('Review navigation is available after the game ends.')
            CALL refresh_gui_labels()
            RETURN
        END IF
        IF (gui_num_half_moves <= 0) THEN
            CALL set_status('No moves available for review.')
            CALL refresh_gui_labels()
            RETURN
        END IF

        gui_replay_mode = .TRUE.
        gui_replay_half_move_index = MAX(0, MIN(gui_num_half_moves, target_half_move))
        CALL rebuild_replay_board()
        CALL update_review_status()
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
        IF (C_ASSOCIATED(gui_graph_hwnd)) CALL force_window_refresh(gui_graph_hwnd)
    END SUBROUTINE set_review_position

    RECURSIVE SUBROUTINE rebuild_replay_board()
        TYPE(UnmakeInfo_Type) :: replay_info
        INTEGER :: move_idx

        CALL init_board(gui_replay_board)
        DO move_idx = 1, gui_replay_half_move_index
            CALL make_move(gui_replay_board, gui_played_moves(move_idx), replay_info)
        END DO
    END SUBROUTINE rebuild_replay_board

    RECURSIVE SUBROUTINE update_review_status()
        TYPE(Board_Type) :: review_board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: suggested_move
        INTEGER :: num_legal_moves, display_half_moves
        LOGICAL :: suggestion_found
        CHARACTER(LEN=32) :: suggestion_san, suggestion_source, side_text
        CHARACTER(LEN=256) :: review_text

        CALL get_display_board(review_board)
        display_half_moves = current_display_half_move_index()
        CALL generate_moves(review_board, legal_moves, num_legal_moves)
        IF (review_board%current_player == WHITE) THEN
            side_text = 'White'
        ELSE
            side_text = 'Black'
        END IF

        WRITE(review_text, '(A,I0,A,I0,A)') 'Review ', display_half_moves, '/', gui_num_half_moves, '. '
        review_text = TRIM(review_text) // TRIM(side_text) // ' to move.'

        IF (num_legal_moves <= 0) THEN
            CALL set_move_edit_text('')
            CALL set_status(TRIM(review_text) // ' No legal moves.')
            RETURN
        END IF

        CALL suggest_move_for_position(review_board, legal_moves, num_legal_moves, gui_move_history, display_half_moves, &
            gui_white_book, gui_black_book, gui_search_depth, suggestion_found, suggested_move, suggestion_san, &
            suggestion_source, opening_book_allowed(review_board%current_player, display_half_moves, gui_book_max_moves_per_side), &
            ANALYSIS_SUGGESTION_TIME_LIMIT_SECONDS)
        IF (suggestion_found) THEN
            CALL set_move_edit_text(TRIM(suggestion_san))
            CALL set_status(TRIM(review_text) // ' Best move ' // TRIM(display_move_text(suggestion_san)) // ' (' // &
                TRIM(suggestion_source) // ').')
        ELSE
            CALL set_move_edit_text('')
            CALL set_status(TRIM(review_text) // ' No suggestion available.')
        END IF
    END SUBROUTINE update_review_status

    RECURSIVE SUBROUTINE get_display_board(board_out)
        TYPE(Board_Type), INTENT(OUT) :: board_out

        IF (gui_replay_mode) THEN
            board_out = gui_replay_board
        ELSE
            board_out = gui_board
        END IF
    END SUBROUTINE get_display_board

    RECURSIVE INTEGER FUNCTION current_display_half_move_index() RESULT(display_half_moves)
        IF (gui_replay_mode) THEN
            display_half_moves = gui_replay_half_move_index
        ELSE
            display_half_moves = gui_num_half_moves
        END IF
    END FUNCTION current_display_half_move_index

    RECURSIVE LOGICAL FUNCTION continue_after_flag_fall(main_hwnd, flagged_player_color, flagged_human) RESULT(continued)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER, INTENT(IN) :: flagged_player_color
        LOGICAL, INTENT(IN) :: flagged_human
        CHARACTER(LEN=128) :: question_text
        CHARACTER(LEN=32) :: player_name

        continued = .FALSE.
        IF (gui_autoplay_mode) RETURN

        IF (flagged_human) THEN
            player_name = 'Your'
        ELSE IF (flagged_player_color == WHITE) THEN
            player_name = 'White''s'
        ELSE
            player_name = 'Black''s'
        END IF
        question_text = TRIM(player_name) // ' time expired. Continue untimed?'
        IF (ask_yes_no(main_hwnd, TRIM(question_text), 'Time Expired')) THEN
            continued = .TRUE.
            gui_time_control_enabled = .FALSE.
            gui_game_lost_on_time = .FALSE.
            gui_time_forfeit_winner_color = NO_COLOR
            gui_paused = .FALSE.
            gui_autoplay_mode = .FALSE.
            CALL set_status('Time expired. Continuing untimed.')
            CALL refresh_gui_labels()
            CALL invalidate_main_window()
            CALL force_window_refresh(main_hwnd)
        END IF
    END FUNCTION continue_after_flag_fall

    RECURSIVE SUBROUTINE process_ai_turn(main_hwnd, played_move_text)
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

        IF (gui_time_control_enabled .AND. .NOT. gui_analysis_mode .AND. .NOT. book_move_found) THEN
            IF (.NOT. apply_clock_after_move(gui_board%current_player, clock_spent_ms, gui_white_time_ms, gui_black_time_ms, gui_increment_ms)) THEN
                IF (continue_after_flag_fall(main_hwnd, gui_board%current_player, .FALSE.)) THEN
                    CALL finish_ai_turn_debug_log('Computer flagged, but user continued untimed.')
                    CALL log_gui_event('Computer flagged, continuing untimed.')
                ELSE
                    gui_game_lost_on_time = .TRUE.
                    gui_time_forfeit_winner_color = get_opponent_color(gui_board%current_player)
                    CALL finish_ai_turn_debug_log('Computer lost on time before completing the move.')
                    CALL log_gui_failure('process_ai_turn', 'computer flagged on time')
                    gui_autoplay_mode = .FALSE.
                    CALL finish_game(main_hwnd, 'Computer lost on time.')
                    RETURN
                END IF
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
        gui_move_eval_cp_history(gui_num_half_moves) = display_eval_white_cp(gui_board)
        IF (book_move_found) THEN
            CALL finish_ai_turn_debug_log('Opening-book move played.')
        ELSE
            CALL finish_ai_turn_debug_log('Search move played.')
        END IF
        played_move_text = san_text
            CALL set_status('Computer played ' // TRIM(display_move_text(san_text)) // '.')
        CALL log_gui_checkpoint('process_ai_turn', 'completed')
    END SUBROUTINE process_ai_turn

    RECURSIVE SUBROUTINE handle_post_move(main_hwnd, human_moved, move_text)
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
            ELSE IF (gui_analysis_mode) THEN
                CALL set_status('Analysis: evaluating position...')
                CALL refresh_gui_labels()
                CALL force_window_refresh(main_hwnd)
                CALL update_analysis_status()
            ELSE IF (gui_board%current_player == gui_human_color) THEN
                IF (.NOT. last_move_was_human) THEN
                    CALL set_status('Computer played ' // TRIM(display_move_text(last_move_text)) // '. Your move.')
                ELSE
                    CALL set_status('Your move.')
                END IF
            ELSE
                CALL set_status('Computer is thinking...')
            END IF
            CALL refresh_gui_labels()
            CALL invalidate_main_window()

            automatic_turn_pending = (.NOT. gui_paused) .AND. &
                (gui_autoplay_mode .OR. ((.NOT. gui_analysis_mode) .AND. gui_board%current_player == gui_ai_color))
            IF (.NOT. automatic_turn_pending) EXIT

            CALL force_window_refresh(main_hwnd)
            CALL process_ai_turn(main_hwnd, last_move_text)
            last_move_was_human = .FALSE.
            IF (gui_game_over) EXIT
        END DO

        CALL log_gui_checkpoint('handle_post_move', 'completed')
    END SUBROUTINE handle_post_move

    RECURSIVE SUBROUTINE suggest_gui_move(main_hwnd)
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
        IF ((.NOT. gui_analysis_mode) .AND. gui_board%current_player /= gui_human_color) THEN
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
            suggestion_source, opening_book_allowed(gui_board%current_player, gui_num_half_moves, gui_book_max_moves_per_side), &
            ANALYSIS_SUGGESTION_TIME_LIMIT_SECONDS)
        IF (.NOT. suggestion_found) THEN
            CALL set_status('No suggestion available.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('suggest_gui_move', 'no suggestion found')
            RETURN
        END IF

        CALL set_move_edit_text(TRIM(suggestion_san))
        CALL set_status('Suggestion: ' // TRIM(display_move_text(suggestion_san)) // ' (' // TRIM(suggestion_source) // ').')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_value('suggest_gui_move san', suggestion_san)
        CALL log_gui_checkpoint('suggest_gui_move', 'completed')
    END SUBROUTINE suggest_gui_move

    RECURSIVE SUBROUTINE update_analysis_status()
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(Move_Type) :: suggested_move
        INTEGER :: num_legal_moves
        LOGICAL :: suggestion_found
        CHARACTER(LEN=32) :: suggestion_san, suggestion_source, side_text

        CALL generate_moves(gui_board, legal_moves, num_legal_moves)
        IF (gui_board%current_player == WHITE) THEN
            side_text = 'White'
        ELSE
            side_text = 'Black'
        END IF

        IF (num_legal_moves <= 0) THEN
        CALL set_move_edit_text('')
            CALL set_status('Analysis: ' // TRIM(side_text) // ' to move. No legal moves.')
            RETURN
        END IF

        CALL suggest_move_for_position(gui_board, legal_moves, num_legal_moves, gui_move_history, gui_num_half_moves, &
            gui_white_book, gui_black_book, gui_search_depth, suggestion_found, suggested_move, suggestion_san, &
            suggestion_source, opening_book_allowed(gui_board%current_player, gui_num_half_moves, gui_book_max_moves_per_side), &
            ANALYSIS_SUGGESTION_TIME_LIMIT_SECONDS)
        IF (suggestion_found) THEN
            CALL set_move_edit_text(TRIM(suggestion_san))
            CALL set_status('Analysis: ' // TRIM(side_text) // ' to move. Best move ' // TRIM(display_move_text(suggestion_san)) // &
                ' (' // TRIM(suggestion_source) // ').')
        ELSE
            CALL set_move_edit_text('')
            CALL set_status('Analysis: ' // TRIM(side_text) // ' to move. No suggestion available.')
        END IF
    END SUBROUTINE update_analysis_status

    RECURSIVE SUBROUTINE autoplay_gui_game(main_hwnd)
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
        IF (gui_paused) THEN
            CALL set_status('Resume first.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('autoplay_gui_game', 'game paused')
            RETURN
        END IF

        gui_autoplay_mode = .TRUE.
        CALL set_status('Autoplay running...')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL handle_post_move(main_hwnd, .FALSE., '')
        CALL log_gui_checkpoint('autoplay_gui_game', 'completed')
    END SUBROUTINE autoplay_gui_game

    RECURSIVE SUBROUTINE toggle_pause(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER(KIND=8) :: now_count, elapsed_now, remaining_now

        CALL log_gui_checkpoint('toggle_pause', 'entered')
        IF (.NOT. gui_game_started .OR. gui_game_over) THEN
            CALL set_status('No active game.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('toggle_pause', 'no active game')
            RETURN
        END IF
        IF (.NOT. gui_time_control_enabled .OR. gui_analysis_mode) THEN
            CALL set_status('Pause is only available in timed play mode.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('toggle_pause', 'pause unavailable')
            RETURN
        END IF

        IF (gui_paused) THEN
            gui_paused = .FALSE.
            CALL SYSTEM_CLOCK(gui_turn_start_count)
            IF (gui_board%current_player == gui_ai_color) THEN
                CALL set_status('Computer is thinking...')
                CALL refresh_gui_labels()
                CALL force_window_refresh(main_hwnd)
                CALL handle_post_move(main_hwnd, .FALSE., '')
            ELSE
                CALL set_status('Game resumed.')
                CALL refresh_gui_labels()
                CALL force_window_refresh(main_hwnd)
            END IF
            CALL log_gui_checkpoint('toggle_pause', 'completed')
            RETURN
        END IF

        CALL SYSTEM_CLOCK(now_count)
        elapsed_now = elapsed_milliseconds(gui_turn_start_count, now_count, gui_count_rate)
        IF (gui_board%current_player == WHITE) THEN
            remaining_now = gui_white_time_ms - elapsed_now
            IF (remaining_now <= 0_8) THEN
                gui_white_time_ms = 0_8
                gui_game_lost_on_time = .TRUE.
                gui_time_forfeit_winner_color = BLACK
                CALL log_gui_failure('toggle_pause', 'white flagged while pausing')
                CALL finish_game(main_hwnd, 'You lost on time.')
                RETURN
            END IF
            gui_white_time_ms = remaining_now
        ELSE
            remaining_now = gui_black_time_ms - elapsed_now
            IF (remaining_now <= 0_8) THEN
                gui_black_time_ms = 0_8
                gui_game_lost_on_time = .TRUE.
                gui_time_forfeit_winner_color = WHITE
                CALL log_gui_failure('toggle_pause', 'black flagged while pausing')
                CALL finish_game(main_hwnd, 'You lost on time.')
                RETURN
            END IF
            gui_black_time_ms = remaining_now
        END IF

        gui_paused = .TRUE.
        gui_autoplay_mode = .FALSE.
        CALL set_status('Game paused.')
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_checkpoint('toggle_pause', 'completed')
    END SUBROUTINE toggle_pause

    RECURSIVE SUBROUTINE toggle_eval_display(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_show_eval = .NOT. gui_show_eval
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_eval_display

    RECURSIVE SUBROUTINE toggle_pgn_display(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_show_pgn = .NOT. gui_show_pgn
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_pgn_display

    RECURSIVE SUBROUTINE toggle_pgn_eval_display(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_show_pgn_eval = .NOT. gui_show_pgn_eval
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_pgn_eval_display

    RECURSIVE SUBROUTINE toggle_pgn_time_display(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_show_pgn_time = .NOT. gui_show_pgn_time
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_pgn_time_display

    RECURSIVE SUBROUTINE toggle_fan_display(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        gui_show_fan = .NOT. gui_show_fan
        IF (gui_replay_mode) THEN
            CALL update_review_status()
        ELSE IF (gui_analysis_mode .AND. gui_game_started .AND. .NOT. gui_game_over) THEN
            CALL update_analysis_status()
        END IF
        CALL refresh_gui_labels()
        CALL force_window_refresh(main_hwnd)
    END SUBROUTINE toggle_fan_display

    RECURSIVE SUBROUTINE takeback_gui_move(main_hwnd)
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
        IF (gui_analysis_mode) THEN
            undo_count = MIN(1, gui_num_half_moves)
        ELSE
            undo_count = MIN(2, gui_num_half_moves)
        END IF
        undone_count = 0
        DO WHILE (undo_count > 0)
            CALL unmake_move(gui_board, gui_played_moves(gui_num_half_moves), gui_unmake_history(gui_num_half_moves))
            gui_white_time_ms = gui_white_time_before_move(gui_num_half_moves)
            gui_black_time_ms = gui_black_time_before_move(gui_num_half_moves)
            gui_move_history(gui_num_half_moves) = ''
            gui_move_elapsed_ms_history(gui_num_half_moves) = -1_8
            gui_move_eval_cp_history(gui_num_half_moves) = 0
            gui_num_half_moves = gui_num_half_moves - 1
            undo_count = undo_count - 1
            undone_count = undone_count + 1
        END DO

        gui_game_over = .FALSE.
        gui_game_lost_on_time = .FALSE.
        gui_game_winner_color = NO_COLOR
        gui_current_game_status = GAME_ONGOING
        gui_time_forfeit_winner_color = NO_COLOR
        gui_replay_mode = .FALSE.
        gui_replay_half_move_index = 0
        gui_selected_from_rank = 0
        gui_selected_from_file = 0
        gui_pgn_result = '*'
        CALL enable_game_controls(.TRUE.)
        CALL set_status('Took back ' // TRIM(int_to_text(undone_count)) // ' move(s).')
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
        CALL log_gui_checkpoint('takeback_gui_move', 'completed')
    END SUBROUTINE takeback_gui_move

    RECURSIVE SUBROUTINE switch_gui_sides(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER :: previous_human_color

        CALL log_gui_checkpoint('switch_gui_sides', 'entered')
        previous_human_color = gui_human_color
        gui_autoplay_mode = .FALSE.

        IF (gui_human_color == WHITE) THEN
            gui_human_color = BLACK
            gui_ai_color = WHITE
            CALL set_combo_selection(hwnd_color_edit, 1)
        ELSE
            gui_human_color = WHITE
            gui_ai_color = BLACK
            CALL set_combo_selection(hwnd_color_edit, 0)
        END IF
        gui_board_flipped = .NOT. gui_board_flipped
        gui_selected_from_rank = 0
        gui_selected_from_file = 0

        IF (.NOT. gui_game_started) THEN
            CALL set_status('Sides switched.')
            CALL refresh_gui_labels()
            CALL invalidate_main_window()
            CALL force_window_refresh(main_hwnd)
            CALL log_gui_checkpoint('switch_gui_sides', 'completed')
            RETURN
        END IF

        CALL invalidate_main_window()

        IF (gui_game_over) THEN
            CALL set_status('Sides switched.')
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
            CALL log_gui_checkpoint('switch_gui_sides', 'completed')
            RETURN
        END IF

        IF (gui_analysis_mode) THEN
            CALL set_status('Analysis: evaluating position...')
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
            CALL update_analysis_status()
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
            CALL log_gui_checkpoint('switch_gui_sides', 'completed')
            RETURN
        END IF

        IF (gui_board%current_player == gui_ai_color) THEN
            CALL set_status('Computer is thinking...')
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
            CALL handle_post_move(main_hwnd, .FALSE., '')
        ELSE
            IF (previous_human_color /= gui_human_color) THEN
                CALL set_status('Sides switched. Your move.')
            ELSE
                CALL set_status('Your move.')
            END IF
            CALL refresh_gui_labels()
            CALL force_window_refresh(main_hwnd)
        END IF
        CALL log_gui_checkpoint('switch_gui_sides', 'completed')
    END SUBROUTINE switch_gui_sides

    RECURSIVE SUBROUTINE offer_draw(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('offer_draw', 'entered')
        IF (.NOT. gui_game_started .OR. gui_game_over) THEN
            CALL set_status('No active game.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('offer_draw', 'no active game')
            RETURN
        END IF
        IF (gui_analysis_mode) THEN
            CALL set_status('Draw offers are disabled in analysis mode.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('offer_draw', 'analysis mode')
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

    RECURSIVE SUBROUTINE resign_game(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd

        CALL log_gui_checkpoint('resign_game', 'entered')
        IF (.NOT. gui_game_started .OR. gui_game_over) THEN
            CALL set_status('No active game.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('resign_game', 'no active game')
            RETURN
        END IF
        IF (gui_analysis_mode) THEN
            CALL set_status('Resignation is disabled in analysis mode.')
            CALL refresh_gui_labels()
            CALL log_gui_failure('resign_game', 'analysis mode')
            RETURN
        END IF

        IF (gui_human_color == WHITE) THEN
            CALL finish_game(main_hwnd, 'White resigned. Black wins.', '0-1')
        ELSE
            CALL finish_game(main_hwnd, 'Black resigned. White wins.', '1-0')
        END IF
    END SUBROUTINE resign_game

    RECURSIVE SUBROUTINE finish_game(main_hwnd, final_status, result_override)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=*), INTENT(IN) :: final_status
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: result_override

        CALL log_gui_checkpoint('finish_game', 'entered')
        CALL log_gui_value('finish_game status', final_status)
        gui_game_over = .TRUE.
        gui_autoplay_mode = .FALSE.
        gui_selected_from_rank = 0
        gui_selected_from_file = 0
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
        gui_replay_mode = (gui_num_half_moves > 0)
        gui_replay_half_move_index = gui_num_half_moves
        gui_replay_board = gui_board
        IF (gui_replay_mode) THEN
            CALL update_review_status()
        ELSE
            CALL set_status(TRIM(final_status))
        END IF
        CALL refresh_gui_labels()
        CALL invalidate_main_window()
        CALL force_window_refresh(main_hwnd)
        IF (C_ASSOCIATED(gui_graph_hwnd)) CALL force_window_refresh(gui_graph_hwnd)
        CALL log_gui_checkpoint('finish_game', 'completed')
    END SUBROUTINE finish_game

    RECURSIVE SUBROUTINE on_gui_timer(main_hwnd, timer_id)
        TYPE(c_ptr), VALUE :: main_hwnd
        INTEGER(c_intptr_t), VALUE :: timer_id
        INTEGER(KIND=8) :: now_count, elapsed_now, remaining_now

        CALL log_gui_checkpoint('on_gui_timer', 'entered')
        IF (timer_id /= TIMER_CLOCK) RETURN
        IF (gui_time_control_enabled .AND. .NOT. gui_analysis_mode .AND. .NOT. gui_paused .AND. gui_game_started .AND. &
            .NOT. gui_game_over .AND. gui_board%current_player == gui_human_color) THEN
            CALL SYSTEM_CLOCK(now_count)
            elapsed_now = elapsed_milliseconds(gui_turn_start_count, now_count, gui_count_rate)
            IF (gui_board%current_player == WHITE) THEN
                remaining_now = gui_white_time_ms - elapsed_now
                IF (remaining_now <= 0_8) THEN
                    gui_white_time_ms = 0
                    IF (continue_after_flag_fall(main_hwnd, WHITE, .TRUE.)) THEN
                        CALL log_gui_event('White human flagged on timer, continuing untimed.')
                        RETURN
                    ELSE
                        gui_game_lost_on_time = .TRUE.
                        gui_time_forfeit_winner_color = BLACK
                        CALL log_gui_failure('on_gui_timer', 'white human flagged on time')
                        CALL finish_game(main_hwnd, 'You lost on time.')
                        RETURN
                    END IF
                END IF
            ELSE
                remaining_now = gui_black_time_ms - elapsed_now
                IF (remaining_now <= 0_8) THEN
                    gui_black_time_ms = 0
                    IF (continue_after_flag_fall(main_hwnd, BLACK, .TRUE.)) THEN
                        CALL log_gui_event('Black human flagged on timer, continuing untimed.')
                        RETURN
                    ELSE
                        gui_game_lost_on_time = .TRUE.
                        gui_time_forfeit_winner_color = WHITE
                        CALL log_gui_failure('on_gui_timer', 'black human flagged on time')
                        CALL finish_game(main_hwnd, 'You lost on time.')
                        RETURN
                    END IF
                END IF
            END IF
        END IF
        CALL refresh_gui_labels()
    END SUBROUTINE on_gui_timer

    RECURSIVE SUBROUTINE refresh_gui_labels()
        CHARACTER(LEN=64) :: turn_text, white_text, black_text, eval_text
        INTEGER :: eval_white_cp
        INTEGER(c_int) :: ignored_int

        IF (.NOT. gui_game_started) THEN
            turn_text = 'Game not started'
        ELSE IF (gui_replay_mode) THEN
            WRITE(turn_text, '(A,I0,A,I0,A)') 'Review ', current_display_half_move_index(), '/', gui_num_half_moves, ''
        ELSE IF (gui_game_over) THEN
            turn_text = 'Game over'
        ELSE IF (gui_paused) THEN
            IF (gui_board%current_player == WHITE) THEN
                turn_text = 'Paused: White to move'
            ELSE
                turn_text = 'Paused: Black to move'
            END IF
        ELSE IF (gui_analysis_mode) THEN
            IF (gui_board%current_player == WHITE) THEN
                turn_text = 'Analysis: White to move'
            ELSE
                turn_text = 'Analysis: Black to move'
            END IF
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

        IF (gui_show_eval) THEN
            eval_white_cp = current_eval_white_cp()
            WRITE(eval_text, '(A,F7.2)') 'Eval (White): ', REAL(eval_white_cp) / 100.0
        ELSE
            eval_text = 'Eval hidden'
        END IF

        CALL set_control_text(hwnd_turn_static, TRIM(turn_text))
        CALL set_control_text(hwnd_white_clock, TRIM(white_text))
        CALL set_control_text(hwnd_black_clock, TRIM(black_text))
        CALL set_control_text(hwnd_eval_static, TRIM(eval_text))
        IF (gui_paused) THEN
            CALL set_control_text(hwnd_pause_button, 'Resume')
        ELSE
            CALL set_control_text(hwnd_pause_button, 'Pause')
        END IF
        IF (gui_show_eval) THEN
            CALL set_control_text(hwnd_eval_toggle_button, 'Eval: On')
        ELSE
            CALL set_control_text(hwnd_eval_toggle_button, 'Eval: Off')
        END IF
        IF (gui_auto_submit_moves) THEN
            CALL set_control_text(hwnd_auto_submit_button, 'Auto: On')
        ELSE
            CALL set_control_text(hwnd_auto_submit_button, 'Auto: Off')
        END IF
        IF (gui_show_pgn) THEN
            CALL set_control_text(hwnd_pgn_toggle_button, 'PGN: On')
            ignored_int = ShowWindow(hwnd_pgn_edit, SW_SHOWNORMAL)
            CALL update_pgn_display()
        ELSE
            CALL set_control_text(hwnd_pgn_toggle_button, 'PGN: Off')
            ignored_int = ShowWindow(hwnd_pgn_edit, SW_HIDE)
            CALL set_control_text(hwnd_pgn_edit, '')
        END IF
        IF (gui_show_pgn_eval) THEN
            CALL set_control_text(hwnd_pgn_eval_toggle_button, 'Eval: On')
        ELSE
            CALL set_control_text(hwnd_pgn_eval_toggle_button, 'Eval: Off')
        END IF
        IF (gui_show_pgn_time) THEN
            CALL set_control_text(hwnd_pgn_time_toggle_button, 'Time: On')
        ELSE
            CALL set_control_text(hwnd_pgn_time_toggle_button, 'Time: Off')
        END IF
        IF (gui_show_fan) THEN
            CALL set_control_text(hwnd_fan_toggle_button, 'FAN: On')
        ELSE
            CALL set_control_text(hwnd_fan_toggle_button, 'FAN: Off')
        END IF
        CALL set_control_text(hwnd_status_static, TRIM(gui_status_text))
    END SUBROUTINE refresh_gui_labels

    RECURSIVE INTEGER(KIND=8) FUNCTION display_clock_value(player_color) RESULT(clock_value)
        INTEGER, INTENT(IN) :: player_color
        INTEGER(KIND=8) :: now_count, elapsed_now

        IF (player_color == WHITE) THEN
            clock_value = gui_white_time_ms
        ELSE
            clock_value = gui_black_time_ms
        END IF
        IF (.NOT. gui_time_control_enabled) RETURN
        IF (gui_analysis_mode) RETURN
        IF (gui_paused) RETURN
        IF (.NOT. gui_game_started .OR. gui_game_over) RETURN
        IF (gui_board%current_player == player_color) THEN
            CALL SYSTEM_CLOCK(now_count)
            elapsed_now = elapsed_milliseconds(gui_turn_start_count, now_count, gui_count_rate)
            clock_value = MAX(0_8, clock_value - elapsed_now)
        END IF
    END FUNCTION display_clock_value

    RECURSIVE INTEGER FUNCTION current_eval_white_cp() RESULT(eval_white_cp)
        TYPE(Board_Type) :: display_board

        IF (.NOT. gui_game_started) THEN
            eval_white_cp = 0
            RETURN
        END IF
        CALL get_display_board(display_board)
        eval_white_cp = display_eval_white_cp(display_board)
    END FUNCTION current_eval_white_cp

    RECURSIVE SUBROUTINE enable_game_controls(enable_flag)
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
        IF (C_ASSOCIATED(hwnd_pause_button)) ignored_int = EnableWindow(hwnd_pause_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_switch_button)) ignored_int = EnableWindow(hwnd_switch_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_draw_button)) ignored_int = EnableWindow(hwnd_draw_button, enabled_int)
        IF (C_ASSOCIATED(hwnd_resign_button)) ignored_int = EnableWindow(hwnd_resign_button, enabled_int)
    END SUBROUTINE enable_game_controls

    RECURSIVE SUBROUTINE append_move_to_history(move_text, elapsed_ms)
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        INTEGER(KIND=8), INTENT(IN), OPTIONAL :: elapsed_ms

        IF (gui_num_half_moves < SIZE(gui_move_history)) THEN
            gui_num_half_moves = gui_num_half_moves + 1
            gui_move_history(gui_num_half_moves) = TRIM(move_text)
            IF (PRESENT(elapsed_ms)) gui_move_elapsed_ms_history(gui_num_half_moves) = elapsed_ms
        END IF
    END SUBROUTINE append_move_to_history

    RECURSIVE FUNCTION format_gui_movetext() RESULT(text)
        CHARACTER(LEN=MAX_CONTROL_TEXT) :: text
        CHARACTER(LEN=32) :: move_number_text
        CHARACTER(LEN=32) :: eval_text, time_text
        INTEGER :: move_idx, full_move_number, text_len

        text = ''
        text_len = 0
        gui_move_display_start = -1
        gui_move_display_end = -1
        DO move_idx = 1, gui_num_half_moves, 2
            full_move_number = (move_idx + 1) / 2
            WRITE(move_number_text, '(I0,A)') full_move_number, '.'
            CALL append_text_segment(text, text_len, TRIM(move_number_text))
            gui_move_display_start(move_idx) = text_len
            CALL append_text_segment(text, text_len, TRIM(format_display_history_move(move_idx)))
            gui_move_display_end(move_idx) = text_len
            IF (gui_show_pgn_eval) THEN
                eval_text = format_gui_eval_comment(move_idx)
                IF (LEN_TRIM(eval_text) > 0) THEN
                    CALL append_text_segment(text, text_len, ' ')
                    CALL append_text_segment(text, text_len, TRIM(eval_text))
                END IF
            END IF
            IF (gui_show_pgn_time) THEN
                time_text = format_gui_move_time(gui_move_elapsed_ms_history(move_idx))
                IF (LEN_TRIM(time_text) > 0) THEN
                    CALL append_text_segment(text, text_len, ' ')
                    CALL append_text_segment(text, text_len, TRIM(time_text))
                END IF
            END IF
            IF (move_idx + 1 <= gui_num_half_moves) THEN
                CALL append_text_segment(text, text_len, ' ')
                gui_move_display_start(move_idx + 1) = text_len
                CALL append_text_segment(text, text_len, TRIM(format_display_history_move(move_idx + 1)))
                gui_move_display_end(move_idx + 1) = text_len
                IF (gui_show_pgn_eval) THEN
                    eval_text = format_gui_eval_comment(move_idx + 1)
                    IF (LEN_TRIM(eval_text) > 0) THEN
                        CALL append_text_segment(text, text_len, ' ')
                        CALL append_text_segment(text, text_len, TRIM(eval_text))
                    END IF
                END IF
                IF (gui_show_pgn_time) THEN
                    time_text = format_gui_move_time(gui_move_elapsed_ms_history(move_idx + 1))
                    IF (LEN_TRIM(time_text) > 0) THEN
                        CALL append_text_segment(text, text_len, ' ')
                        CALL append_text_segment(text, text_len, TRIM(time_text))
                    END IF
                END IF
            END IF
            IF (move_idx + 2 <= gui_num_half_moves) CALL append_text_segment(text, text_len, ' ')
        END DO

        IF (gui_game_over .AND. LEN_TRIM(gui_pgn_result) > 0 .AND. gui_pgn_result /= '*') THEN
            IF (text_len > 0) CALL append_text_segment(text, text_len, ' ')
            CALL append_text_segment(text, text_len, TRIM(gui_pgn_result))
        END IF
    END FUNCTION format_gui_movetext

    RECURSIVE FUNCTION format_graph_move_pair(full_move_number) RESULT(text)
        INTEGER, INTENT(IN) :: full_move_number
        CHARACTER(LEN=128) :: text
        INTEGER :: white_idx, black_idx

        white_idx = 2 * full_move_number - 1
        black_idx = 2 * full_move_number

        WRITE(text, '(I0,A)') full_move_number, '. '
        IF (white_idx <= gui_num_half_moves) THEN
            text = TRIM(text) // TRIM(display_move_text(gui_move_history(white_idx)))
        END IF
        IF (black_idx <= gui_num_half_moves) THEN
            text = TRIM(text) // ' ' // TRIM(display_move_text(gui_move_history(black_idx)))
        END IF
    END FUNCTION format_graph_move_pair

    RECURSIVE FUNCTION format_display_history_move(move_idx) RESULT(text)
        INTEGER, INTENT(IN) :: move_idx
        CHARACTER(LEN=64) :: text
        INTEGER :: severity

        text = display_move_text(gui_move_history(move_idx))
        severity = move_mistake_severity(move_idx)
        IF (severity >= 2) THEN
            text = TRIM(text) // '??'
        ELSE IF (severity == 1) THEN
            text = TRIM(text) // '?'
        END IF
    END FUNCTION format_display_history_move

    RECURSIVE FUNCTION format_gui_eval_comment(move_idx) RESULT(text)
        INTEGER, INTENT(IN) :: move_idx
        CHARACTER(LEN=32) :: text

        text = '{' // TRIM(format_signed_pawn_text(gui_move_eval_cp_history(move_idx))) // '/' // &
            TRIM(format_signed_pawn_text(eval_delta_cp(move_idx))) // '}'
    END FUNCTION format_gui_eval_comment

    RECURSIVE INTEGER FUNCTION eval_delta_cp(move_idx) RESULT(delta_cp)
        INTEGER, INTENT(IN) :: move_idx
        INTEGER :: previous_eval_cp

        IF (move_idx <= 1) THEN
            previous_eval_cp = 0
        ELSE
            previous_eval_cp = gui_move_eval_cp_history(move_idx - 1)
        END IF
        delta_cp = gui_move_eval_cp_history(move_idx) - previous_eval_cp
    END FUNCTION eval_delta_cp

    RECURSIVE INTEGER FUNCTION move_mistake_severity(move_idx) RESULT(severity)
        INTEGER, INTENT(IN) :: move_idx
        INTEGER :: mover_loss_cp, delta_cp_value

        delta_cp_value = eval_delta_cp(move_idx)
        IF (MOD(move_idx, 2) == 1) THEN
            mover_loss_cp = -delta_cp_value
        ELSE
            mover_loss_cp = delta_cp_value
        END IF

        IF (mover_loss_cp >= MOVE_BLUNDER_CP) THEN
            severity = 2
        ELSE IF (mover_loss_cp >= MOVE_MISTAKE_CP) THEN
            severity = 1
        ELSE
            severity = 0
        END IF
    END FUNCTION move_mistake_severity

    RECURSIVE FUNCTION format_signed_pawn_text(eval_cp) RESULT(text)
        INTEGER, INTENT(IN) :: eval_cp
        CHARACTER(LEN=16) :: text

        WRITE(text, '(SP,F0.2,SS)') REAL(eval_cp) / 100.0
    END FUNCTION format_signed_pawn_text

    RECURSIVE FUNCTION format_gui_move_time(elapsed_ms) RESULT(text)
        INTEGER(KIND=8), INTENT(IN) :: elapsed_ms
        CHARACTER(LEN=16) :: text
        INTEGER(KIND=8) :: elapsed_seconds

        text = ''
        IF (elapsed_ms < 0_8) RETURN
        elapsed_seconds = MAX(0_8, elapsed_ms / 1000_8)
        WRITE(text, '(A,I0,A)') '{', elapsed_seconds, 's}'
    END FUNCTION format_gui_move_time

    RECURSIVE FUNCTION display_move_text(san_text) RESULT(display_text)
        CHARACTER(LEN=*), INTENT(IN) :: san_text
        CHARACTER(LEN=64) :: display_text

        display_text = ''
        IF (gui_show_fan) THEN
            display_text = san_to_fan_text(TRIM(san_text))
        ELSE
            display_text = san_text
        END IF
    END FUNCTION display_move_text

    RECURSIVE FUNCTION san_to_fan_text(san_text) RESULT(fan_text)
        CHARACTER(LEN=*), INTENT(IN) :: san_text
        CHARACTER(LEN=64) :: fan_text
        INTEGER :: i, out_pos
        CHARACTER(LEN=1) :: ch

        fan_text = ''
        out_pos = 0
        DO i = 1, LEN_TRIM(san_text)
            ch = san_text(i:i)
            SELECT CASE (ch)
            CASE ('K')
                CALL append_fan_symbol(fan_text, out_pos, '♔')
            CASE ('Q')
                CALL append_fan_symbol(fan_text, out_pos, '♕')
            CASE ('R')
                CALL append_fan_symbol(fan_text, out_pos, '♖')
            CASE ('B')
                CALL append_fan_symbol(fan_text, out_pos, '♗')
            CASE ('N')
                CALL append_fan_symbol(fan_text, out_pos, '♘')
            CASE DEFAULT
                IF (out_pos < LEN(fan_text)) THEN
                    out_pos = out_pos + 1
                    fan_text(out_pos:out_pos) = ch
                END IF
            END SELECT
        END DO
    END FUNCTION san_to_fan_text

    RECURSIVE SUBROUTINE append_fan_symbol(text, current_len, symbol)
        CHARACTER(LEN=*), INTENT(INOUT) :: text
        INTEGER, INTENT(INOUT) :: current_len
        CHARACTER(LEN=1), INTENT(IN) :: symbol

        IF (current_len >= LEN(text)) RETURN
        current_len = current_len + 1
        text(current_len:current_len) = symbol
    END SUBROUTINE append_fan_symbol

    RECURSIVE SUBROUTINE append_text_segment(buffer, current_len, segment)
        CHARACTER(LEN=*), INTENT(INOUT) :: buffer
        INTEGER, INTENT(INOUT) :: current_len
        CHARACTER(LEN=*), INTENT(IN) :: segment
        INTEGER :: start_pos, copy_len

        start_pos = current_len + 1
        IF (start_pos > LEN(buffer)) RETURN
        copy_len = MIN(LEN(segment), LEN(buffer) - start_pos + 1)
        IF (copy_len <= 0) RETURN
        buffer(start_pos:start_pos + copy_len - 1) = segment(1:copy_len)
        current_len = current_len + copy_len
    END SUBROUTINE append_text_segment

    RECURSIVE SUBROUTINE update_pgn_display()
        INTEGER(c_intptr_t) :: ignored_result
        INTEGER :: highlight_index

        CALL set_control_text(hwnd_pgn_edit, TRIM(format_gui_movetext()))
        IF (.NOT. gui_replay_mode) THEN
            ignored_result = SendMessageA(hwnd_pgn_edit, EM_SETSEL, 0_c_intptr_t, 0_c_intptr_t)
            RETURN
        END IF

        highlight_index = current_display_half_move_index()
        IF (highlight_index <= 0 .OR. gui_move_display_start(highlight_index) < 0) THEN
            ignored_result = SendMessageA(hwnd_pgn_edit, EM_SETSEL, 0_c_intptr_t, 0_c_intptr_t)
            RETURN
        END IF
        ignored_result = SendMessageA(hwnd_pgn_edit, EM_SETSEL, INT(gui_move_display_start(highlight_index), c_intptr_t), &
            INT(gui_move_display_end(highlight_index), c_intptr_t))
        ignored_result = SendMessageA(hwnd_pgn_edit, EM_SCROLLCARET, 0_c_intptr_t, 0_c_intptr_t)
    END SUBROUTINE update_pgn_display

    RECURSIVE SUBROUTINE set_status(text)
        CHARACTER(LEN=*), INTENT(IN) :: text

        gui_status_text = ''
        gui_status_text = text
        CALL log_gui_value('status', text)
    END SUBROUTINE set_status

    RECURSIVE FUNCTION format_pgn_time_control_tag(initial_ms, increment_ms_value) RESULT(tag)
        INTEGER(KIND=8), INTENT(IN) :: initial_ms, increment_ms_value
        CHARACTER(LEN=32) :: tag
        INTEGER(KIND=8) :: initial_seconds, increment_seconds

        initial_seconds = MAX(0_8, initial_ms / 1000_8)
        increment_seconds = MAX(0_8, increment_ms_value / 1000_8)
        WRITE(tag, '(I0,A,I0)') initial_seconds, '+', increment_seconds
    END FUNCTION format_pgn_time_control_tag

    RECURSIVE SUBROUTINE set_control_text(hwnd, text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(MAX_CONTROL_TEXT), TARGET :: text_buffer
        INTEGER(c_int) :: ignored_int

        IF (.NOT. C_ASSOCIATED(hwnd)) RETURN
        CALL set_c_string(text, text_buffer)
        ignored_int = SetWindowTextA(hwnd, c_loc(text_buffer(1)))
    END SUBROUTINE set_control_text

    RECURSIVE SUBROUTINE set_move_edit_text(text)
        CHARACTER(LEN=*), INTENT(IN) :: text

        gui_suppress_move_edit_submit = .TRUE.
        CALL set_control_text(hwnd_move_edit, text)
        gui_suppress_move_edit_submit = .FALSE.
    END SUBROUTINE set_move_edit_text

    RECURSIVE LOGICAL FUNCTION copy_text_to_clipboard(hwnd, text) RESULT(copied_ok)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(MAX_CONTROL_TEXT), TARGET :: text_buffer
        CHARACTER(KIND=c_char), POINTER :: clipboard_chars(:)
        TYPE(c_ptr) :: memory_handle, memory_ptr, clipboard_handle
        INTEGER(c_int) :: ignored_int
        INTEGER :: text_len, idx

        copied_ok = .FALSE.
        CALL set_c_string(text, text_buffer)
        text_len = MIN(LEN_TRIM(text), MAX_CONTROL_TEXT - 1) + 1

        IF (OpenClipboard(hwnd) == 0_c_int) RETURN
        ignored_int = EmptyClipboard()
        memory_handle = GlobalAlloc(GMEM_MOVEABLE, INT(text_len, c_size_t))
        IF (.NOT. C_ASSOCIATED(memory_handle)) THEN
            ignored_int = CloseClipboard()
            RETURN
        END IF

        memory_ptr = GlobalLock(memory_handle)
        IF (.NOT. C_ASSOCIATED(memory_ptr)) THEN
            ignored_int = CloseClipboard()
            RETURN
        END IF

        CALL c_f_pointer(memory_ptr, clipboard_chars, (/ text_len /))
        DO idx = 1, text_len
            clipboard_chars(idx) = text_buffer(idx)
        END DO

        ignored_int = GlobalUnlock(memory_handle)
        clipboard_handle = SetClipboardData(CF_TEXT, memory_handle)
        copied_ok = C_ASSOCIATED(clipboard_handle)
        ignored_int = CloseClipboard()
    END FUNCTION copy_text_to_clipboard

    RECURSIVE FUNCTION get_control_text(hwnd) RESULT(text)
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

    RECURSIVE SUBROUTINE invalidate_main_window()
        INTEGER(c_int) :: ignored_int

        IF (C_ASSOCIATED(gui_main_hwnd)) ignored_int = InvalidateRect(gui_main_hwnd, c_null_ptr, 1_c_int)
    END SUBROUTINE invalidate_main_window

    RECURSIVE SUBROUTINE force_window_refresh(hwnd)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int) :: ignored_int

        ignored_int = InvalidateRect(hwnd, c_null_ptr, 1_c_int)
        ignored_int = UpdateWindow(hwnd)
    END SUBROUTINE force_window_refresh

    RECURSIVE CHARACTER(LEN=1) FUNCTION board_piece_char(board, rank, file)
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

    RECURSIVE SUBROUTINE draw_text(device_context, x_pos, y_pos, text)
        TYPE(c_ptr), VALUE :: device_context
        INTEGER(c_int), VALUE :: x_pos, y_pos
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(256), TARGET :: text_buffer
        INTEGER(c_int) :: text_len, ignored_int

        CALL set_c_string(text, text_buffer)
        text_len = LEN_TRIM(text)
        ignored_int = TextOutA(device_context, x_pos, y_pos, c_loc(text_buffer(1)), text_len)
    END SUBROUTINE draw_text

    RECURSIVE INTEGER(c_long) FUNCTION rgb_color(red, green, blue)
        INTEGER, INTENT(IN) :: red, green, blue

        rgb_color = INT(red + 256 * green + 65536 * blue, c_long)
    END FUNCTION rgb_color

    RECURSIVE TYPE(c_ptr) FUNCTION ptr_from_intptr(raw_value) RESULT(pointer_value)
        INTEGER(c_intptr_t), INTENT(IN) :: raw_value
        TYPE(c_ptr) :: template_ptr

        template_ptr = c_null_ptr
        pointer_value = TRANSFER(raw_value, template_ptr)
    END FUNCTION ptr_from_intptr

    RECURSIVE INTEGER(c_intptr_t) FUNCTION funptr_to_intptr(raw_value) RESULT(pointer_value)
        TYPE(c_funptr), INTENT(IN) :: raw_value
        INTEGER(c_intptr_t) :: template_value

        template_value = 0_c_intptr_t
        pointer_value = TRANSFER(raw_value, template_value)
    END FUNCTION funptr_to_intptr

    RECURSIVE INTEGER(c_intptr_t) FUNCTION transfer_ptr(raw_ptr) RESULT(pointer_value)
        TYPE(c_ptr), INTENT(IN) :: raw_ptr
        INTEGER(c_intptr_t) :: template_value

        template_value = 0_c_intptr_t
        pointer_value = TRANSFER(raw_ptr, template_value)
    END FUNCTION transfer_ptr

    RECURSIVE FUNCTION int_to_text(value) RESULT(text)
        INTEGER, INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION int_to_text

    RECURSIVE FUNCTION int64_to_text(value) RESULT(text)
        INTEGER(KIND=8), INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION int64_to_text

    RECURSIVE FUNCTION intptr_to_text(value) RESULT(text)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(I0)') value
    END FUNCTION intptr_to_text

    RECURSIVE FUNCTION real_to_text(value) RESULT(text)
        REAL, INTENT(IN) :: value
        CHARACTER(LEN=32) :: text

        WRITE(text, '(F10.3)') value
        text = ADJUSTL(text)
    END FUNCTION real_to_text

    RECURSIVE FUNCTION logical_to_text(flag) RESULT(text)
        LOGICAL, INTENT(IN) :: flag
        CHARACTER(LEN=5) :: text

        IF (flag) THEN
            text = 'true'
        ELSE
            text = 'false'
        END IF
    END FUNCTION logical_to_text

    RECURSIVE LOGICAL FUNCTION parse_nonnegative_integer(text, value_out) RESULT(parsed_ok)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER, INTENT(OUT) :: value_out
        INTEGER :: ios

        value_out = -1
        READ(text, *, IOSTAT=ios) value_out
        parsed_ok = (ios == 0 .AND. value_out >= 0)
    END FUNCTION parse_nonnegative_integer

    RECURSIVE LOGICAL FUNCTION opening_book_allowed(player_color, half_moves_played, move_limit_per_side) RESULT(allowed)
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

    RECURSIVE LOGICAL FUNCTION validate_gui_opening_books(main_hwnd) RESULT(ok)
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

    RECURSIVE SUBROUTINE show_book_error(main_hwnd, side_name, book)
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

    RECURSIVE SUBROUTINE show_message_box(hwnd, text, title, box_type)
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

    RECURSIVE LOGICAL FUNCTION ask_yes_no(hwnd, text, title) RESULT(answer_yes)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text, title
        CHARACTER(KIND=c_char), DIMENSION(512), TARGET :: text_buffer
        CHARACTER(KIND=c_char), DIMENSION(128), TARGET :: title_buffer
        INTEGER(c_int) :: dialog_result

        CALL log_gui_event('Yes/No box [' // TRIM(title) // ']: ' // TRIM(text))
        CALL set_c_string(text, text_buffer)
        CALL set_c_string(title, title_buffer)
        dialog_result = MessageBoxA(hwnd, c_loc(text_buffer(1)), c_loc(title_buffer(1)), MB_YESNO + MB_ICONQUESTION)
        answer_yes = (dialog_result == IDYES)
    END FUNCTION ask_yes_no

    RECURSIVE FUNCTION sanitize_move_text(text_in) RESULT(clean_text)
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

    RECURSIVE SUBROUTINE set_c_string(text, buffer)
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

    RECURSIVE SUBROUTINE set_wide_string(text, buffer)
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

END MODULE Chess_Click_GUI_App

PROGRAM Fortran_Chess_Click_GUI
    USE Chess_Click_GUI_App, ONLY: run_chess_click_gui
    IMPLICIT NONE

    CALL run_chess_click_gui()
END PROGRAM Fortran_Chess_Click_GUI
