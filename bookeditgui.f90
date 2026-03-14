MODULE Book_Edit_GUI_App
    USE, INTRINSIC :: iso_c_binding
    USE Chess_Types
    USE Board_Utils, ONLY: init_board
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move
    USE Notation_Utils, ONLY: move_matches_input, move_to_san
    USE Opening_Book, ONLY: Opening_Book_Type, load_opening_book
    USE Transposition_Table, ONLY: init_zobrist_keys
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_book_edit_gui

    INTEGER, PARAMETER :: MAX_BOOK_LINES_LOCAL = 512
    INTEGER, PARAMETER :: MAX_BOOK_MOVES_LOCAL = 256
    INTEGER, PARAMETER :: MAX_PRESERVED_LINES_LOCAL = 2048
    INTEGER, PARAMETER :: MAX_CONTROL_TEXT = 131072
    INTEGER, PARAMETER :: MAX_STATUS_TEXT = 512
    INTEGER, PARAMETER :: MAX_PATH_TEXT = 512

    INTEGER(c_int), PARAMETER :: WM_CREATE = 1_c_int
    INTEGER(c_int), PARAMETER :: WM_DESTROY = 2_c_int
    INTEGER(c_int), PARAMETER :: WM_PAINT = 15_c_int
    INTEGER(c_int), PARAMETER :: WM_CLOSE = 16_c_int
    INTEGER(c_int), PARAMETER :: WM_COMMAND = 273_c_int
    INTEGER(c_int), PARAMETER :: WM_LBUTTONDOWN = 513_c_int
    INTEGER(c_int), PARAMETER :: SW_SHOWNORMAL = 1_c_int
    INTEGER(c_int), PARAMETER :: CW_USEDEFAULT = -2147483647_c_int - 1_c_int
    INTEGER(c_int), PARAMETER :: TRANSPARENT = 1_c_int
    INTEGER(c_int), PARAMETER :: GWL_WNDPROC = -4_c_int
    INTEGER(c_int), PARAMETER :: EM_CHARFROMPOS = 215_c_int
    INTEGER(c_int), PARAMETER :: EM_LINEFROMCHAR = 201_c_int
    INTEGER(c_int), PARAMETER :: MB_OK = 0_c_int
    INTEGER(c_int), PARAMETER :: MB_ICONERROR = 16_c_int
    INTEGER(c_long), PARAMETER :: WS_OVERLAPPEDWINDOW = INT(Z'00CF0000', c_long)
    INTEGER(c_long), PARAMETER :: WS_CHILD = INT(Z'40000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_VISIBLE = INT(Z'10000000', c_long)
    INTEGER(c_long), PARAMETER :: WS_TABSTOP = INT(Z'00010000', c_long)
    INTEGER(c_long), PARAMETER :: WS_BORDER = INT(Z'00800000', c_long)
    INTEGER(c_long), PARAMETER :: WS_VSCROLL = INT(Z'00200000', c_long)
    INTEGER(c_long), PARAMETER :: WS_HSCROLL = INT(Z'00100000', c_long)
    INTEGER(c_long), PARAMETER :: ES_AUTOHSCROLL = INT(Z'00000080', c_long)
    INTEGER(c_long), PARAMETER :: ES_MULTILINE = INT(Z'00000004', c_long)
    INTEGER(c_long), PARAMETER :: ES_AUTOVSCROLL = INT(Z'00000040', c_long)
    INTEGER(c_long), PARAMETER :: ES_READONLY = INT(Z'00000800', c_long)
    INTEGER(c_long), PARAMETER :: WS_EX_CLIENTEDGE = INT(Z'00000200', c_long)
    INTEGER(c_int), PARAMETER :: PS_SOLID = 0_c_int

    INTEGER(c_int), PARAMETER :: WINDOW_WIDTH = 1080_c_int
    INTEGER(c_int), PARAMETER :: WINDOW_HEIGHT = 780_c_int
    INTEGER(c_int), PARAMETER :: BOARD_LEFT = 24_c_int
    INTEGER(c_int), PARAMETER :: BOARD_TOP = 100_c_int
    INTEGER(c_int), PARAMETER :: BOARD_PIXELS = 512_c_int
    INTEGER(c_int), PARAMETER :: SQUARE_PIXELS = BOARD_PIXELS / 8_c_int
    INTEGER(c_int), PARAMETER :: PANEL_LEFT = 570_c_int

    INTEGER(c_int), PARAMETER :: IDC_PATH_EDIT = 1001_c_int
    INTEGER(c_int), PARAMETER :: IDC_LOAD_BUTTON = 1002_c_int
    INTEGER(c_int), PARAMETER :: IDC_SAVE_BUTTON = 1003_c_int
    INTEGER(c_int), PARAMETER :: IDC_ROOT_BUTTON = 1004_c_int
    INTEGER(c_int), PARAMETER :: IDC_BACK_BUTTON = 1005_c_int
    INTEGER(c_int), PARAMETER :: IDC_REMOVE_BUTTON = 1006_c_int
    INTEGER(c_int), PARAMETER :: IDC_STATUS_STATIC = 1007_c_int
    INTEGER(c_int), PARAMETER :: IDC_CURRENT_LINE_EDIT = 1008_c_int
    INTEGER(c_int), PARAMETER :: IDC_CONT_EDIT = 1009_c_int
    INTEGER(c_int), PARAMETER :: IDC_ALL_LINES_EDIT = 1010_c_int
    INTEGER(c_int), PARAMETER :: IDC_ALL_LINES_MODE_BUTTON = 1011_c_int

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

    TYPE(Board_Type), SAVE :: editor_board
    TYPE(c_ptr), SAVE :: gui_instance = c_null_ptr
    TYPE(c_ptr), SAVE :: gui_main_hwnd = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_path_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_status_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_current_line = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_continuations = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_all_lines = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_all_lines_mode_button = c_null_ptr
    INTEGER(c_intptr_t), SAVE :: original_all_lines_proc = 0_c_intptr_t
    INTEGER, SAVE :: editor_num_lines = 0
    INTEGER, DIMENSION(MAX_BOOK_LINES_LOCAL), SAVE :: editor_line_lengths = 0
    CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES_LOCAL, MAX_BOOK_LINES_LOCAL), SAVE :: editor_lines = ''
    INTEGER, SAVE :: current_prefix_len = 0
    CHARACTER(LEN=32), DIMENSION(MAX_BOOK_MOVES_LOCAL), SAVE :: current_prefix = ''
    INTEGER, SAVE :: selected_from_rank = 0
    INTEGER, SAVE :: selected_from_file = 0
    CHARACTER(LEN=MAX_STATUS_TEXT), SAVE :: status_text = 'Load a book or start from the root position.'
    LOGICAL, SAVE :: show_all_lines_full = .TRUE.
    INTEGER, SAVE :: preserved_line_count = 0
    INTEGER, DIMENSION(MAX_PRESERVED_LINES_LOCAL), SAVE :: preserved_line_anchor = 1
    CHARACTER(LEN=512), DIMENSION(MAX_PRESERVED_LINES_LOCAL), SAVE :: preserved_lines = ''
    TYPE(c_ptr), SAVE :: piece_images(2, 6)
    INTEGER(c_size_t), SAVE :: gdiplus_token = 0_c_size_t
    LOGICAL, SAVE :: gdiplus_started = .FALSE.
    LOGICAL, SAVE :: piece_images_loaded = .FALSE.

    INTEGER(c_int), PARAMETER :: GDI_PLUS_OK = 0_c_int

    INTERFACE
        FUNCTION GetModuleHandleA(module_name) BIND(C, NAME="GetModuleHandleA")
            IMPORT :: c_ptr
            TYPE(c_ptr), VALUE :: module_name
            TYPE(c_ptr) :: GetModuleHandleA
        END FUNCTION GetModuleHandleA

        FUNCTION RegisterClassExA(window_class) BIND(C, NAME="RegisterClassExA")
            IMPORT :: WNDCLASSEXA, c_short
            TYPE(WNDCLASSEXA), INTENT(IN) :: window_class
            INTEGER(c_short) :: RegisterClassExA
        END FUNCTION RegisterClassExA

        FUNCTION CreateWindowExA(ex_style, class_name, window_name, style, x, y, width, height, &
                                 parent, menu, instance, param) BIND(C, NAME="CreateWindowExA")
            IMPORT :: c_long, c_int, c_ptr
            INTEGER(c_long), VALUE :: ex_style
            TYPE(c_ptr), VALUE :: class_name, window_name
            INTEGER(c_long), VALUE :: style
            INTEGER(c_int), VALUE :: x, y, width, height
            TYPE(c_ptr), VALUE :: parent, menu, instance, param
            TYPE(c_ptr) :: CreateWindowExA
        END FUNCTION CreateWindowExA

        FUNCTION DefWindowProcA(hwnd, message, w_param, l_param) BIND(C, NAME="DefWindowProcA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: message
            INTEGER(c_intptr_t), VALUE :: w_param, l_param
            INTEGER(c_intptr_t) :: DefWindowProcA
        END FUNCTION DefWindowProcA

        FUNCTION ShowWindow(hwnd, command_show) BIND(C, NAME="ShowWindow")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: command_show
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

        FUNCTION GetMessageA(message_struct, hwnd, filter_min, filter_max) BIND(C, NAME="GetMessageA")
            IMPORT :: MSG, c_ptr, c_int
            TYPE(MSG), INTENT(OUT) :: message_struct
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: filter_min, filter_max
            INTEGER(c_int) :: GetMessageA
        END FUNCTION GetMessageA

        FUNCTION TranslateMessage(message_struct) BIND(C, NAME="TranslateMessage")
            IMPORT :: MSG, c_int
            TYPE(MSG), INTENT(IN) :: message_struct
            INTEGER(c_int) :: TranslateMessage
        END FUNCTION TranslateMessage

        FUNCTION DispatchMessageA(message_struct) BIND(C, NAME="DispatchMessageA")
            IMPORT :: MSG, c_intptr_t
            TYPE(MSG), INTENT(IN) :: message_struct
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
            IMPORT :: c_ptr, c_int, PAINTSTRUCT
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
            IMPORT :: c_int, c_ptr
            INTEGER(c_int), VALUE :: color
            TYPE(c_ptr) :: CreateSolidBrush
        END FUNCTION CreateSolidBrush

        FUNCTION CreatePen(style, width, color) BIND(C, NAME="CreatePen")
            IMPORT :: c_int, c_ptr
            INTEGER(c_int), VALUE :: style, width, color
            TYPE(c_ptr) :: CreatePen
        END FUNCTION CreatePen

        FUNCTION SelectObject(hdc, obj) BIND(C, NAME="SelectObject")
            IMPORT :: c_ptr
            TYPE(c_ptr), VALUE :: hdc, obj
            TYPE(c_ptr) :: SelectObject
        END FUNCTION SelectObject

        FUNCTION DeleteObject(obj) BIND(C, NAME="DeleteObject")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: obj
            INTEGER(c_int) :: DeleteObject
        END FUNCTION DeleteObject

        FUNCTION MoveToEx(hdc, x, y, point) BIND(C, NAME="MoveToEx")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: x, y
            TYPE(c_ptr), VALUE :: point
            INTEGER(c_int) :: MoveToEx
        END FUNCTION MoveToEx

        FUNCTION LineTo(hdc, x, y) BIND(C, NAME="LineTo")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: x, y
            INTEGER(c_int) :: LineTo
        END FUNCTION LineTo

        FUNCTION SetTextColor(hdc, color) BIND(C, NAME="SetTextColor")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: color
            INTEGER(c_int) :: SetTextColor
        END FUNCTION SetTextColor

        FUNCTION SetBkMode(hdc, mode) BIND(C, NAME="SetBkMode")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: mode
            INTEGER(c_int) :: SetBkMode
        END FUNCTION SetBkMode

        FUNCTION TextOutA(hdc, x, y, text, text_len) BIND(C, NAME="TextOutA")
            IMPORT :: c_ptr, c_int, c_char
            TYPE(c_ptr), VALUE :: hdc
            INTEGER(c_int), VALUE :: x, y, text_len
            CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: text
            INTEGER(c_int) :: TextOutA
        END FUNCTION TextOutA

        FUNCTION SetWindowTextA(hwnd, text) BIND(C, NAME="SetWindowTextA")
            IMPORT :: c_ptr, c_char, c_int
            TYPE(c_ptr), VALUE :: hwnd
            CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: text
            INTEGER(c_int) :: SetWindowTextA
        END FUNCTION SetWindowTextA

        FUNCTION GetWindowTextA(hwnd, buffer, max_count) BIND(C, NAME="GetWindowTextA")
            IMPORT :: c_ptr, c_char, c_int
            TYPE(c_ptr), VALUE :: hwnd
            CHARACTER(KIND=c_char), DIMENSION(*), INTENT(OUT) :: buffer
            INTEGER(c_int), VALUE :: max_count
            INTEGER(c_int) :: GetWindowTextA
        END FUNCTION GetWindowTextA

        FUNCTION InvalidateRect(hwnd, rect, erase) BIND(C, NAME="InvalidateRect")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, rect
            INTEGER(c_int), VALUE :: erase
            INTEGER(c_int) :: InvalidateRect
        END FUNCTION InvalidateRect

        FUNCTION MessageBoxA(hwnd, text, title, flags) BIND(C, NAME="MessageBoxA")
            IMPORT :: c_ptr, c_char, c_int
            TYPE(c_ptr), VALUE :: hwnd
            CHARACTER(KIND=c_char), DIMENSION(*), INTENT(IN) :: text, title
            INTEGER(c_int), VALUE :: flags
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

        FUNCTION SendMessageA(hwnd, msg, wparam, lparam) BIND(C, NAME="SendMessageA")
            IMPORT :: c_ptr, c_int, c_intptr_t
            TYPE(c_ptr), VALUE :: hwnd
            INTEGER(c_int), VALUE :: msg
            INTEGER(c_intptr_t), VALUE :: wparam, lparam
            INTEGER(c_intptr_t) :: SendMessageA
        END FUNCTION SendMessageA

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

    SUBROUTINE run_book_edit_gui()
        TYPE(WNDCLASSEXA), TARGET :: window_class
        TYPE(MSG) :: message_data
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_name
        CHARACTER(KIND=c_char), DIMENSION(64), TARGET :: window_title
        INTEGER(c_short) :: class_result
        INTEGER(c_int) :: show_result, update_result, translate_result
        INTEGER(c_intptr_t) :: dispatch_result

        CALL init_zobrist_keys()
        CALL init_board(editor_board)
        CALL initialize_gdiplus_and_images()
        gui_instance = GetModuleHandleA(c_null_ptr)
        CALL set_c_string(class_name, 'FortranChessBookEditor')
        CALL set_c_string(window_title, 'Fortran Chess Book Editor')

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
        window_class%lpszClassName = c_loc(class_name)
        window_class%hIconSm = c_null_ptr

        class_result = RegisterClassExA(window_class)
        gui_main_hwnd = CreateWindowExA(0_c_long, c_loc(class_name), c_loc(window_title), WS_OVERLAPPEDWINDOW, &
            CW_USEDEFAULT, CW_USEDEFAULT, WINDOW_WIDTH, WINDOW_HEIGHT, c_null_ptr, c_null_ptr, gui_instance, c_null_ptr)
        IF (.NOT. c_associated(gui_main_hwnd)) RETURN

        show_result = ShowWindow(gui_main_hwnd, SW_SHOWNORMAL)
        update_result = UpdateWindow(gui_main_hwnd)

        DO WHILE (GetMessageA(message_data, c_null_ptr, 0_c_int, 0_c_int) /= 0_c_int)
            translate_result = TranslateMessage(message_data)
            dispatch_result = DispatchMessageA(message_data)
        END DO
    END SUBROUTINE run_book_edit_gui

    RECURSIVE INTEGER(c_intptr_t) FUNCTION window_proc(hwnd, message, w_param, l_param) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: message
        INTEGER(c_intptr_t), VALUE :: w_param, l_param
        TYPE(PAINTSTRUCT) :: paint_struct
        TYPE(c_ptr) :: hdc
        INTEGER(c_int) :: control_id
        INTEGER(c_int) :: end_paint_result, destroy_result

        SELECT CASE (message)
        CASE (WM_CREATE)
            gui_main_hwnd = hwnd
            CALL create_controls()
            CALL update_views()
            window_proc = 0_c_intptr_t
            RETURN
        CASE (WM_COMMAND)
            control_id = loword(w_param)
            SELECT CASE (control_id)
            CASE (IDC_LOAD_BUTTON)
                CALL load_book()
            CASE (IDC_SAVE_BUTTON)
                CALL save_book()
            CASE (IDC_ROOT_BUTTON)
                CALL go_to_root()
            CASE (IDC_BACK_BUTTON)
                CALL go_back()
            CASE (IDC_REMOVE_BUTTON)
                CALL remove_current_branch()
            CASE (IDC_ALL_LINES_MODE_BUTTON)
                show_all_lines_full = .NOT. show_all_lines_full
                CALL update_views()
            END SELECT
            window_proc = 0_c_intptr_t
            RETURN
        CASE (WM_LBUTTONDOWN)
            CALL handle_board_click(loword_signed(l_param), hiword_signed(l_param))
            window_proc = 0_c_intptr_t
            RETURN
        CASE (WM_PAINT)
            hdc = BeginPaint(hwnd, paint_struct)
            CALL draw_editor(hdc)
            end_paint_result = EndPaint(hwnd, paint_struct)
            window_proc = 0_c_intptr_t
            RETURN
        CASE (WM_CLOSE)
            destroy_result = DestroyWindow(hwnd)
            window_proc = 0_c_intptr_t
            RETURN
        CASE (WM_DESTROY)
            CALL release_piece_images()
            IF (gdiplus_started) CALL GdiplusShutdown(gdiplus_token)
            CALL PostQuitMessage(0_c_int)
            window_proc = 0_c_intptr_t
            RETURN
        END SELECT

        window_proc = DefWindowProcA(hwnd, message, w_param, l_param)
    END FUNCTION window_proc

    RECURSIVE INTEGER(c_intptr_t) FUNCTION all_lines_edit_proc(hwnd, msg, wparam, lparam) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: msg
        INTEGER(c_intptr_t), VALUE :: wparam, lparam
        INTEGER(c_intptr_t) :: char_result, line_result
        INTEGER :: clicked_line

        IF (msg == WM_LBUTTONDOWN) THEN
            char_result = SendMessageA(hwnd, EM_CHARFROMPOS, 0_c_intptr_t, lparam)
            line_result = SendMessageA(hwnd, EM_LINEFROMCHAR, char_result, 0_c_intptr_t)
            clicked_line = INT(line_result) + 1
            IF (clicked_line >= 1 .AND. clicked_line <= editor_num_lines) THEN
                CALL go_to_book_line(clicked_line)
            END IF
        END IF

        all_lines_edit_proc = CallWindowProcA(original_all_lines_proc, hwnd, msg, wparam, lparam)
    END FUNCTION all_lines_edit_proc

    SUBROUTINE create_controls()
        TYPE(c_ptr) :: unused_hwnd

        hwnd_path_edit = create_control('EDIT', 'book_white.txt', WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, &
            WS_EX_CLIENTEDGE, PANEL_LEFT, 44_c_int, 320_c_int, 26_c_int, IDC_PATH_EDIT)
        unused_hwnd = create_control('BUTTON', 'Load', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT + 332_c_int, 42_c_int, 72_c_int, 28_c_int, IDC_LOAD_BUTTON)
        unused_hwnd = create_control('BUTTON', 'Save', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT + 410_c_int, 42_c_int, 72_c_int, 28_c_int, IDC_SAVE_BUTTON)
        unused_hwnd = create_control('BUTTON', 'Root', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT, 78_c_int, 72_c_int, 28_c_int, IDC_ROOT_BUTTON)
        unused_hwnd = create_control('BUTTON', 'Back', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT + 78_c_int, 78_c_int, 72_c_int, 28_c_int, IDC_BACK_BUTTON)
        unused_hwnd = create_control('BUTTON', 'Remove Branch', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT + 156_c_int, 78_c_int, 128_c_int, 28_c_int, IDC_REMOVE_BUTTON)
        unused_hwnd = create_control('STATIC', 'Book Path', WS_CHILD + WS_VISIBLE, 0_c_long, &
            PANEL_LEFT, 24_c_int, 100_c_int, 18_c_int, 0_c_int)
        unused_hwnd = create_control('STATIC', 'Current Line', WS_CHILD + WS_VISIBLE, 0_c_long, &
            PANEL_LEFT, 122_c_int, 100_c_int, 18_c_int, 0_c_int)
        unused_hwnd = create_control('STATIC', 'Continuations', WS_CHILD + WS_VISIBLE, 0_c_long, &
            PANEL_LEFT, 176_c_int, 120_c_int, 18_c_int, 0_c_int)
        unused_hwnd = create_control('STATIC', 'All Book Lines', WS_CHILD + WS_VISIBLE, 0_c_long, &
            PANEL_LEFT, 366_c_int, 120_c_int, 18_c_int, 0_c_int)
        hwnd_all_lines_mode_button = create_control('BUTTON', 'Full', WS_CHILD + WS_VISIBLE + WS_TABSTOP, 0_c_long, &
            PANEL_LEFT + 390_c_int, 362_c_int, 92_c_int, 24_c_int, IDC_ALL_LINES_MODE_BUTTON)

        hwnd_current_line = create_control('EDIT', '', WS_CHILD + WS_VISIBLE + WS_BORDER + ES_AUTOHSCROLL + ES_READONLY, &
            WS_EX_CLIENTEDGE, PANEL_LEFT, 142_c_int, 492_c_int, 26_c_int, IDC_CURRENT_LINE_EDIT)
        hwnd_continuations = create_control('EDIT', '', WS_CHILD + WS_VISIBLE + WS_BORDER + WS_VSCROLL + ES_MULTILINE + &
            ES_AUTOVSCROLL + ES_READONLY, WS_EX_CLIENTEDGE, PANEL_LEFT, 196_c_int, 492_c_int, 156_c_int, IDC_CONT_EDIT)
        hwnd_all_lines = create_control('EDIT', '', WS_CHILD + WS_VISIBLE + WS_BORDER + WS_VSCROLL + WS_HSCROLL + &
            ES_MULTILINE + ES_AUTOVSCROLL + ES_AUTOHSCROLL + ES_READONLY, WS_EX_CLIENTEDGE, PANEL_LEFT, 386_c_int, 492_c_int, &
            282_c_int, IDC_ALL_LINES_EDIT)
        original_all_lines_proc = SetWindowLongPtrA(hwnd_all_lines, GWL_WNDPROC, funptr_to_intptr(c_funloc(all_lines_edit_proc)))
        hwnd_status_static = create_control('STATIC', status_text, WS_CHILD + WS_VISIBLE, 0_c_long, &
            24_c_int, 630_c_int, 1010_c_int, 24_c_int, IDC_STATUS_STATIC)
    END SUBROUTINE create_controls

    RECURSIVE SUBROUTINE load_book()
        TYPE(Opening_Book_Type) :: book
        CHARACTER(LEN=MAX_PATH_TEXT) :: filename
        INTEGER :: i

        filename = get_control_text(hwnd_path_edit)
        IF (LEN_TRIM(filename) == 0) THEN
            CALL show_message_box('Enter a book file path first.', 'Book Editor', MB_OK + MB_ICONERROR)
            RETURN
        END IF

        CALL load_opening_book(TRIM(filename), book)
        IF (.NOT. book%found) THEN
            CALL show_message_box('Book file not found.', 'Book Editor', MB_OK + MB_ICONERROR)
            CALL update_status('Book file not found.')
            RETURN
        END IF

        IF (.NOT. book%valid) THEN
            CALL show_message_box(build_book_error_message(book), 'Book Editor', MB_OK + MB_ICONERROR)
            CALL update_status('Book failed validation. Fix it or load another file.')
            RETURN
        END IF

        CALL load_preserved_lines(TRIM(filename))
        editor_num_lines = book%num_lines
        editor_line_lengths = 0
        editor_lines = ''
        DO i = 1, editor_num_lines
            editor_line_lengths(i) = book%line_lengths(i)
            editor_lines(1:editor_line_lengths(i), i) = book%lines(1:editor_line_lengths(i), i)
        END DO
        CALL canonicalize_book_lines()

        current_prefix_len = 0
        current_prefix = ''
        selected_from_rank = 0
        selected_from_file = 0
        CALL rebuild_board_from_prefix()
        CALL update_status('Loaded opening book with ' // trim(integer_to_text(editor_num_lines)) // ' line(s).')
        CALL update_views()
    END SUBROUTINE load_book

    RECURSIVE SUBROUTINE save_book()
        INTEGER :: unit_no, ios
        CHARACTER(LEN=MAX_PATH_TEXT) :: filename

        filename = get_control_text(hwnd_path_edit)
        IF (LEN_TRIM(filename) == 0) THEN
            CALL show_message_box('Enter a file path before saving.', 'Book Editor', MB_OK + MB_ICONERROR)
            RETURN
        END IF

        unit_no = 71
        OPEN(unit=unit_no, file=TRIM(filename), status='REPLACE', action='WRITE', iostat=ios)
        IF (ios /= 0) THEN
            CALL show_message_box('Could not save the book file.', 'Book Editor', MB_OK + MB_ICONERROR)
            CALL update_status('Save failed.')
            RETURN
        END IF

        CALL canonicalize_book_lines()

        CALL write_preserved_lines_and_book(unit_no)
        CLOSE(unit_no)

        CALL update_status('Saved ' // trim(integer_to_text(editor_num_lines)) // ' line(s) to ' // TRIM(filename) // '.')
        CALL update_views()
    END SUBROUTINE save_book

    RECURSIVE SUBROUTINE go_to_root()
        current_prefix_len = 0
        current_prefix = ''
        selected_from_rank = 0
        selected_from_file = 0
        CALL rebuild_board_from_prefix()
        CALL update_status('Returned to the root position.')
        CALL update_views()
    END SUBROUTINE go_to_root

    RECURSIVE SUBROUTINE go_back()
        IF (current_prefix_len <= 0) THEN
            CALL update_status('Already at the root position.')
            CALL update_views()
            RETURN
        END IF

        current_prefix(current_prefix_len) = ''
        current_prefix_len = current_prefix_len - 1
        selected_from_rank = 0
        selected_from_file = 0
        CALL rebuild_board_from_prefix()
        CALL update_status('Moved back one ply.')
        CALL update_views()
    END SUBROUTINE go_back

    RECURSIVE SUBROUTINE go_to_book_line(line_index)
        INTEGER, INTENT(IN) :: line_index
        INTEGER :: i

        IF (line_index < 1 .OR. line_index > editor_num_lines) RETURN

        current_prefix_len = editor_line_lengths(line_index)
        current_prefix = ''
        DO i = 1, current_prefix_len
            current_prefix(i) = editor_lines(i, line_index)
        END DO
        selected_from_rank = 0
        selected_from_file = 0
        CALL rebuild_board_from_prefix()
        CALL update_status('Loaded line ' // TRIM(integer_to_text(line_index)) // ' on the board for extension.')
        CALL update_views()
    END SUBROUTINE go_to_book_line

    RECURSIVE SUBROUTINE remove_current_branch()
        INTEGER :: old_line_count, i, j, kept

        IF (current_prefix_len <= 0) THEN
            CALL update_status('Select a move branch first.')
            CALL update_views()
            RETURN
        END IF

        old_line_count = editor_num_lines
        kept = 0
        DO i = 1, editor_num_lines
            IF (line_matches_prefix(i, current_prefix_len)) CYCLE
            kept = kept + 1
            editor_line_lengths(kept) = editor_line_lengths(i)
            editor_lines(:, kept) = editor_lines(:, i)
        END DO

        DO i = kept + 1, old_line_count
            editor_line_lengths(i) = 0
            DO j = 1, MAX_BOOK_MOVES_LOCAL
                editor_lines(j, i) = ''
            END DO
        END DO
        editor_num_lines = kept

        current_prefix(current_prefix_len) = ''
        current_prefix_len = current_prefix_len - 1
        selected_from_rank = 0
        selected_from_file = 0
        CALL rebuild_board_from_prefix()
        CALL update_status('Removed branch and its continuations.')
        CALL update_views()
    END SUBROUTINE remove_current_branch

    RECURSIVE SUBROUTINE update_views()
        CALL set_control_text(hwnd_current_line, format_current_line_text())
        CALL set_control_text(hwnd_continuations, format_continuations_text())
        CALL set_control_text(hwnd_all_lines, format_all_lines_text())
        CALL set_control_text(hwnd_status_static, status_text)
        IF (show_all_lines_full) THEN
            CALL set_control_text(hwnd_all_lines_mode_button, 'Full')
        ELSE
            CALL set_control_text(hwnd_all_lines_mode_button, 'Compact')
        END IF
        CALL invalidate_main_window()
    END SUBROUTINE update_views

    RECURSIVE SUBROUTINE rebuild_board_from_prefix()
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: num_legal_moves, i, move_index

        CALL init_board(editor_board)
        selected_from_rank = 0
        selected_from_file = 0

        DO i = 1, current_prefix_len
            CALL generate_moves(editor_board, legal_moves, num_legal_moves)
            move_index = find_matching_move(editor_board, legal_moves, num_legal_moves, current_prefix(i))
            IF (move_index <= 0) THEN
                current_prefix_len = i - 1
                CALL update_status('Could not replay move ' // TRIM(current_prefix(i)) // '.')
                RETURN
            END IF
            CALL make_move(editor_board, legal_moves(move_index), unmake_info)
        END DO
    END SUBROUTINE rebuild_board_from_prefix

    RECURSIVE SUBROUTINE handle_board_click(mouse_x, mouse_y)
        INTEGER, INTENT(IN) :: mouse_x, mouse_y
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        INTEGER :: num_legal_moves, rank, file, i, chosen_idx
        CHARACTER(LEN=32) :: move_token

        IF (.NOT. board_square_from_click(mouse_x, mouse_y, rank, file)) RETURN

        IF (selected_from_rank == 0) THEN
            IF (editor_board%squares_color(rank, file) /= editor_board%current_player) THEN
                CALL update_status('Select a piece for the side to move.')
                CALL update_views()
                RETURN
            END IF
            selected_from_rank = rank
            selected_from_file = file
            CALL update_status('Selected ' // square_name(rank, file) // '.')
            CALL update_views()
            RETURN
        END IF

        IF (selected_from_rank == rank .AND. selected_from_file == file) THEN
            selected_from_rank = 0
            selected_from_file = 0
            CALL update_status('Move selection cleared.')
            CALL update_views()
            RETURN
        END IF

        CALL generate_moves(editor_board, legal_moves, num_legal_moves)
        chosen_idx = 0
        DO i = 1, num_legal_moves
            IF (legal_moves(i)%from_sq%rank /= selected_from_rank) CYCLE
            IF (legal_moves(i)%from_sq%file /= selected_from_file) CYCLE
            IF (legal_moves(i)%to_sq%rank /= rank) CYCLE
            IF (legal_moves(i)%to_sq%file /= file) CYCLE
            chosen_idx = i
            IF (legal_moves(i)%promotion_piece == QUEEN) EXIT
        END DO

        IF (chosen_idx <= 0) THEN
            IF (editor_board%squares_color(rank, file) == editor_board%current_player) THEN
                selected_from_rank = rank
                selected_from_file = file
                CALL update_status('Selected ' // square_name(rank, file) // '.')
            ELSE
                selected_from_rank = 0
                selected_from_file = 0
                CALL update_status('That is not a legal move.')
            END IF
            CALL update_views()
            RETURN
        END IF

        move_token = normalize_move_to_book(editor_board, legal_moves(chosen_idx), legal_moves, num_legal_moves)
        selected_from_rank = 0
        selected_from_file = 0

        IF (.NOT. continuation_exists(move_token)) THEN
            CALL add_continuation(move_token)
            CALL update_status('Added ' // TRIM(format_continuation_token(current_prefix_len + 1, &
                san_for_book_token(editor_board, move_token))) // ' to the book.')
        ELSE
            CALL update_status('Moved into existing continuation ' // TRIM(format_continuation_token(current_prefix_len + 1, &
                san_for_book_token(editor_board, move_token))) // '.')
        END IF

        current_prefix_len = current_prefix_len + 1
        current_prefix(current_prefix_len) = move_token
        CALL rebuild_board_from_prefix()
        CALL update_views()
    END SUBROUTINE handle_board_click

    LOGICAL FUNCTION continuation_exists(move_token)
        CHARACTER(LEN=*), INTENT(IN) :: move_token
        INTEGER :: i

        continuation_exists = .FALSE.
        DO i = 1, editor_num_lines
            IF (.NOT. line_matches_prefix(i, current_prefix_len)) CYCLE
            IF (editor_line_lengths(i) <= current_prefix_len) CYCLE
            IF (TRIM(editor_lines(current_prefix_len + 1, i)) == TRIM(move_token)) THEN
                continuation_exists = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION continuation_exists

    SUBROUTINE add_continuation(move_token)
        CHARACTER(LEN=*), INTENT(IN) :: move_token
        INTEGER :: i

        IF (editor_num_lines >= MAX_BOOK_LINES_LOCAL) THEN
            CALL show_message_box('The editor is full. Increase MAX_BOOK_LINES_LOCAL to store more lines.', &
                'Book Editor', MB_OK + MB_ICONERROR)
            RETURN
        END IF
        IF (current_prefix_len >= MAX_BOOK_MOVES_LOCAL) THEN
            CALL show_message_box('The line is too long for the editor.', 'Book Editor', MB_OK + MB_ICONERROR)
            RETURN
        END IF

        editor_num_lines = editor_num_lines + 1
        editor_line_lengths(editor_num_lines) = current_prefix_len + 1
        editor_lines(:, editor_num_lines) = ''
        DO i = 1, current_prefix_len
            editor_lines(i, editor_num_lines) = current_prefix(i)
        END DO
        editor_lines(current_prefix_len + 1, editor_num_lines) = move_token
        CALL canonicalize_book_lines()
    END SUBROUTINE add_continuation

    LOGICAL FUNCTION line_matches_prefix(line_index, prefix_len)
        INTEGER, INTENT(IN) :: line_index, prefix_len
        INTEGER :: i

        IF (prefix_len == 0) THEN
            line_matches_prefix = .TRUE.
            RETURN
        END IF
        IF (editor_line_lengths(line_index) < prefix_len) THEN
            line_matches_prefix = .FALSE.
            RETURN
        END IF

        line_matches_prefix = .TRUE.
        DO i = 1, prefix_len
            IF (TRIM(editor_lines(i, line_index)) /= TRIM(current_prefix(i))) THEN
                line_matches_prefix = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION line_matches_prefix

    SUBROUTINE canonicalize_book_lines()
        INTEGER :: i, j, kept
        LOGICAL :: remove_line

        IF (editor_num_lines <= 1) RETURN

        kept = 0
        DO i = 1, editor_num_lines
            remove_line = .FALSE.
            DO j = 1, editor_num_lines
                IF (i == j) CYCLE
                IF (lines_identical(i, j)) THEN
                    IF (j < i) THEN
                        remove_line = .TRUE.
                        EXIT
                    END IF
                ELSE IF (line_is_proper_prefix(i, j)) THEN
                    remove_line = .TRUE.
                    EXIT
                END IF
            END DO
            IF (remove_line) CYCLE

            kept = kept + 1
            IF (kept /= i) THEN
                editor_line_lengths(kept) = editor_line_lengths(i)
                editor_lines(:, kept) = editor_lines(:, i)
            END IF
        END DO

        DO i = kept + 1, editor_num_lines
            editor_line_lengths(i) = 0
            editor_lines(:, i) = ''
        END DO
        editor_num_lines = kept
    END SUBROUTINE canonicalize_book_lines

    SUBROUTINE load_preserved_lines(filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER :: unit_no, ios, book_lines_seen
        CHARACTER(LEN=512) :: raw_line, trimmed_line

        preserved_line_count = 0
        preserved_line_anchor = 1
        preserved_lines = ''

        unit_no = 72
        OPEN(unit=unit_no, file=TRIM(filename), status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) RETURN

        book_lines_seen = 0
        DO
            READ(unit_no, '(A)', IOSTAT=ios) raw_line
            IF (ios /= 0) EXIT
            trimmed_line = ADJUSTL(TRIM(raw_line))
            IF (LEN_TRIM(trimmed_line) == 0 .OR. trimmed_line(1:1) == '!' .OR. trimmed_line(1:1) == '#') THEN
                IF (preserved_line_count < MAX_PRESERVED_LINES_LOCAL) THEN
                    preserved_line_count = preserved_line_count + 1
                    preserved_line_anchor(preserved_line_count) = book_lines_seen + 1
                    preserved_lines(preserved_line_count) = TRIM(raw_line)
                END IF
            ELSE
                book_lines_seen = book_lines_seen + 1
            END IF
        END DO
        CLOSE(unit_no)
    END SUBROUTINE load_preserved_lines

    SUBROUTINE write_preserved_lines_and_book(unit_no)
        INTEGER, INTENT(IN) :: unit_no
        INTEGER :: i, j
        CHARACTER(LEN=1024) :: line_text
        IF (editor_num_lines <= 0) THEN
            DO j = 1, preserved_line_count
                WRITE(unit_no, '(A)') TRIM(preserved_lines(j))
            END DO
            RETURN
        END IF

        CALL write_preserved_group(unit_no, 1)
        line_text = format_compact_sequence_text(editor_lines(:, 1), editor_line_lengths(1), editor_lines(:, 1), 0)
        WRITE(unit_no, '(A)') TRIM(line_text)

        DO i = 2, editor_num_lines
            CALL write_preserved_group(unit_no, i)
            line_text = format_compact_sequence_text(editor_lines(:, i), editor_line_lengths(i), &
                editor_lines(:, i - 1), editor_line_lengths(i - 1))
            WRITE(unit_no, '(A)') TRIM(line_text)
        END DO

        CALL write_preserved_group(unit_no, editor_num_lines + 1)
    END SUBROUTINE write_preserved_lines_and_book

    SUBROUTINE write_preserved_group(unit_no, anchor_index)
        INTEGER, INTENT(IN) :: unit_no, anchor_index
        INTEGER :: j, anchor

        DO j = 1, preserved_line_count
            anchor = MIN(MAX(1, preserved_line_anchor(j)), editor_num_lines + 1)
            IF (anchor == anchor_index) WRITE(unit_no, '(A)') TRIM(preserved_lines(j))
        END DO
    END SUBROUTINE write_preserved_group

    LOGICAL FUNCTION lines_identical(left_idx, right_idx)
        INTEGER, INTENT(IN) :: left_idx, right_idx
        INTEGER :: i

        IF (editor_line_lengths(left_idx) /= editor_line_lengths(right_idx)) THEN
            lines_identical = .FALSE.
            RETURN
        END IF

        lines_identical = .TRUE.
        DO i = 1, editor_line_lengths(left_idx)
            IF (TRIM(editor_lines(i, left_idx)) /= TRIM(editor_lines(i, right_idx))) THEN
                lines_identical = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION lines_identical

    LOGICAL FUNCTION line_is_proper_prefix(prefix_idx, full_idx)
        INTEGER, INTENT(IN) :: prefix_idx, full_idx
        INTEGER :: i

        IF (editor_line_lengths(prefix_idx) >= editor_line_lengths(full_idx)) THEN
            line_is_proper_prefix = .FALSE.
            RETURN
        END IF

        line_is_proper_prefix = .TRUE.
        DO i = 1, editor_line_lengths(prefix_idx)
            IF (TRIM(editor_lines(i, prefix_idx)) /= TRIM(editor_lines(i, full_idx))) THEN
                line_is_proper_prefix = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION line_is_proper_prefix

    INTEGER FUNCTION find_matching_move(board, legal_moves, num_legal_moves, move_text)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        INTEGER :: i

        find_matching_move = 0
        DO i = 1, num_legal_moves
            IF (move_matches_input(board, legal_moves(i), legal_moves, num_legal_moves, move_text)) THEN
                find_matching_move = i
                RETURN
            END IF
        END DO
    END FUNCTION find_matching_move

    RECURSIVE SUBROUTINE draw_editor(hdc)
        TYPE(c_ptr), INTENT(IN), VALUE :: hdc
        TYPE(RECT) :: square_rect
        TYPE(c_ptr) :: brush, pen, old_pen, graphics
        INTEGER(c_int) :: fill_result, delete_result, ignored_int
        INTEGER :: rank, file, x, y, piece, color
        LOGICAL :: light_square

        graphics = c_null_ptr
        IF (gdiplus_started) ignored_int = GdipCreateFromHDC(hdc, graphics)

        DO rank = 8, 1, -1
            y = BOARD_TOP + (8 - rank) * SQUARE_PIXELS
            DO file = 1, 8
                x = BOARD_LEFT + (file - 1) * SQUARE_PIXELS
                light_square = MOD(rank + file, 2) == 1
                square_rect%left = x
                square_rect%top = y
                square_rect%right = x + SQUARE_PIXELS
                square_rect%bottom = y + SQUARE_PIXELS
                IF (light_square) THEN
                    brush = CreateSolidBrush(rgb_color(240, 217, 181))
                ELSE
                    brush = CreateSolidBrush(rgb_color(181, 136, 99))
                END IF
                fill_result = FillRect(hdc, square_rect, brush)
                delete_result = DeleteObject(brush)

                IF (selected_from_rank == rank .AND. selected_from_file == file) THEN
                    pen = CreatePen(PS_SOLID, 3_c_int, rgb_color(200, 40, 40))
                    old_pen = SelectObject(hdc, pen)
                    CALL draw_rectangle_outline(hdc, x, y, SQUARE_PIXELS, SQUARE_PIXELS)
                    old_pen = SelectObject(hdc, old_pen)
                    delete_result = DeleteObject(pen)
                END IF

                piece = editor_board%squares_piece(rank, file)
                color = editor_board%squares_color(rank, file)
                IF (piece /= NO_PIECE) THEN
                    IF (C_ASSOCIATED(graphics) .AND. C_ASSOCIATED(piece_images(merge(1, 2, color == WHITE), piece))) THEN
                        ignored_int = GdipDrawImageRectI(graphics, piece_images(merge(1, 2, color == WHITE), piece), &
                            INT(x + 6, c_int), INT(y + 6, c_int), SQUARE_PIXELS - 12_c_int, SQUARE_PIXELS - 12_c_int)
                    ELSE
                        CALL draw_text(hdc, x + 24, y + 20, piece_symbol(piece, color), rgb_color(20, 20, 20))
                    END IF
                END IF
            END DO
        END DO

        IF (C_ASSOCIATED(graphics)) ignored_int = GdipDeleteGraphics(graphics)

        DO file = 1, 8
            CALL draw_text(hdc, BOARD_LEFT + (file - 1) * SQUARE_PIXELS + 26, BOARD_TOP + BOARD_PIXELS + 6, &
                file_label(file), rgb_color(30, 30, 30))
        END DO
        DO rank = 8, 1, -1
            CALL draw_text(hdc, BOARD_LEFT - 16, BOARD_TOP + (8 - rank) * SQUARE_PIXELS + 24, rank_label(rank), &
                rgb_color(30, 30, 30))
        END DO
        CALL draw_text(hdc, BOARD_LEFT, 58, 'Click source square, then destination square, to add or follow a book move.', &
            rgb_color(20, 20, 20))
    END SUBROUTINE draw_editor

    CHARACTER(LEN=MAX_CONTROL_TEXT) FUNCTION format_current_line_text()
        IF (current_prefix_len <= 0) THEN
            format_current_line_text = '(root position)'
        ELSE
            format_current_line_text = format_sequence_text(current_prefix, current_prefix_len, .FALSE.)
        END IF
    END FUNCTION format_current_line_text

    CHARACTER(LEN=MAX_CONTROL_TEXT) FUNCTION format_continuations_text()
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        CHARACTER(LEN=32), DIMENSION(MAX_MOVES) :: unique_moves
        INTEGER :: num_legal_moves, unique_count, i, j, used
        LOGICAL :: already_seen

        format_continuations_text = ''
        unique_moves = ''
        unique_count = 0

        DO i = 1, editor_num_lines
            IF (.NOT. line_matches_prefix(i, current_prefix_len)) CYCLE
            IF (editor_line_lengths(i) <= current_prefix_len) CYCLE
            already_seen = .FALSE.
            DO j = 1, unique_count
                IF (TRIM(unique_moves(j)) == TRIM(editor_lines(current_prefix_len + 1, i))) THEN
                    already_seen = .TRUE.
                    EXIT
                END IF
            END DO
            IF (.NOT. already_seen .AND. unique_count < MAX_MOVES) THEN
                unique_count = unique_count + 1
                unique_moves(unique_count) = editor_lines(current_prefix_len + 1, i)
            END IF
        END DO

        IF (unique_count == 0) THEN
            format_continuations_text = '(no continuations from this position)'
            RETURN
        END IF

        used = 0
        CALL generate_moves(editor_board, legal_moves, num_legal_moves)
        DO i = 1, unique_count
            IF (i > 1) CALL append_fragment(format_continuations_text, used, windows_newline())
            CALL append_fragment(format_continuations_text, used, TRIM(format_continuation_token(current_prefix_len + 1, &
                san_for_book_token(editor_board, unique_moves(i)))))
        END DO
    END FUNCTION format_continuations_text

    CHARACTER(LEN=MAX_CONTROL_TEXT) FUNCTION format_all_lines_text()
        INTEGER :: i, used

        format_all_lines_text = ''
        IF (editor_num_lines == 0) THEN
            format_all_lines_text = '(book is empty)'
            RETURN
        END IF

        used = 0
        CALL append_fragment(format_all_lines_text, used, &
            TRIM(format_sequence_text(editor_lines(:, 1), editor_line_lengths(1), .FALSE.)))
        DO i = 2, editor_num_lines
            CALL append_fragment(format_all_lines_text, used, windows_newline())
            IF (show_all_lines_full) THEN
                CALL append_fragment(format_all_lines_text, used, &
                    TRIM(format_sequence_text(editor_lines(:, i), editor_line_lengths(i), .FALSE.)))
            ELSE
                CALL append_fragment(format_all_lines_text, used, &
                    TRIM(format_compact_sequence_text(editor_lines(:, i), editor_line_lengths(i), &
                    editor_lines(:, i - 1), editor_line_lengths(i - 1))))
            END IF
        END DO
    END FUNCTION format_all_lines_text

    CHARACTER(LEN=1024) FUNCTION format_sequence_text(sequence, sequence_len, use_black_ellipsis)
        CHARACTER(LEN=32), DIMENSION(:), INTENT(IN) :: sequence
        INTEGER, INTENT(IN) :: sequence_len
        LOGICAL, INTENT(IN) :: use_black_ellipsis
        TYPE(Board_Type) :: temp_board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: num_legal_moves, used, i, move_index
        CHARACTER(LEN=32) :: display_move

        format_sequence_text = ''
        IF (sequence_len <= 0) RETURN

        used = 0
        CALL init_board(temp_board)
        DO i = 1, sequence_len
            CALL generate_moves(temp_board, legal_moves, num_legal_moves)
            move_index = find_matching_move(temp_board, legal_moves, num_legal_moves, sequence(i))
            IF (move_index > 0) THEN
                display_move = TRIM(move_to_san(temp_board, legal_moves(move_index), legal_moves, num_legal_moves))
                CALL make_move(temp_board, legal_moves(move_index), unmake_info)
            ELSE
                display_move = TRIM(sequence(i))
            END IF
            IF (i > 1) CALL append_fragment(format_sequence_text, used, ' ')
            CALL append_fragment(format_sequence_text, used, TRIM(format_halfmove_token(i, display_move, use_black_ellipsis)))
        END DO
    END FUNCTION format_sequence_text

    CHARACTER(LEN=1024) FUNCTION format_compact_sequence_text(sequence, sequence_len, previous_sequence, previous_len)
        CHARACTER(LEN=32), DIMENSION(:), INTENT(IN) :: sequence, previous_sequence
        INTEGER, INTENT(IN) :: sequence_len, previous_len
        TYPE(Board_Type) :: temp_board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: num_legal_moves, used, i, move_index, start_index, common_prefix
        CHARACTER(LEN=32) :: display_move

        format_compact_sequence_text = ''
        IF (sequence_len <= 0) RETURN

        common_prefix = 0
        DO i = 1, MIN(sequence_len, previous_len)
            IF (TRIM(sequence(i)) /= TRIM(previous_sequence(i))) EXIT
            common_prefix = i
        END DO
        start_index = common_prefix + 1
        IF (start_index > sequence_len) start_index = sequence_len

        used = 0
        CALL init_board(temp_board)
        DO i = 1, sequence_len
            CALL generate_moves(temp_board, legal_moves, num_legal_moves)
            move_index = find_matching_move(temp_board, legal_moves, num_legal_moves, sequence(i))
            IF (move_index > 0) THEN
                display_move = TRIM(move_to_san(temp_board, legal_moves(move_index), legal_moves, num_legal_moves))
                CALL make_move(temp_board, legal_moves(move_index), unmake_info)
            ELSE
                display_move = TRIM(sequence(i))
            END IF
            IF (i < start_index) CYCLE
            IF (used > 0) CALL append_fragment(format_compact_sequence_text, used, ' ')
            IF (i == start_index) THEN
                CALL append_fragment(format_compact_sequence_text, used, TRIM(format_halfmove_token(i, display_move, .TRUE.)))
            ELSE
                CALL append_fragment(format_compact_sequence_text, used, TRIM(format_halfmove_token(i, display_move, .FALSE.)))
            END IF
        END DO
    END FUNCTION format_compact_sequence_text

    CHARACTER(LEN=64) FUNCTION format_continuation_token(halfmove_index, move_text)
        INTEGER, INTENT(IN) :: halfmove_index
        CHARACTER(LEN=*), INTENT(IN) :: move_text

        format_continuation_token = format_halfmove_token(halfmove_index, move_text, .TRUE.)
    END FUNCTION format_continuation_token

    CHARACTER(LEN=64) FUNCTION format_halfmove_token(halfmove_index, move_text, use_black_ellipsis)
        INTEGER, INTENT(IN) :: halfmove_index
        CHARACTER(LEN=*), INTENT(IN) :: move_text
        LOGICAL, INTENT(IN) :: use_black_ellipsis
        CHARACTER(LEN=16) :: prefix

        prefix = ''
        WRITE(prefix, '(I0)') (halfmove_index + 1) / 2
        IF (MOD(halfmove_index, 2) == 1) THEN
            format_halfmove_token = TRIM(prefix) // '.' // TRIM(move_text)
        ELSE IF (use_black_ellipsis) THEN
            format_halfmove_token = TRIM(prefix) // '...' // TRIM(move_text)
        ELSE
            format_halfmove_token = TRIM(move_text)
        END IF
    END FUNCTION format_halfmove_token

    CHARACTER(LEN=32) FUNCTION san_for_book_token(board, move_token)
        TYPE(Board_Type), INTENT(IN) :: board
        CHARACTER(LEN=*), INTENT(IN) :: move_token
        TYPE(Board_Type) :: temp_board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        INTEGER :: num_legal_moves, move_index

        temp_board = board
        CALL generate_moves(temp_board, legal_moves, num_legal_moves)
        move_index = find_matching_move(temp_board, legal_moves, num_legal_moves, move_token)
        IF (move_index > 0) THEN
            san_for_book_token = TRIM(move_to_san(temp_board, legal_moves(move_index), legal_moves, num_legal_moves))
        ELSE
            san_for_book_token = TRIM(move_token)
        END IF
    END FUNCTION san_for_book_token

    CHARACTER(LEN=32) FUNCTION normalize_move_to_book(board, mv, legal_moves, num_legal_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), INTENT(IN) :: mv
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves

        normalize_move_to_book = lowercase_alnum(move_to_san(board, mv, legal_moves, num_legal_moves))
    END FUNCTION normalize_move_to_book

    CHARACTER(LEN=32) FUNCTION lowercase_alnum(text)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER :: i, code, out_pos
        CHARACTER(LEN=64) :: lowered

        lowered = ADJUSTL(TRIM(text))
        DO i = 1, LEN_TRIM(lowered)
            IF (lowered(i:i) == '0') lowered(i:i) = 'O'
            code = IACHAR(lowered(i:i))
            IF (code >= IACHAR('A') .AND. code <= IACHAR('Z')) lowered(i:i) = ACHAR(code + 32)
        END DO

        lowercase_alnum = ''
        out_pos = 0
        DO i = 1, LEN_TRIM(lowered)
            SELECT CASE (lowered(i:i))
            CASE ('x', '+', '#', '=', '!', '?', ' ')
                CYCLE
            CASE DEFAULT
                out_pos = out_pos + 1
                IF (out_pos <= LEN(lowercase_alnum)) lowercase_alnum(out_pos:out_pos) = lowered(i:i)
            END SELECT
        END DO
    END FUNCTION lowercase_alnum

    CHARACTER(LEN=MAX_STATUS_TEXT) FUNCTION build_book_error_message(book)
        TYPE(Opening_Book_Type), INTENT(IN) :: book
        INTEGER :: used

        build_book_error_message = ''
        used = 0
        CALL append_fragment(build_book_error_message, used, 'Book validation failed at line ')
        CALL append_fragment(build_book_error_message, used, TRIM(integer_to_text(book%error_line)))
        CALL append_fragment(build_book_error_message, used, ':' // windows_newline())
        CALL append_fragment(build_book_error_message, used, TRIM(book%error_message))
        IF (LEN_TRIM(book%error_text) > 0) THEN
            CALL append_fragment(build_book_error_message, used, windows_newline() // 'Bad line: ' // TRIM(book%error_text))
        END IF
        IF (LEN_TRIM(book%replacement_line) > 0) THEN
            CALL append_fragment(build_book_error_message, used, windows_newline() // 'Possible replacement: ' // &
                TRIM(book%replacement_line))
        END IF
    END FUNCTION build_book_error_message

    LOGICAL FUNCTION board_square_from_click(mouse_x, mouse_y, rank, file)
        INTEGER, INTENT(IN) :: mouse_x, mouse_y
        INTEGER, INTENT(OUT) :: rank, file

        board_square_from_click = .FALSE.
        rank = 0
        file = 0
        IF (mouse_x < BOARD_LEFT .OR. mouse_x >= BOARD_LEFT + BOARD_PIXELS) RETURN
        IF (mouse_y < BOARD_TOP .OR. mouse_y >= BOARD_TOP + BOARD_PIXELS) RETURN

        file = 1 + (mouse_x - BOARD_LEFT) / SQUARE_PIXELS
        rank = 8 - (mouse_y - BOARD_TOP) / SQUARE_PIXELS
        board_square_from_click = .TRUE.
    END FUNCTION board_square_from_click

    SUBROUTINE update_status(text)
        CHARACTER(LEN=*), INTENT(IN) :: text

        status_text = text
    END SUBROUTINE update_status

    FUNCTION create_control(class_name, text, style, ex_style, x, y, width, height, control_id) RESULT(hwnd)
        CHARACTER(LEN=*), INTENT(IN) :: class_name, text
        INTEGER(c_long), INTENT(IN) :: style, ex_style
        INTEGER(c_int), INTENT(IN) :: x, y, width, height, control_id
        TYPE(c_ptr) :: hwnd
        CHARACTER(KIND=c_char), DIMENSION(64), TARGET :: class_buffer
        CHARACTER(KIND=c_char), DIMENSION(:), ALLOCATABLE, TARGET :: text_buffer

        CALL set_c_string(class_buffer, class_name)
        ALLOCATE(text_buffer(MAX(2, LEN_TRIM(text) + 1)))
        CALL set_c_string(text_buffer, text)
        hwnd = CreateWindowExA(ex_style, c_loc(class_buffer), c_loc(text_buffer), style, x, y, width, height, &
            gui_main_hwnd, ptr_from_intptr(INT(control_id, c_intptr_t)), gui_instance, c_null_ptr)
        DEALLOCATE(text_buffer)
    END FUNCTION create_control

    SUBROUTINE set_control_text(hwnd, text)
        TYPE(c_ptr), INTENT(IN), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(:), ALLOCATABLE, TARGET :: buffer
        INTEGER(c_int) :: set_result

        IF (.NOT. c_associated(hwnd)) RETURN
        ALLOCATE(buffer(MAX(2, LEN_TRIM(text) + 1)))
        CALL set_c_string(buffer, text)
        set_result = SetWindowTextA(hwnd, buffer)
        DEALLOCATE(buffer)
    END SUBROUTINE set_control_text

    FUNCTION get_control_text(hwnd) RESULT(text)
        TYPE(c_ptr), INTENT(IN), VALUE :: hwnd
        CHARACTER(LEN=MAX_PATH_TEXT) :: text
        CHARACTER(KIND=c_char), DIMENSION(MAX_PATH_TEXT), TARGET :: buffer
        INTEGER(c_int) :: copied, i

        text = ''
        IF (.NOT. c_associated(hwnd)) RETURN
        copied = GetWindowTextA(hwnd, buffer, MAX_PATH_TEXT - 1)
        DO i = 1, MIN(INT(copied), LEN(text))
            text(i:i) = CHAR(IACHAR(buffer(i)))
        END DO
    END FUNCTION get_control_text

    SUBROUTINE show_message_box(message_text, title_text, flags)
        CHARACTER(LEN=*), INTENT(IN) :: message_text, title_text
        INTEGER(c_int), INTENT(IN) :: flags
        CHARACTER(KIND=c_char), DIMENSION(MAX_STATUS_TEXT), TARGET :: message_buffer, title_buffer
        INTEGER(c_int) :: message_result

        CALL set_c_string(message_buffer, message_text)
        CALL set_c_string(title_buffer, title_text)
        message_result = MessageBoxA(gui_main_hwnd, message_buffer, title_buffer, flags)
    END SUBROUTINE show_message_box

    SUBROUTINE invalidate_main_window()
        INTEGER(c_int) :: invalidate_result

        IF (c_associated(gui_main_hwnd)) invalidate_result = InvalidateRect(gui_main_hwnd, c_null_ptr, 1_c_int)
    END SUBROUTINE invalidate_main_window

    SUBROUTINE draw_text(hdc, x, y, text, color)
        TYPE(c_ptr), INTENT(IN), VALUE :: hdc
        INTEGER, INTENT(IN) :: x, y, color
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(128), TARGET :: buffer
        INTEGER(c_int) :: bk_result, color_result, text_result

        CALL set_c_string(buffer, text)
        bk_result = SetBkMode(hdc, TRANSPARENT)
        color_result = SetTextColor(hdc, INT(color, c_int))
        text_result = TextOutA(hdc, INT(x, c_int), INT(y, c_int), buffer, LEN_TRIM(text))
    END SUBROUTINE draw_text

    SUBROUTINE draw_rectangle_outline(hdc, x, y, width, height)
        TYPE(c_ptr), INTENT(IN), VALUE :: hdc
        INTEGER, INTENT(IN) :: x, y, width, height
        INTEGER(c_int) :: move_result, line_result

        move_result = MoveToEx(hdc, INT(x, c_int), INT(y, c_int), c_null_ptr)
        line_result = LineTo(hdc, INT(x + width - 1, c_int), INT(y, c_int))
        line_result = LineTo(hdc, INT(x + width - 1, c_int), INT(y + height - 1, c_int))
        line_result = LineTo(hdc, INT(x, c_int), INT(y + height - 1, c_int))
        line_result = LineTo(hdc, INT(x, c_int), INT(y, c_int))
    END SUBROUTINE draw_rectangle_outline

    CHARACTER(LEN=2) FUNCTION square_name(rank, file)
        INTEGER, INTENT(IN) :: rank, file
        square_name(1:1) = CHAR(ICHAR('a') + file - 1)
        square_name(2:2) = CHAR(ICHAR('0') + rank)
    END FUNCTION square_name

    CHARACTER(LEN=1) FUNCTION piece_symbol(piece, color)
        INTEGER, INTENT(IN) :: piece, color
        SELECT CASE (piece)
        CASE (PAWN)
            piece_symbol = 'P'
        CASE (KNIGHT)
            piece_symbol = 'N'
        CASE (BISHOP)
            piece_symbol = 'B'
        CASE (ROOK)
            piece_symbol = 'R'
        CASE (QUEEN)
            piece_symbol = 'Q'
        CASE (KING)
            piece_symbol = 'K'
        CASE DEFAULT
            piece_symbol = ' '
        END SELECT
        IF (color == BLACK) piece_symbol = CHAR(ICHAR(piece_symbol) + 32)
    END FUNCTION piece_symbol

    CHARACTER(LEN=1) FUNCTION file_label(file)
        INTEGER, INTENT(IN) :: file
        file_label = CHAR(ICHAR('a') + file - 1)
    END FUNCTION file_label

    CHARACTER(LEN=1) FUNCTION rank_label(rank)
        INTEGER, INTENT(IN) :: rank
        rank_label = CHAR(ICHAR('0') + rank)
    END FUNCTION rank_label

    CHARACTER(LEN=16) FUNCTION integer_to_text(value)
        INTEGER, INTENT(IN) :: value
        WRITE(integer_to_text, '(I0)') value
    END FUNCTION integer_to_text

    SUBROUTINE append_fragment(buffer, used, fragment)
        CHARACTER(LEN=*), INTENT(INOUT) :: buffer
        INTEGER, INTENT(INOUT) :: used
        CHARACTER(LEN=*), INTENT(IN) :: fragment
        INTEGER :: amount

        IF (LEN(fragment) <= 0) RETURN
        IF (used >= LEN(buffer)) RETURN
        amount = MIN(LEN(fragment), LEN(buffer) - used)
        IF (amount <= 0) RETURN
        buffer(used + 1:used + amount) = fragment(1:amount)
        used = used + amount
    END SUBROUTINE append_fragment

    SUBROUTINE set_c_string(buffer, text)
        CHARACTER(KIND=c_char), DIMENSION(:), INTENT(OUT), TARGET :: buffer
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER :: i, limit

        buffer = c_null_char
        limit = MIN(LEN_TRIM(text), SIZE(buffer) - 1)
        DO i = 1, limit
            buffer(i) = CHAR(ICHAR(text(i:i)), KIND=c_char)
        END DO
        buffer(limit + 1) = c_null_char
    END SUBROUTINE set_c_string

    TYPE(c_ptr) FUNCTION ptr_from_intptr(value)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        ptr_from_intptr = TRANSFER(value, ptr_from_intptr)
    END FUNCTION ptr_from_intptr

    INTEGER(c_intptr_t) FUNCTION funptr_to_intptr(value)
        TYPE(c_funptr), INTENT(IN) :: value
        funptr_to_intptr = TRANSFER(value, funptr_to_intptr)
    END FUNCTION funptr_to_intptr

    INTEGER(c_int) FUNCTION loword(value)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        loword = INT(IAND(value, INT(Z'FFFF', c_intptr_t)), c_int)
    END FUNCTION loword

    INTEGER FUNCTION loword_signed(value)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        INTEGER(c_intptr_t) :: masked

        masked = IAND(value, INT(Z'FFFF', c_intptr_t))
        IF (masked >= 32768_c_intptr_t) masked = masked - 65536_c_intptr_t
        loword_signed = INT(masked)
    END FUNCTION loword_signed

    INTEGER FUNCTION hiword_signed(value)
        INTEGER(c_intptr_t), INTENT(IN) :: value
        INTEGER(c_intptr_t) :: shifted

        shifted = IAND(ISHFT(value, -16), INT(Z'FFFF', c_intptr_t))
        IF (shifted >= 32768_c_intptr_t) shifted = shifted - 65536_c_intptr_t
        hiword_signed = INT(shifted)
    END FUNCTION hiword_signed

    INTEGER FUNCTION rgb_color(red, green, blue)
        INTEGER, INTENT(IN) :: red, green, blue
        rgb_color = red + ISHFT(green, 8) + ISHFT(blue, 16)
    END FUNCTION rgb_color

    CHARACTER(LEN=2) FUNCTION windows_newline()
        windows_newline = CHAR(13) // CHAR(10)
    END FUNCTION windows_newline

    RECURSIVE SUBROUTINE initialize_gdiplus_and_images()
        TYPE(GdiplusStartupInput) :: gdiplus_input

        piece_images = c_null_ptr
        gdiplus_started = .FALSE.
        piece_images_loaded = .FALSE.
        gdiplus_input%GdiplusVersion = 1_c_int
        gdiplus_input%DebugEventCallback = c_null_funptr
        gdiplus_input%SuppressBackgroundThread = 0_c_int
        gdiplus_input%SuppressExternalCodecs = 0_c_int

        IF (GdiplusStartup(gdiplus_token, gdiplus_input, c_null_ptr) == GDI_PLUS_OK) THEN
            gdiplus_started = .TRUE.
            CALL load_piece_images()
        END IF
    END SUBROUTINE initialize_gdiplus_and_images

    RECURSIVE SUBROUTINE load_piece_images()
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
    END SUBROUTINE load_piece_images

    RECURSIVE SUBROUTINE load_one_piece_image(filename, image_out)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(c_ptr), INTENT(OUT) :: image_out
        INTEGER(c_int16_t), DIMENSION(260), TARGET :: wide_filename

        image_out = c_null_ptr
        CALL set_wide_string(filename, wide_filename)
        IF (GdipCreateBitmapFromFile(c_loc(wide_filename(1)), image_out) /= GDI_PLUS_OK) THEN
            image_out = c_null_ptr
        END IF
    END SUBROUTINE load_one_piece_image

    RECURSIVE SUBROUTINE release_piece_images()
        INTEGER :: color_idx, piece_idx
        INTEGER(c_int) :: ignored_int

        DO color_idx = 1, 2
            DO piece_idx = 1, 6
                IF (C_ASSOCIATED(piece_images(color_idx, piece_idx))) THEN
                    ignored_int = GdipDisposeImage(piece_images(color_idx, piece_idx))
                    piece_images(color_idx, piece_idx) = c_null_ptr
                END IF
            END DO
        END DO
        piece_images_loaded = .FALSE.
    END SUBROUTINE release_piece_images

    RECURSIVE SUBROUTINE set_wide_string(text, buffer)
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER(c_int16_t), DIMENSION(:), INTENT(OUT) :: buffer
        INTEGER :: i, limit

        buffer = 0_c_int16_t
        limit = MIN(LEN_TRIM(text), SIZE(buffer) - 1)
        DO i = 1, limit
            buffer(i) = INT(IACHAR(text(i:i)), c_int16_t)
        END DO
        buffer(limit + 1) = 0_c_int16_t
    END SUBROUTINE set_wide_string

END MODULE Book_Edit_GUI_App

PROGRAM Fortran_Chess_Book_Editor
    USE Book_Edit_GUI_App, ONLY: run_book_edit_gui
    IMPLICIT NONE

    CALL run_book_edit_gui()
END PROGRAM Fortran_Chess_Book_Editor
