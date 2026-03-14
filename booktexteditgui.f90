MODULE Book_Text_Edit_GUI_App
    USE, INTRINSIC :: iso_c_binding
    USE Opening_Book, ONLY: Opening_Book_Type, load_opening_book
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_book_text_edit_gui

    INTEGER(c_int), PARAMETER :: WM_CREATE = 1_c_int
    INTEGER(c_int), PARAMETER :: WM_DESTROY = 2_c_int
    INTEGER(c_int), PARAMETER :: WM_COMMAND = 273_c_int
    INTEGER(c_int), PARAMETER :: SW_SHOWNORMAL = 1_c_int
    INTEGER(c_int), PARAMETER :: CW_USEDEFAULT = -2147483647_c_int - 1_c_int
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
    INTEGER(c_long), PARAMETER :: WS_EX_CLIENTEDGE = INT(Z'00000200', c_long)
    INTEGER(c_int), PARAMETER :: MB_OK = 0_c_int
    INTEGER(c_int), PARAMETER :: MB_ICONERROR = 16_c_int
    INTEGER(c_int), PARAMETER :: WINDOW_WIDTH = 980_c_int
    INTEGER(c_int), PARAMETER :: WINDOW_HEIGHT = 760_c_int
    INTEGER, PARAMETER :: MAX_EDITOR_TEXT = 131072
    INTEGER, PARAMETER :: MAX_PATH_TEXT = 512
    CHARACTER(LEN=*), PARAMETER :: VALIDATE_TMP_FILE = 'bookedit_validate.tmp'

    TYPE, BIND(C) :: POINT
        INTEGER(c_long) :: x
        INTEGER(c_long) :: y
    END TYPE POINT

    TYPE, BIND(C) :: MSG
        TYPE(c_ptr) :: hwnd
        INTEGER(c_int) :: message
        INTEGER(c_intptr_t) :: wParam
        INTEGER(c_intptr_t) :: lParam
        INTEGER(c_long) :: time
        TYPE(POINT) :: pt
    END TYPE MSG

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

    TYPE(c_ptr), SAVE :: gui_instance = c_null_ptr
    TYPE(c_ptr), SAVE :: gui_main_hwnd = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_path_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_editor_edit = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_status_static = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_load_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_save_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_validate_button = c_null_ptr
    TYPE(c_ptr), SAVE :: hwnd_fix_button = c_null_ptr

    INTEGER, SAVE :: last_error_line = 0
    CHARACTER(LEN=512), SAVE :: last_replacement_line = ''
    CHARACTER(LEN=512), SAVE :: last_error_text = ''
    CHARACTER(LEN=256), SAVE :: last_error_message = ''
    CHARACTER(LEN=256), SAVE :: last_suggestion = ''
    CHARACTER(LEN=256), SAVE :: status_text = 'Set a file path, then Load or edit text directly.'

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

        FUNCTION MessageBoxA(hwnd, text_ptr, caption_ptr, box_type) BIND(C, NAME="MessageBoxA")
            IMPORT :: c_ptr, c_int
            TYPE(c_ptr), VALUE :: hwnd, text_ptr, caption_ptr
            INTEGER(c_int), VALUE :: box_type
            INTEGER(c_int) :: MessageBoxA
        END FUNCTION MessageBoxA
    END INTERFACE

CONTAINS

    RECURSIVE SUBROUTINE run_book_text_edit_gui()
        TYPE(WNDCLASSEXA) :: window_class
        TYPE(MSG) :: message_data
        TYPE(c_ptr) :: window_handle
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: class_name
        CHARACTER(KIND=c_char), DIMENSION(32), TARGET :: window_title
        INTEGER(c_int) :: message_status

        gui_instance = c_null_ptr
        CALL set_c_string('FortranChessBookEditGUI', class_name)
        CALL set_c_string('Fortran Chess Book Editor', window_title)
        window_class%cbSize = INT(c_sizeof(window_class), c_int)
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

        IF (RegisterClassExA(window_class) == 0_c_int) RETURN

        window_handle = CreateWindowExA(0_c_long, c_loc(class_name(1)), c_loc(window_title(1)), WS_OVERLAPPEDWINDOW, &
            CW_USEDEFAULT, CW_USEDEFAULT, WINDOW_WIDTH, WINDOW_HEIGHT, c_null_ptr, c_null_ptr, gui_instance, c_null_ptr)
        IF (.NOT. C_ASSOCIATED(window_handle)) RETURN
        gui_main_hwnd = window_handle

        message_status = ShowWindow(window_handle, SW_SHOWNORMAL)
        message_status = UpdateWindow(window_handle)

        DO
            message_status = GetMessageA(message_data, c_null_ptr, 0_c_int, 0_c_int)
            IF (message_status <= 0_c_int) EXIT
            message_status = TranslateMessage(message_data)
            message_status = INT(DispatchMessageA(message_data), c_int)
        END DO
    END SUBROUTINE run_book_text_edit_gui

    RECURSIVE INTEGER(c_intptr_t) FUNCTION window_proc(hwnd, msg, wparam, lparam) BIND(C)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER(c_int), VALUE :: msg
        INTEGER(c_intptr_t), VALUE :: wparam, lparam
        TYPE(c_ptr) :: command_hwnd

        SELECT CASE (msg)
        CASE (WM_CREATE)
            CALL create_controls(hwnd)
            window_proc = 0_c_intptr_t
        CASE (WM_COMMAND)
            command_hwnd = ptr_from_intptr(lparam)
            IF (C_ASSOCIATED(command_hwnd, hwnd_load_button)) THEN
                CALL load_book_file(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_save_button)) THEN
                CALL save_book_file(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_validate_button)) THEN
                CALL validate_editor_buffer(hwnd)
                window_proc = 0_c_intptr_t
                RETURN
            ELSE IF (C_ASSOCIATED(command_hwnd, hwnd_fix_button)) THEN
                CALL apply_suggested_fix()
                window_proc = 0_c_intptr_t
                RETURN
            END IF
            window_proc = DefWindowProcA(hwnd, msg, wparam, lparam)
        CASE (WM_DESTROY)
            CALL PostQuitMessage(0_c_int)
            window_proc = 0_c_intptr_t
        CASE DEFAULT
            window_proc = DefWindowProcA(hwnd, msg, wparam, lparam)
        END SELECT
    END FUNCTION window_proc

    RECURSIVE SUBROUTINE create_controls(parent_hwnd)
        TYPE(c_ptr), VALUE :: parent_hwnd
        TYPE(c_ptr) :: ignored_hwnd

        ignored_hwnd = create_control('STATIC', 'Opening Book Editor', 0_c_long, WS_CHILD + WS_VISIBLE, &
            20_c_int, 16_c_int, 220_c_int, 20_c_int, parent_hwnd)
        ignored_hwnd = create_control('STATIC', 'File', 0_c_long, WS_CHILD + WS_VISIBLE, &
            20_c_int, 48_c_int, 40_c_int, 20_c_int, parent_hwnd)
        hwnd_path_edit = create_control('EDIT', 'book_white.txt', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + ES_AUTOHSCROLL, 20_c_int, 72_c_int, 540_c_int, 24_c_int, parent_hwnd)
        hwnd_load_button = create_control('BUTTON', 'Load', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, 574_c_int, 70_c_int, 80_c_int, 28_c_int, parent_hwnd)
        hwnd_save_button = create_control('BUTTON', 'Save', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, 664_c_int, 70_c_int, 80_c_int, 28_c_int, parent_hwnd)
        hwnd_validate_button = create_control('BUTTON', 'Validate', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, 754_c_int, 70_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_fix_button = create_control('BUTTON', 'Apply Fix', 0_c_long, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP, 854_c_int, 70_c_int, 90_c_int, 28_c_int, parent_hwnd)
        hwnd_editor_edit = create_control('EDIT', '', WS_EX_CLIENTEDGE, &
            WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER + WS_VSCROLL + WS_HSCROLL + ES_MULTILINE + ES_AUTOVSCROLL + &
            ES_AUTOHSCROLL, 20_c_int, 116_c_int, 924_c_int, 540_c_int, parent_hwnd)
        hwnd_status_static = create_control('STATIC', TRIM(status_text), 0_c_long, &
            WS_CHILD + WS_VISIBLE, 20_c_int, 668_c_int, 924_c_int, 52_c_int, parent_hwnd)
        CALL load_book_file(parent_hwnd)
    END SUBROUTINE create_controls

    RECURSIVE FUNCTION create_control(class_name, text, ex_style, style, x, y, width, height, parent_hwnd) RESULT(control_hwnd)
        CHARACTER(LEN=*), INTENT(IN) :: class_name, text
        INTEGER(c_long), INTENT(IN) :: ex_style, style
        INTEGER(c_int), INTENT(IN) :: x, y, width, height
        TYPE(c_ptr), VALUE :: parent_hwnd
        TYPE(c_ptr) :: control_hwnd
        CHARACTER(KIND=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: class_buffer, text_buffer

        CALL allocate_c_string(class_name, class_buffer)
        CALL allocate_c_string(text, text_buffer)
        control_hwnd = CreateWindowExA(ex_style, c_loc(class_buffer(1)), c_loc(text_buffer(1)), style, x, y, width, height, &
            parent_hwnd, c_null_ptr, gui_instance, c_null_ptr)
        DEALLOCATE(class_buffer, text_buffer)
    END FUNCTION create_control

    RECURSIVE SUBROUTINE load_book_file(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=MAX_PATH_TEXT) :: filename
        CHARACTER(LEN=MAX_EDITOR_TEXT) :: editor_text, error_text
        LOGICAL :: ok

        filename = TRIM(get_path_text(hwnd_path_edit))
        IF (LEN_TRIM(filename) == 0) THEN
            CALL set_status('Enter a file path first.')
            RETURN
        END IF

        CALL clear_last_validation()
        ok = read_text_file(filename, editor_text, error_text)
        IF (.NOT. ok) THEN
            CALL set_status(TRIM(error_text))
            CALL show_message_box(main_hwnd, TRIM(error_text), 'Load Failed', MB_OK + MB_ICONERROR)
            RETURN
        END IF
        CALL set_editor_text(hwnd_editor_edit, editor_text)
        CALL set_status('Loaded ' // TRIM(filename))
    END SUBROUTINE load_book_file

    RECURSIVE SUBROUTINE save_book_file(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        CHARACTER(LEN=MAX_PATH_TEXT) :: filename
        CHARACTER(LEN=MAX_EDITOR_TEXT) :: editor_text, error_text
        LOGICAL :: ok

        filename = TRIM(get_path_text(hwnd_path_edit))
        IF (LEN_TRIM(filename) == 0) THEN
            CALL set_status('Enter a file path first.')
            RETURN
        END IF

        editor_text = get_editor_text(hwnd_editor_edit)
        ok = write_text_file(filename, editor_text, error_text)
        IF (.NOT. ok) THEN
            CALL set_status(TRIM(error_text))
            CALL show_message_box(main_hwnd, TRIM(error_text), 'Save Failed', MB_OK + MB_ICONERROR)
            RETURN
        END IF
        CALL set_status('Saved ' // TRIM(filename))
    END SUBROUTINE save_book_file

    RECURSIVE SUBROUTINE validate_editor_buffer(main_hwnd)
        TYPE(c_ptr), VALUE :: main_hwnd
        TYPE(Opening_Book_Type) :: book
        CHARACTER(LEN=MAX_EDITOR_TEXT) :: editor_text, error_text, detail_text
        LOGICAL :: ok

        editor_text = get_editor_text(hwnd_editor_edit)
        ok = write_text_file(VALIDATE_TMP_FILE, editor_text, error_text)
        IF (.NOT. ok) THEN
            CALL set_status(TRIM(error_text))
            RETURN
        END IF

        CALL clear_last_validation()
        CALL load_opening_book(VALIDATE_TMP_FILE, book)
        CALL delete_temp_file(VALIDATE_TMP_FILE)

        IF (.NOT. book%found) THEN
            CALL set_status('Validation failed: temporary file could not be read.')
            RETURN
        END IF

        IF (book%valid) THEN
            CALL set_status('Opening book is valid.')
            RETURN
        END IF

        last_error_line = book%error_line
        last_replacement_line = book%replacement_line
        last_error_text = book%error_text
        last_error_message = book%error_message
        last_suggestion = book%suggestion

        detail_text = 'Line ' // TRIM(int_to_text(last_error_line)) // ': ' // TRIM(last_error_message)
        IF (LEN_TRIM(last_error_text) > 0) detail_text = TRIM(detail_text) // CHAR(13) // CHAR(10) // 'Bad line: ' // TRIM(last_error_text)
        IF (LEN_TRIM(last_replacement_line) > 0) THEN
            detail_text = TRIM(detail_text) // CHAR(13) // CHAR(10) // 'Replacement: ' // TRIM(last_replacement_line)
        ELSE IF (LEN_TRIM(last_suggestion) > 0) THEN
            detail_text = TRIM(detail_text) // CHAR(13) // CHAR(10) // 'Suggestion: ' // TRIM(last_suggestion)
        END IF
        CALL set_status('Invalid book at line ' // TRIM(int_to_text(last_error_line)) // '.')
        CALL show_message_box(main_hwnd, TRIM(detail_text), 'Validation Result', MB_OK + MB_ICONERROR)
    END SUBROUTINE validate_editor_buffer

    RECURSIVE SUBROUTINE apply_suggested_fix()
        TYPE(Opening_Book_Type) :: book
        CHARACTER(LEN=MAX_EDITOR_TEXT) :: editor_text, error_text
        LOGICAL :: ok

        IF (last_error_line <= 0) THEN
            CALL set_status('No validated error is available to fix.')
            RETURN
        END IF
        IF (LEN_TRIM(last_replacement_line) == 0) THEN
            CALL set_status('No automatic fix is available for the last validation error.')
            RETURN
        END IF

        editor_text = get_editor_text(hwnd_editor_edit)
        ok = write_text_file(VALIDATE_TMP_FILE, editor_text, error_text)
        IF (.NOT. ok) THEN
            CALL set_status(TRIM(error_text))
            RETURN
        END IF
        ok = replace_line_in_file(VALIDATE_TMP_FILE, last_error_line, last_replacement_line, error_text)
        IF (.NOT. ok) THEN
            CALL delete_temp_file(VALIDATE_TMP_FILE)
            CALL set_status(TRIM(error_text))
            RETURN
        END IF
        ok = read_text_file(VALIDATE_TMP_FILE, editor_text, error_text)
        IF (.NOT. ok) THEN
            CALL delete_temp_file(VALIDATE_TMP_FILE)
            CALL set_status(TRIM(error_text))
            RETURN
        END IF

        CALL load_opening_book(VALIDATE_TMP_FILE, book)
        CALL delete_temp_file(VALIDATE_TMP_FILE)
        CALL set_editor_text(hwnd_editor_edit, editor_text)
        IF (book%valid) THEN
            CALL clear_last_validation()
            CALL set_status('Applied suggested fix. Save to write it back to disk.')
        ELSE
            last_error_line = book%error_line
            last_replacement_line = book%replacement_line
            last_error_text = book%error_text
            last_error_message = book%error_message
            last_suggestion = book%suggestion
            CALL set_status('Applied fix, but the book still has another error.')
        END IF
    END SUBROUTINE apply_suggested_fix

    RECURSIVE SUBROUTINE clear_last_validation()
        last_error_line = 0
        last_replacement_line = ''
        last_error_text = ''
        last_error_message = ''
        last_suggestion = ''
    END SUBROUTINE clear_last_validation

    RECURSIVE SUBROUTINE set_status(text)
        CHARACTER(LEN=*), INTENT(IN) :: text

        status_text = text
        CALL set_control_text(hwnd_status_static, text)
    END SUBROUTINE set_status

    RECURSIVE SUBROUTINE show_message_box(hwnd, text, title, box_type)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text, title
        INTEGER(c_int), INTENT(IN) :: box_type
        CHARACTER(KIND=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: text_buffer, title_buffer
        INTEGER(c_int) :: ignored_int

        CALL allocate_c_string(text, text_buffer)
        CALL allocate_c_string(title, title_buffer)
        ignored_int = MessageBoxA(hwnd, c_loc(text_buffer(1)), c_loc(title_buffer(1)), box_type)
        DEALLOCATE(text_buffer, title_buffer)
    END SUBROUTINE show_message_box

    RECURSIVE SUBROUTINE set_control_text(hwnd, text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: text_buffer
        INTEGER(c_int) :: ignored_int

        IF (.NOT. C_ASSOCIATED(hwnd)) RETURN
        CALL allocate_c_string(text, text_buffer)
        ignored_int = SetWindowTextA(hwnd, c_loc(text_buffer(1)))
        DEALLOCATE(text_buffer)
    END SUBROUTINE set_control_text

    RECURSIVE SUBROUTINE set_editor_text(hwnd, text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=*), INTENT(IN) :: text

        CALL set_control_text(hwnd, text)
    END SUBROUTINE set_editor_text

    RECURSIVE FUNCTION get_path_text(hwnd) RESULT(text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=MAX_PATH_TEXT) :: text
        text = get_control_text(hwnd, MAX_PATH_TEXT)
    END FUNCTION get_path_text

    RECURSIVE FUNCTION get_editor_text(hwnd) RESULT(text)
        TYPE(c_ptr), VALUE :: hwnd
        CHARACTER(LEN=MAX_EDITOR_TEXT) :: text
        text = get_control_text(hwnd, MAX_EDITOR_TEXT)
    END FUNCTION get_editor_text

    RECURSIVE FUNCTION get_control_text(hwnd, max_len) RESULT(text)
        TYPE(c_ptr), VALUE :: hwnd
        INTEGER, INTENT(IN) :: max_len
        CHARACTER(LEN=max_len) :: text
        CHARACTER(KIND=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: text_buffer
        INTEGER(c_int) :: copied_len
        INTEGER :: i

        text = ''
        IF (.NOT. C_ASSOCIATED(hwnd)) RETURN
        ALLOCATE(text_buffer(max_len))
        text_buffer = c_null_char
        copied_len = GetWindowTextA(hwnd, c_loc(text_buffer(1)), INT(max_len - 1, c_int))
        IF (copied_len > 0) THEN
            DO i = 1, MIN(INT(copied_len), max_len)
                text(i:i) = ACHAR(IACHAR(text_buffer(i)), KIND=kind(text))
            END DO
        END IF
        DEALLOCATE(text_buffer)
    END FUNCTION get_control_text

    RECURSIVE SUBROUTINE allocate_c_string(text, buffer)
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), ALLOCATABLE, DIMENSION(:), TARGET, INTENT(OUT) :: buffer
        INTEGER :: needed_len, i

        needed_len = LEN_TRIM(text) + 1
        IF (needed_len < 1) needed_len = 1
        ALLOCATE(buffer(needed_len))
        buffer = c_null_char
        DO i = 1, LEN_TRIM(text)
            buffer(i) = ACHAR(IACHAR(text(i:i)), KIND=c_char)
        END DO
    END SUBROUTINE allocate_c_string

    RECURSIVE SUBROUTINE set_c_string(text, buffer)
        CHARACTER(LEN=*), INTENT(IN) :: text
        CHARACTER(KIND=c_char), DIMENSION(:), INTENT(OUT), TARGET :: buffer
        INTEGER :: i, limit_len

        buffer = c_null_char
        limit_len = MIN(LEN_TRIM(text), SIZE(buffer) - 1)
        DO i = 1, limit_len
            buffer(i) = ACHAR(IACHAR(text(i:i)), KIND=c_char)
        END DO
    END SUBROUTINE set_c_string

    RECURSIVE LOGICAL FUNCTION read_text_file(filename, text_out, error_text) RESULT(ok)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        CHARACTER(LEN=*), INTENT(OUT) :: text_out, error_text
        CHARACTER(LEN=512) :: line_text
        INTEGER :: unit_no, ios
        INTEGER :: text_len, line_len, copy_len

        ok = .FALSE.
        text_out = ''
        error_text = ''
        unit_no = 71
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            error_text = 'Could not open file: ' // TRIM(filename)
            RETURN
        END IF

        text_len = 0
        DO
            READ(unit_no, '(A)', IOSTAT=ios) line_text
            IF (ios /= 0) EXIT
            line_len = LEN_TRIM(line_text)
            copy_len = MIN(line_len, LEN(text_out) - text_len)
            IF (copy_len > 0) THEN
                text_out(text_len + 1:text_len + copy_len) = line_text(1:copy_len)
                text_len = text_len + copy_len
            END IF
            IF (text_len + 2 <= LEN(text_out)) THEN
                text_out(text_len + 1:text_len + 2) = CHAR(13) // CHAR(10)
                text_len = text_len + 2
            END IF
        END DO
        CLOSE(unit_no)
        ok = .TRUE.
    END FUNCTION read_text_file

    RECURSIVE LOGICAL FUNCTION write_text_file(filename, text_in, error_text) RESULT(ok)
        CHARACTER(LEN=*), INTENT(IN) :: filename, text_in
        CHARACTER(LEN=*), INTENT(OUT) :: error_text
        CHARACTER(LEN=1024) :: line_text
        INTEGER :: unit_no, ios, start_pos, end_pos, write_len, text_len
        CHARACTER(LEN=1) :: ch

        ok = .FALSE.
        error_text = ''
        unit_no = 72
        OPEN(unit=unit_no, file=filename, status='REPLACE', action='WRITE', iostat=ios)
        IF (ios /= 0) THEN
            error_text = 'Could not write file: ' // TRIM(filename)
            RETURN
        END IF

        text_len = LEN_TRIM(text_in)
        start_pos = 1
        DO WHILE (start_pos <= MAX(1, text_len))
            end_pos = start_pos
            DO WHILE (end_pos <= text_len)
                ch = text_in(end_pos:end_pos)
                IF (ch == CHAR(10) .OR. ch == CHAR(13)) EXIT
                end_pos = end_pos + 1
            END DO
            line_text = ''
            write_len = MIN(end_pos - start_pos, LEN(line_text))
            IF (write_len > 0) line_text(1:write_len) = text_in(start_pos:start_pos + write_len - 1)
            WRITE(unit_no, '(A)', IOSTAT=ios) TRIM(line_text)
            IF (ios /= 0) THEN
                CLOSE(unit_no)
                error_text = 'Failed while writing file: ' // TRIM(filename)
                RETURN
            END IF
            IF (end_pos > text_len) EXIT
            start_pos = end_pos + 1
            IF (end_pos < text_len .AND. text_in(end_pos:end_pos) == CHAR(13) .AND. text_in(start_pos:start_pos) == CHAR(10)) &
                start_pos = start_pos + 1
            IF (start_pos > text_len + 1) EXIT
        END DO
        CLOSE(unit_no)
        ok = .TRUE.
    END FUNCTION write_text_file

    RECURSIVE LOGICAL FUNCTION replace_line_in_file(filename, target_line, replacement_text, error_text) RESULT(ok)
        CHARACTER(LEN=*), INTENT(IN) :: filename, replacement_text
        INTEGER, INTENT(IN) :: target_line
        CHARACTER(LEN=*), INTENT(OUT) :: error_text
        CHARACTER(LEN=1024), ALLOCATABLE, DIMENSION(:) :: lines
        CHARACTER(LEN=1024) :: line_text
        INTEGER :: unit_no, ios, line_count, i

        ok = .FALSE.
        error_text = ''
        unit_no = 73
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            error_text = 'Could not reopen temporary text for fixing.'
            RETURN
        END IF

        line_count = 0
        DO
            READ(unit_no, '(A)', IOSTAT=ios) line_text
            IF (ios /= 0) EXIT
            line_count = line_count + 1
        END DO
        CLOSE(unit_no)

        IF (target_line < 1 .OR. target_line > line_count) THEN
            error_text = 'Saved validation line is out of range.'
            RETURN
        END IF

        ALLOCATE(lines(line_count))
        OPEN(unit=unit_no, file=filename, status='OLD', action='READ', iostat=ios)
        IF (ios /= 0) THEN
            DEALLOCATE(lines)
            error_text = 'Could not reread temporary text for fixing.'
            RETURN
        END IF
        DO i = 1, line_count
            READ(unit=unit_no, fmt='(A)', IOSTAT=ios) lines(i)
            IF (ios /= 0) THEN
                CLOSE(unit_no)
                DEALLOCATE(lines)
                error_text = 'Failed while reading temporary text for fixing.'
                RETURN
            END IF
        END DO
        CLOSE(unit_no)

        lines(target_line) = TRIM(replacement_text)
        OPEN(unit=unit_no, file=filename, status='REPLACE', action='WRITE', iostat=ios)
        IF (ios /= 0) THEN
            DEALLOCATE(lines)
            error_text = 'Could not reopen temporary text for writing.'
            RETURN
        END IF
        DO i = 1, line_count
            WRITE(unit_no, '(A)', IOSTAT=ios) TRIM(lines(i))
            IF (ios /= 0) THEN
                CLOSE(unit_no)
                DEALLOCATE(lines)
                error_text = 'Failed while writing the fixed text.'
                RETURN
            END IF
        END DO
        CLOSE(unit_no)
        DEALLOCATE(lines)
        ok = .TRUE.
    END FUNCTION replace_line_in_file

    RECURSIVE SUBROUTINE delete_temp_file(filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER :: unit_no, ios
        LOGICAL :: exists

        INQUIRE(FILE=filename, EXIST=exists)
        IF (.NOT. exists) RETURN
        unit_no = 74
        OPEN(unit=unit_no, file=filename, status='OLD', action='READWRITE', iostat=ios)
        IF (ios == 0) CLOSE(unit_no, STATUS='DELETE')
    END SUBROUTINE delete_temp_file

    RECURSIVE FUNCTION int_to_text(value) RESULT(text)
        INTEGER, INTENT(IN) :: value
        CHARACTER(LEN=16) :: text

        WRITE(text, '(I0)') value
    END FUNCTION int_to_text

    RECURSIVE TYPE(c_ptr) FUNCTION ptr_from_intptr(raw_value) RESULT(pointer_value)
        INTEGER(c_intptr_t), VALUE :: raw_value

        pointer_value = transfer(raw_value, pointer_value)
    END FUNCTION ptr_from_intptr

END MODULE Book_Text_Edit_GUI_App

PROGRAM Fortran_Chess_Book_Text_Editor
    USE Book_Text_Edit_GUI_App, ONLY: run_book_text_edit_gui
    IMPLICIT NONE

    CALL run_book_text_edit_gui()
END PROGRAM Fortran_Chess_Book_Text_Editor
