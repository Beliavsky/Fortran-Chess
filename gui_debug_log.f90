MODULE GUI_Debug_Log
    USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_intptr_t
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: start_gui_debug_session, finish_gui_debug_session, log_gui_event, log_gui_message, &
              log_gui_checkpoint, log_gui_value, log_gui_failure

    INTEGER, PARAMETER :: LOG_UNIT = 93
    CHARACTER(LEN=*), PARAMETER :: LOG_FILENAME = 'gui_debug.log'
    LOGICAL, SAVE :: log_active = .FALSE.
    INTEGER, SAVE :: paint_counter = 0
    INTEGER, SAVE :: timer_counter = 0
    INTEGER, SAVE :: paint_checkpoint_counter = 0
    INTEGER, SAVE :: timer_checkpoint_counter = 0

CONTAINS

    SUBROUTINE start_gui_debug_session()
        INTEGER :: ios, values(8)

        IF (log_active) THEN
            CLOSE(LOG_UNIT)
            log_active = .FALSE.
        END IF

        OPEN(unit=LOG_UNIT, file=LOG_FILENAME, status='UNKNOWN', action='WRITE', position='APPEND', iostat=ios)
        IF (ios /= 0) RETURN
        log_active = .TRUE.
        paint_counter = 0
        timer_counter = 0
        paint_checkpoint_counter = 0
        timer_checkpoint_counter = 0

        CALL DATE_AND_TIME(VALUES=values)
        WRITE(LOG_UNIT, '(A)') '================================================================'
        WRITE(LOG_UNIT, '(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') 'GUI session: ', &
            values(1), '-', values(2), '-', values(3), ' ', values(5), ':', values(6), ':', values(7)
        WRITE(LOG_UNIT, '(A)') 'Log file captures GUI startup, messages, actions, and checkpoints.'
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE start_gui_debug_session

    SUBROUTINE finish_gui_debug_session(reason_text)
        CHARACTER(LEN=*), INTENT(IN) :: reason_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A)') 'GUI session end: ', TRIM(reason_text)
        WRITE(LOG_UNIT, '(A)') ''
        CALL FLUSH(LOG_UNIT)
        CLOSE(LOG_UNIT)
        log_active = .FALSE.
    END SUBROUTINE finish_gui_debug_session

    SUBROUTINE log_gui_event(event_text)
        CHARACTER(LEN=*), INTENT(IN) :: event_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A)') 'Event: ', TRIM(event_text)
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_gui_event

    SUBROUTINE log_gui_checkpoint(routine_name, checkpoint_name)
        CHARACTER(LEN=*), INTENT(IN) :: routine_name, checkpoint_name

        IF (.NOT. log_active) RETURN
        IF (TRIM(routine_name) == 'draw_gui') THEN
            paint_checkpoint_counter = paint_checkpoint_counter + 1
            IF (.NOT. should_log_repeated(paint_checkpoint_counter)) RETURN
        ELSE IF (TRIM(routine_name) == 'on_gui_timer') THEN
            timer_checkpoint_counter = timer_checkpoint_counter + 1
            IF (.NOT. should_log_repeated(timer_checkpoint_counter)) RETURN
        END IF
        WRITE(LOG_UNIT, '(A,A,A,A)') 'Checkpoint: ', TRIM(routine_name), ' :: ', TRIM(checkpoint_name)
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_gui_checkpoint

    SUBROUTINE log_gui_value(label_text, value_text)
        CHARACTER(LEN=*), INTENT(IN) :: label_text, value_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A,A,A)') 'Value: ', TRIM(label_text), ' = ', TRIM(value_text)
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_gui_value

    SUBROUTINE log_gui_failure(context_text, detail_text)
        CHARACTER(LEN=*), INTENT(IN) :: context_text, detail_text

        IF (.NOT. log_active) RETURN
        WRITE(LOG_UNIT, '(A,A,A,A)') 'Failure: ', TRIM(context_text), ' :: ', TRIM(detail_text)
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_gui_failure

    SUBROUTINE log_gui_message(message_name, wparam, lparam)
        CHARACTER(LEN=*), INTENT(IN) :: message_name
        INTEGER(c_intptr_t), INTENT(IN) :: wparam, lparam

        CHARACTER(LEN=32) :: w_text, l_text

        IF (.NOT. log_active) RETURN
        IF (TRIM(message_name) == 'WM_PAINT') THEN
            paint_counter = paint_counter + 1
            IF (.NOT. should_log_repeated(paint_counter)) RETURN
        ELSE IF (TRIM(message_name) == 'WM_TIMER') THEN
            timer_counter = timer_counter + 1
            IF (.NOT. should_log_repeated(timer_counter)) RETURN
        END IF

        WRITE(w_text, '(I0)') wparam
        WRITE(l_text, '(I0)') lparam
        WRITE(LOG_UNIT, '(A,A,A,A,A,A)') 'Message: ', TRIM(message_name), ' wParam=', TRIM(ADJUSTL(w_text)), &
            ' lParam=', TRIM(ADJUSTL(l_text))
        CALL FLUSH(LOG_UNIT)
    END SUBROUTINE log_gui_message

    LOGICAL FUNCTION should_log_repeated(counter_value)
        INTEGER, INTENT(IN) :: counter_value

        should_log_repeated = (counter_value <= 5 .OR. MOD(counter_value, 25) == 0)
    END FUNCTION should_log_repeated

END MODULE GUI_Debug_Log
