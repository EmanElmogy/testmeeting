*&---------------------------------------------------------------------*
*& Report ZHCM_TIME_ATTENDANCE_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_time_attendance_upload.
************************************************************************
*Title : 'Upload Time Events 'Check in - Check out' of Employees* *    *
* Description : This program is used to upload time events data(date,  *
* clockin/clockout , employee number, time event type,
*                                                                      *                                    *
************************************************************************
************************************************************************
*
* Inputs : Excel Sheet - converted to text file contains *
* 'employee number , date , in/out , Time Event Type **
* Outputs : Time events uploaded in IT2011 *
*
************************************************************************
* Processing Steps: *
* take sheet details mentioned above and for every employee insert a *
* record in IT2011 (TEVEN Database table) *
************************************************************************
TABLES: teven.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-s01.
*//Radio buttons design
PARAMETERS: p_rad1 RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND dummy,
            p_rad2 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME.
PARAMETERS: dataset(132) LOWER CASE.
SELECT-OPTIONS:  p_emp FOR teven-pernr MATCHCODE OBJECT ewapernr, "OBLIGATORY,
                 p_dats FOR teven-ldate.
SELECTION-SCREEN END OF BLOCK blk2.

DATA: xteven          TYPE teven.
DATA: wa_teven        TYPE teven.
DATA: del_teven	      LIKE TABLE OF	teven.
DATA: ins_teven	      LIKE TABLE OF	teven.
DATA: del_teven_more  LIKE TABLE OF teven_more.
DATA: ins_teven_more  LIKE TABLE OF teven_more.
DATA: stored_teven    TYPE TABLE OF teven WITH HEADER LINE.
DATA t_log_message TYPE TABLE OF symsg WITH HEADER LINE.

DATA : BEGIN OF record,
         pernr     TYPE n LENGTH 8,
         ldate(10),
         ltime(8),
         satza(3),
         terid(4),
         dallf(1) ,
       END OF record.

DATA tfile TYPE string.
DATA : records LIKE TABLE OF record WITH HEADER LINE.

DATA: lines          TYPE i,
      counter        TYPE i,
      e_counter      TYPE i,
      wa_records     LIKE LINE OF records,
      ls_records     LIKE LINE OF records,
      temp_records   LIKE TABLE OF record,
      clock_in       TYPE sy-uzeit,
      clock_out      LIKE clock_in,
      first_clock_in LIKE clock_in,
      last_clock_out LIKE clock_in,
      first_ltime    LIKE record-ltime,
      last_ltime     LIKE record-ltime.

AT SELECTION-SCREEN OUTPUT.

  IF p_rad1 = 'X'.

    LOOP AT SCREEN.
      IF screen-name = 'P_EMP-LOW' OR screen-name = 'P_EMP-HIGH' OR screen-name = '%_P_EMP_%_APP_%-TEXT'
      OR screen-name = '%_P_EMP_%_APP_%-VALU_PUSH' OR screen-name = 'P_DATS-LOW' OR screen-name = 'P_DATS-HIGH'
      OR screen-name = '%_P_DATS_%_APP_%-TEXT' OR screen-name = '%_P_DATS_%_APP_%-VALU_PUSH'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'DATASET' OR screen-name = '%_DATASET_%_APP_%-TEXT'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE.

    CLEAR dataset.

    LOOP AT SCREEN.
      IF screen-name = 'DATASET' OR screen-name = '%_DATASET_%_APP_%-TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'P_EMP-LOW' OR screen-name = 'P_EMP-HIGH' OR screen-name = '%_P_EMP_%_APP_%-TEXT'
      OR screen-name = '%_P_EMP_%_APP_%-VALU_PUSH' OR screen-name = 'P_DATS-LOW' OR screen-name = 'P_DATS-HIGH'
      OR screen-name = '%_P_DATS_%_APP_%-TEXT' OR screen-name = '%_P_DATS_%_APP_%-VALU_PUSH'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR dataset.

  " popup for geting the text file
  PERFORM filename_get.


AT SELECTION-SCREEN.

* In case user pressed "Execute"
  IF sy-ucomm = 'ONLI' OR sy-ucomm IS INITIAL.

    IF p_rad1  = 'X'.

      IF dataset IS NOT INITIAL.

        tfile = dataset.
        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename                = tfile
            has_field_separator     = 'X'
          TABLES
            data_tab                = records
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            OTHERS                  = 17.
**********************************************************************
        IF sy-subrc <> 0.
          MESSAGE 'Unable to Read File' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'No File Uploaded' TYPE 'E'.
      ENDIF. "end of dataset

    ENDIF. "end of p_rad1

    IF p_rad2 = 'X' AND ( p_emp IS NOT INITIAL AND p_dats IS NOT INITIAL ).

      SELECT *
       FROM teven
       INTO TABLE stored_teven
       WHERE pernr IN p_emp
       AND ldate IN p_dats
       AND stokz <> 'X'.

      IF sy-subrc <> 0.
        MESSAGE 'No Data found' TYPE 'E'.
      ENDIF.

    ENDIF.

    IF p_rad2 = 'X' AND ( p_emp IS INITIAL OR p_dats IS INITIAL ).
      MESSAGE 'Personnel No. and Date must be entered' TYPE 'E'.
    ENDIF.

  ENDIF. "end of sy-ucomm

END-OF-SELECTION.

  IF p_rad1 = 'X'.
    PERFORM upload_data.
  ELSE.
    PERFORM delete_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_data.

* Validate first check-in and last check-out for each employees
* Added by Maryam
  SORT records BY pernr ldate ltime.

  DELETE ADJACENT DUPLICATES FROM records.
  DELETE records WHERE pernr IS INITIAL.

  LOOP AT records INTO ls_records.

    LOOP AT records INTO wa_records WHERE pernr = ls_records-pernr AND ldate = ls_records-ldate.
      APPEND wa_records TO temp_records.
      CLEAR wa_records.
    ENDLOOP.

    DESCRIBE TABLE temp_records LINES lines.

    IF lines > 1.

      LOOP AT temp_records INTO wa_records.

        TRANSLATE wa_records-satza TO UPPER CASE.

        CASE wa_records-satza.
          WHEN 'P10'.
            CONCATENATE wa_records-ltime+0(2) wa_records-ltime+3(2) wa_records-ltime+6(2) INTO clock_in.
            IF first_clock_in IS INITIAL.
              first_clock_in = clock_in.
            ELSE.
              IF first_clock_in > clock_in.
                first_clock_in = clock_in.
              ENDIF.
            ENDIF.
          WHEN 'P20'.
            CONCATENATE wa_records-ltime+0(2) wa_records-ltime+3(2)  wa_records-ltime+6(2) INTO clock_out.
            IF last_clock_out IS INITIAL.
              last_clock_out = clock_out.
            ELSE.
              IF last_clock_out < clock_out.
                last_clock_out = clock_out.
              ENDIF.
            ENDIF.
        ENDCASE.

      ENDLOOP.

      CONCATENATE first_clock_in+0(2) ':' first_clock_in+2(2) ':' first_clock_in+4(2) INTO first_ltime.
      CONCATENATE last_clock_out+0(2) ':' last_clock_out+2(2) ':' last_clock_out+4(2) INTO last_ltime.

      DELETE  records WHERE pernr = ls_records-pernr AND ( satza = 'p10' OR satza = 'P10' ) AND ldate = ls_records-ldate AND ltime <> first_ltime.
      DELETE  records WHERE pernr = ls_records-pernr AND ( satza = 'p20' OR satza = 'P20' ) AND ldate = ls_records-ldate AND ltime <> last_ltime.


    ENDIF.

    CLEAR: temp_records[], lines, clock_in, clock_out, first_clock_in, last_clock_out, first_ltime, last_ltime.

  ENDLOOP.

  IF records[] IS NOT INITIAL.
    SELECT *
      FROM teven
      INTO TABLE stored_teven
      FOR ALL ENTRIES IN records
      WHERE pernr = records-pernr
      AND stokz <> 'X'.

    LOOP AT records.
      CLEAR  xteven.

      xteven-pernr      = records-pernr.
      xteven-ldate(4)   = records-ldate+6(4).
      xteven-ldate+4(2) = records-ldate+3(2).
      xteven-ldate+6(2) = records-ldate(2).
      xteven-ltime(2)   = records-ltime(2).
      xteven-ltime+2(2) = records-ltime+3(2).
      xteven-ltime+4(2) = records-ltime+6(2).
      xteven-satza      = records-satza.
      TRANSLATE xteven-satza TO UPPER CASE.
      xteven-terid      = records-terid.
      xteven-dallf      = records-dallf.
      xteven-erdat      = sy-datum.
      xteven-ertim      = sy-uzeit.
      xteven-uname      = sy-uname.
      xteven-origf      = 'M'.

      READ TABLE stored_teven WITH KEY pernr = records-pernr
                                       ldate = xteven-ldate
                                      " ltime = xteven-ltime
                                       satza = xteven-satza.
      IF sy-subrc NE 0.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'PD_SEQ_NR'
            toyear                  = '9999'
          IMPORTING
            number                  = xteven-pdsnr
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        FREE ins_teven.
        APPEND xteven TO ins_teven.

        CALL FUNCTION 'ENQUEUE_EPPRELE'
          EXPORTING
            mode_prel      = 'E'
            mandt          = sy-mandt
            pernr          = xteven-pernr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.

          t_log_message-msgty = 'E'.
          t_log_message-msgid = 'ZHCM'.
          t_log_message-msgno = '002'.
          t_log_message-msgv1 = xteven-pernr.
          APPEND t_log_message.
          CLEAR t_log_message.
          e_counter = e_counter + 1.

        ELSE.

*    MODIFY TEVEN FROM XTEVEN.
*        commit WORK.
*
          CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
            TABLES
              del_teven      = del_teven
              ins_teven      = ins_teven
              del_teven_more = del_teven_more
              ins_teven_more = ins_teven_more
            EXCEPTIONS
              insert_failed  = 1
              update_failed  = 2
              OTHERS         = 3.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          IF sy-subrc = 0.
            counter = counter + 1.
            COMMIT WORK.
            "     MESSAGE s001(zhcm) WITH counter.
          ENDIF.

          CALL FUNCTION 'DEQUEUE_EPPRELE'
            EXPORTING
              mode_prel = 'E'
              mandt     = sy-mandt
              pernr     = xteven-pernr.

        ENDIF.
      ELSE.
        " MESSAGE s008(zhcm).
        t_log_message-msgty = 'E'.
        t_log_message-msgid = 'ZHCM'.
        t_log_message-msgno = '003'.
        t_log_message-msgv1 = xteven-pernr.
        APPEND t_log_message.
        CLEAR t_log_message.
      ENDIF.

    ENDLOOP.

* Prepare Log.
* Rows successfully updated
    t_log_message-msgty = 'S'.
    t_log_message-msgid = 'ZHCM'.
    t_log_message-msgno = '000'.
    t_log_message-msgv1 = counter.
    SHIFT t_log_message-msgv1 LEFT DELETING LEADING space.
    APPEND t_log_message.
    CLEAR t_log_message.

* Rows not created
    IF e_counter <> '0'.
      t_log_message-msgty = 'S'.
      t_log_message-msgid = 'ZHCM'.
      t_log_message-msgno = '001'.
      t_log_message-msgv1 = e_counter.
      SHIFT t_log_message-msgv1 LEFT DELETING LEADING space.
      APPEND t_log_message.
      CLEAR t_log_message.
    ENDIF.

* Sort Log Table.
    SORT t_log_message BY msgno msgv1.
    DELETE ADJACENT DUPLICATES FROM t_log_message COMPARING ALL FIELDS.

    CALL FUNCTION 'ZDISPLAY_LOG'
      TABLES
        t_log_message           = t_log_message
      EXCEPTIONS
        log_header_inconsistent = 1
        logging_error           = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_data.

  LOOP AT stored_teven INTO DATA(ls_teven).

    FREE ins_teven.
    APPEND ls_teven TO del_teven.

    CALL FUNCTION 'ENQUEUE_EPPRELE'
      EXPORTING
        mode_prel      = 'E'
        mandt          = sy-mandt
        pernr          = ls_teven-pernr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      t_log_message-msgty = 'E'.
      t_log_message-msgid = 'ZHCM'.
      t_log_message-msgno = '006'.
      t_log_message-msgv1 = xteven-pernr.
      APPEND t_log_message.
      CLEAR t_log_message.
      e_counter = e_counter + 1.
    ELSE.

      CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
        TABLES
          del_teven      = del_teven
          ins_teven      = ins_teven
          del_teven_more = del_teven_more
          ins_teven_more = ins_teven_more
        EXCEPTIONS
          insert_failed  = 1
          update_failed  = 2
          OTHERS         = 3.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF sy-subrc = 0.
        counter = counter + 1.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_EPPRELE'
        EXPORTING
          mode_prel = 'E'
          mandt     = sy-mandt
          pernr     = ls_teven-pernr.

    ENDIF.

  ENDLOOP.

* Prepare Log.
* Rows successfully updated
  t_log_message-msgty = 'S'.
  t_log_message-msgid = 'ZHCM'.
  t_log_message-msgno = '004'.
  t_log_message-msgv1 = counter.
  SHIFT t_log_message-msgv1 LEFT DELETING LEADING space.
  APPEND t_log_message.
  CLEAR t_log_message.

* Rows not created
  IF e_counter <> '0'.
    t_log_message-msgty = 'S'.
    t_log_message-msgid = 'ZHCM'.
    t_log_message-msgno = '005'.
    t_log_message-msgv1 = e_counter.
    SHIFT t_log_message-msgv1 LEFT DELETING LEADING space.
    APPEND t_log_message.
    CLEAR t_log_message.
  ENDIF.

* Sort Log Table.
  SORT t_log_message BY msgno.
  DELETE ADJACENT DUPLICATES FROM t_log_message COMPARING ALL FIELDS.

  CALL FUNCTION 'ZDISPLAY_LOG'
    TABLES
      t_log_message           = t_log_message
    EXCEPTIONS
      log_header_inconsistent = 1
      logging_error           = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  filename_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM filename_get .

  DATA: filerc         TYPE i,
        filename_tab   TYPE filetable,
        filename       TYPE file_table,
        fileaction     TYPE i,
        window_title   TYPE string,
        my_file_filter TYPE string.

  window_title = TEXT-005.

  CLASS cl_gui_frontend_services DEFINITION LOAD.
*  concatenate cl_gui_frontend_services=>filetype_all
*              into my_file_filter.
  my_file_filter = 'All files(*.*)|*.*'(084).
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = window_title
      file_filter             = my_file_filter
    CHANGING
      file_table              = filename_tab
      rc                      = filerc
      user_action             = fileaction
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4.


  IF fileaction EQ cl_gui_frontend_services=>action_ok.
    CHECK NOT filename_tab IS INITIAL.
    READ TABLE filename_tab INTO filename INDEX 1.
    MOVE filename-filename TO dataset.
  ENDIF.

ENDFORM.                    " filename_get
