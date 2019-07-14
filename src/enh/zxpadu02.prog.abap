**&---------------------------------------------------------------------*
**& Include          ZXPADU02
**&---------------------------------------------------------------------*

DATA: xp2001        TYPE TABLE OF pa2001,
      xp2002        TYPE TABLE OF pa2002,
      xp0002        TYPE pa0002,
      count         TYPE i,
      ls_pa0041     TYPE pa0041,
      no_years      TYPE i,
      no_months     TYPE i,
      total_hours   TYPE pa2002-stdaz,
      holiday_found TYPE char1.

FIELD-SYMBOLS <fs_hours> TYPE any.


IF innnn-infty = '2001' AND ( ipsyst-ioper = 'COP' OR ipsyst-ioper ='INS' ).

  CASE innnn-subty.

*  Maternity Leave
    WHEN '0005'.

* Check if employee is a female
      SELECT SINGLE *
        FROM pa0002
        INTO CORRESPONDING FIELDS OF xp0002
        WHERE pernr = innnn-pernr.

      IF xp0002-gesch <> '2'.
        MESSAGE 'Maternity Leave is allowed for female employees only' TYPE 'E'.
      ENDIF.

      SELECT *
        FROM pa2001
        INTO CORRESPONDING FIELDS OF TABLE xp2001
        WHERE pernr = innnn-pernr AND subty = '0005'.

      DESCRIBE TABLE xp2001 LINES count.

      IF count >= 3.
        MESSAGE 'Maternity Leave can only be taken three times in a lifetime' TYPE 'E'.
      ENDIF.

      SELECT SINGLE *
       FROM pa0041
       INTO CORRESPONDING FIELDS OF ls_pa0041
       WHERE pernr = innnn-pernr.


      IF ls_pa0041-dat01 IS NOT INITIAL AND ls_pa0041-dat01 =< innnn-begda.

        CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
          EXPORTING
            i_datum_bis   = innnn-begda
            i_datum_von   = ls_pa0041-dat01
            i_kz_incl_bis = 'X'
          IMPORTING
            e_monate      = no_months.

        IF no_months < 10.
          MESSAGE 'Employee has not completed 10 months to apply for leave type 0005' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Employee has not completed 10 months to apply for leave type 0005' TYPE 'E'.
      ENDIF.


*Pilgrimage Leave
    WHEN '0007'.

      SELECT *
       FROM pa2001
       INTO CORRESPONDING FIELDS OF TABLE xp2001
       WHERE pernr = innnn-pernr AND subty = '0007'.

      IF sy-subrc = 0.
        MESSAGE 'Employee has previously applied for leave type 0007' TYPE 'E'.
      ELSE.

        SELECT SINGLE *
         FROM pa0041
         INTO CORRESPONDING FIELDS OF ls_pa0041
         WHERE pernr = innnn-pernr.

        IF ls_pa0041-dat01 IS NOT INITIAL AND ls_pa0041-dat01 =< innnn-begda.
          CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
            EXPORTING
              beg_da        = ls_pa0041-dat01
              end_da        = innnn-begda
            IMPORTING
*             NO_DAY        =
*             NO_MONTH      =
              no_year       = no_years
*             NO_CAL_DAY    =
            EXCEPTIONS
              dateint_error = 1
              OTHERS        = 2.
          IF sy-subrc = 0.
            IF no_years < 5.
              MESSAGE 'Employee has not completed 5 years to apply for leave type 0007' TYPE 'E'.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE 'Employee has not completed 5 years to apply for leave type 0007' TYPE 'E'.
        ENDIF.

      ENDIF.
  ENDCASE.


ENDIF.



IF innnn-infty = '2002' AND ( ipsyst-ioper = 'COP' OR ipsyst-ioper ='INS' ).

  CASE innnn-subty.

* Attendance Permission
    WHEN '1000'.

      ASSIGN ('(MP200000)P2002-STDAZ') TO <fs_hours>. "Read Attendance Hours from Screen

      IF <fs_hours> > 2.
        MESSAGE 'Attendance permission hours can not exceed 2 hours' TYPE 'E'.
      ENDIF.

      SELECT *
       FROM pa2002
       INTO CORRESPONDING FIELDS OF TABLE xp2002
       WHERE pernr = innnn-pernr AND subty = '1000'.

* Read Attendance Permission in current month
      LOOP AT xp2002 INTO DATA(ls_p2002) WHERE begda+0(6) = innnn-begda+0(6).
        count = count + 1.
      ENDLOOP.

      IF count >= 2.
        MESSAGE 'Attendance Type (1000) can only be taken twice a month' TYPE 'E'.
      ENDIF.

* Attendance Nursing Hour.
    WHEN '1002'.

      ASSIGN ('(MP200000)P2002-STDAZ') TO <fs_hours>. "Read Attendance Hours from Screen

* Check if already existing nursing hours.
      SELECT SINGLE SUM( stdaz )
        FROM pa2002
        INTO total_hours
        WHERE pernr = innnn-pernr AND subty = '1002'
        AND begda = innnn-begda.

      IF total_hours + <fs_hours> > 1.
        MESSAGE 'Att/Type (1002) records can’t exceed 1 hour per day' TYPE 'E'.
      ENDIF.

* Check if employee is a female
      SELECT SINGLE *
        FROM pa0002
        INTO CORRESPONDING FIELDS OF xp0002
        WHERE pernr = innnn-pernr.

      IF xp0002-gesch <> '2'.
        MESSAGE 'Attendance Nursing Hour is allowed for female employees only' TYPE 'E'.
      ENDIF.

* Check the Entered Attendance hour
      IF <fs_hours> < 1 / 2.
        MESSAGE 'Att/ Type (1002) record can’t be created for less than 30 minutes' TYPE 'E'.
      ELSEIF <fs_hours> > 1 / 2 AND <fs_hours> < 1.
        MESSAGE 'Att/ Type (1002) record has only 2 permitted values 0.5 or 1.0 hour' TYPE 'E'.
      ELSEIF <fs_hours> > 1.
        MESSAGE 'Att/ Type (1002) record can’t be created for more than 60 minutes' TYPE 'E'.
      ENDIF.

  ENDCASE.

ENDIF.


IF innnn-infty = '0015' AND ( ipsyst-ioper = 'COP' OR ipsyst-ioper ='INS' ).

  CASE innnn-subty.

* Attendance Enhancement (Overtime 2)
    WHEN '9536'.

      CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
        EXPORTING
          date                         = innnn-begda
          holiday_calendar_id          = 'IW'
*         WITH_HOLIDAY_ATTRIBUTES      = ' '
        IMPORTING
          holiday_found                = holiday_found
*      TABLES
*         HOLIDAY_ATTRIBUTES           =
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          date_after_range             = 2
          date_before_range            = 3
          date_invalid                 = 4
          holiday_calendar_id_missing  = 5
          holiday_calendar_not_found   = 6
          OTHERS                       = 7.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        IF holiday_found <> 'X'. "Not a public holiday
          MESSAGE 'Attendance Enhancement:Overtime 2, is only applicable on Public Holidays' TYPE 'E'.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDIF.
