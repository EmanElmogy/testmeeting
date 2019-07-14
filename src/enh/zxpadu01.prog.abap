*&---------------------------------------------------------------------*
*& Include          ZXPADU01
*&---------------------------------------------------------------------*

DATA: xp2001    TYPE TABLE OF pa2001,
      xp2002    TYPE TABLE OF pa2002,
      xp0002    TYPE pa0002,
      count     TYPE i,
      ls_pa0041 TYPE pa0041,
      no_years  TYPE i,
      no_months TYPE i.

IF innnn-infty = '2001' AND ( ipsyst-ioper = 'COP' OR ipsyst-ioper ='INS' ).

  CASE innnn-subty.

*Marriage Leave
    WHEN '0006'.

      SELECT *
      FROM pa2001
      INTO CORRESPONDING FIELDS OF TABLE xp2001
      WHERE pernr = innnn-pernr AND subty = '0006'.

      IF sy-subrc = 0.
        MESSAGE 'Employee has previously applied for leave type 0006' TYPE 'E'.
      ENDIF.

* Child Care Leave
    WHEN '0013'.

* Check if employee is a female
      SELECT SINGLE *
        FROM pa0002
        INTO CORRESPONDING FIELDS OF xp0002
        WHERE pernr = innnn-pernr.

      IF xp0002-gesch <> '2'.
        MESSAGE 'Child Care Leave is allowed for female employees only' TYPE 'E'.
      ENDIF.

      SELECT *
        FROM pa2001
        INTO CORRESPONDING FIELDS OF TABLE xp2001
        WHERE pernr = innnn-pernr AND subty = '0013'.

      DESCRIBE TABLE xp2001 LINES count.

      IF count >= 3.
        MESSAGE 'Child Care Leave can only be taken three times during employment period' TYPE 'E'.
      ENDIF.

  ENDCASE.

ENDIF.
