FUNCTION ZDISPLAY_LOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_LOG_MESSAGE STRUCTURE  SYMSG
*"----------------------------------------------------------------------

DATA: l_dummy TYPE string.

****************************************************************
* create a log where all message should be added to
****************************************************************
  PERFORM log_create.
***********************************************************************
* Add information that this is a check for passenger flights
***********************************************************************
* this informnation can be added as a free text
*  PERFORM msg_add_free_text USING t_log_message.
     LOOP AT t_log_message.
* Use the message type ZMESSAGE and msg no 999
* Issue the message in a dummy variable
      MESSAGE ID t_log_message-MSGID TYPE t_log_message-msgty NUMBER t_log_message-MSGNO
      WITH t_log_message-MSGV1 t_log_message-MSGV2
      t_log_message-MSGV3 t_log_message-MSGV4
      INTO l_dummy.

* The parameters set by message statement will be used
* Add the message in the log
      PERFORM msg_add.
    ENDLOOP.
*  ***********************************************************************
* display log file
***********************************************************************
  PERFORM log_display.

ENDFUNCTION.
*--------------------------------------------------------------------
* FORM LOG_CREATE
*--------------------------------------------------------------------
FORM log_create.
  DATA:
    l_s_log TYPE bal_s_log.

* define some header data of this log
  l_s_log-extnumber = 'Application Log Demo'.             "#EC NOTEXT
  l_s_log-aluser    = sy-uname.
  l_s_log-alprog    = sy-repid.
* ...

* create a log
  CALL FUNCTION 'BAL_LOG_CREATE'
       EXPORTING
            i_s_log = l_s_log
       EXCEPTIONS
            OTHERS  = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------
* FORM MSG_ADD_FREE_TEXT
*--------------------------------------------------------------------
FORM msg_add_free_text USING value(i_text) TYPE C.

* add this message to log file
* (I_LOG_HANDLE is not specified, we want to add to the default log.
*  If it does not exist we do not care =>EXCEPTIONS log_not_found = 0)
  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
       EXPORTING
*           I_LOG_HANDLE  =
            i_msgty       = 'S'
            i_text        = i_text
       EXCEPTIONS
            LOG_NOT_FOUND = 0
            OTHERS        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------
* FORM LOG_DISPLAY
*--------------------------------------------------------------------
FORM log_display.
  DATA:
    l_s_display_profile TYPE bal_s_prof,
    l_s_fcat            TYPE bal_s_fcat.
* get a prepared profile
  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
       IMPORTING
            e_s_display_profile = l_s_display_profile
       EXCEPTIONS
            OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* show log file with modified output profile
* - we specify the display profile since we created our own
* - we do not specify any filter (like I_S_LOG_FILTER, ...,
*   I_T_MSG_HANDLE) since we want to display all messages available
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
       EXPORTING
*           I_S_LOG_FILTER         =
*           I_T_LOG_CONTEXT_FILTER =
*           I_S_MSG_FILTER         =
*           I_T_MSG_CONTEXT_FILTER =
*           I_T_LOG_HANDLE         =
*           I_T_MSG_HANDLE         =
            i_s_display_profile    = l_s_display_profile
       EXCEPTIONS
            OTHERS                 = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

 ENDFORM.

 FORM msg_add.
  DATA:
  l_s_msg TYPE bal_s_msg.

* define data of message for Application Log
  l_s_msg-msgty = sy-msgty.
  l_s_msg-msgid = sy-msgid.
  l_s_msg-msgno = sy-msgno.
  l_s_msg-msgv1 = sy-msgv1.
  l_s_msg-msgv2 = sy-msgv2.
  l_s_msg-msgv3 = sy-msgv3.
  l_s_msg-msgv4 = sy-msgv4.

* add this message to log file
* (I_LOG_HANDLE is not specified, we want to add to the default log.
* If it does not exist we do not care =>EXCEPTIONS log_not_found = 0)
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
  EXPORTING
* I_LOG_HANDLE =
  i_s_msg = l_s_msg
  EXCEPTIONS
  log_not_found = 0
  OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
