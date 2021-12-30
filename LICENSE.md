  SELECTION-SCREEN BEGIN OF LINE.    
    PARAMETERS: cb_ftp AS CHECKBOX USER-COMMAND cb2.   "製作FTP Button
    SELECTION-SCREEN COMMENT 3(20) TEXT-002.
  SELECTION-SCREEN END OF LINE.

   DATA: BEGIN OF it_result OCCURS 0,                  "FTP_COMMAND傳出資料TABLE          
          lines(100),
        END OF it_result.

 FORM ftp_upload.
  CHECK cb_ftp EQ 'X'.
  DATA: l_handle        TYPE i,
        l_command(255)  TYPE c,
        user(13)        TYPE c VALUE 'pccstaff',              "FTP  id
        password(64)    TYPE c VALUE '12345678',              "FTP　password
        host(64)        TYPE c VALUE '192.168.100.104 ',      "外部IP
        rfc_destination TYPE rfcdes-rfcdest VALUE 'SAPFTPA',  "SAPFTP前端執行 SAPFTPA後端執行
        key             TYPE i VALUE 26101957.                "密碼固定為26101957

  CALL FUNCTION 'HTTP_SCRAMBLE'  "取得密碼
    EXPORTING
      source      = password
      sourcelen   = '8'
      key         = key
    IMPORTING
      destination = password.
  CALL FUNCTION 'FTP_CONNECT'    "連上FTP
    EXPORTING
      user            = user
      password        = password
      host            = host
      rfc_destination = rfc_destination
    IMPORTING
      handle          = l_handle.
  IF sy-subrc EQ 0.                          "如果成功連上FTP就執行以下
    DATA: l_fname TYPE string VALUE '/tmp/'. "-----將檔案上傳至ALl1--
    l_fname = l_fname && 'W' && p_werks && sy-datum+2(6) && sy-uzeit+0(4) && '.csv'.
    OPEN DATASET l_fname FOR OUTPUT IN TEXT MODE 
                         ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH BYTE-ORDER MARK WITH SMART LINEFEED.
    IF sy-subrc EQ 0.
      LOOP AT it_data1 INTO wa_data1.
        TRANSFER wa_data1-char TO l_fname.
      ENDLOOP.
      CLOSE DATASET l_fname.
    ENDIF.

    l_command = 'cd /work_order'.                                 "選擇外部要連進的資料夾/work_order
    PERFORM ftp_command USING l_handle l_command.
    CLEAR l_command.
    CONCATENATE 'lcd' '/tmp/' INTO l_command SEPARATED BY space.  "選擇AL11(Local)的資料夾
    PERFORM ftp_command USING l_handle l_command.
    l_command = 'ascii'.                                          "選擇編碼方式(ASCII/BIANRY)
    PERFORM ftp_command USING l_handle l_command.
                                                                  "選擇AL11(Local)檔案
    CONCATENATE 'put W' p_werks sy-datum+2(6) sy-uzeit+0(4) '.csv' INTO l_command.
    PERFORM ftp_command USING l_handle l_command.
    IF sy-subrc EQ 0.                                             "如果轉至外部成功後就執行以下
      CALL FUNCTION 'FTP_DISCONNECT'                              "斷開連線
        EXPORTING
          handle = l_handle.
    ELSE.
      MESSAGE s398(00) WITH 'FTP path error' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    MESSAGE s398(00) WITH 'FTP path error: not connected' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  DELETE DATASET l_fname.                                       "將AL11(Local)檔案刪除
 ENDFORM.

 FORM ftp_command  USING p_handle p_command.
  CALL FUNCTION 'FTP_COMMAND'
    EXPORTING
      handle        = p_handle
      command       = p_command
    TABLES
      data          = it_result
    EXCEPTIONS
      tcpip_error   = 1
      command_error = 2
      data_error    = 3
      OTHERS        = 4.
 ENDFORM.
