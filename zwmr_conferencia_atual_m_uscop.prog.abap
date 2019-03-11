*----------------------------------------------------------------------*
***INCLUDE ZWMR_CONFERENCIA_ATUAL_M_USI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9400 INPUT.
  DATA:vl_matnr1 TYPE char18,
       vl_error  TYPE c.

  CASE sy-ucomm.

    WHEN 'SOK01'.

      DATA(io_zcl12) = NEW zclwm_tratamento( ).

      vl_matnr1 = gs_9100-matnr(18).

      io_zcl12->checa_peso(
        EXPORTING
          ref             = gs_9000-ref
          quantidade      = gs_9100_tot-qtd_lidos
          peso_cadastrado = vg_peso
          matnr           = vl_matnr1
        RECEIVING
          error           = vl_error
      ).

      IF vl_error IS NOT INITIAL.
        PERFORM f_exibir_msg USING 'ZWM_BR' '069' '' '' '' '' '0999'.
        vl_continue = abap_true.
        EXIT.
      ELSE.
        IF gs_ekko IS NOT INITIAL.
          PERFORM f_fechar_volume.
        ELSE.
          PERFORM f_grp_fechar_volume.
        ENDIF.

        IF vg_parcial IS NOT INITIAL.
          CALL SCREEN 9100.
        ENDIF.

        vl_continue = abap_true.
      ENDIF.

    WHEN 'SAIR' OR 'BACK1'.
      CLEAR vg_peso.
      MOVE: abap_true TO vl_continue.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
