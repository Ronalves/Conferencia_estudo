*----------------------------------------------------------------------*
***INCLUDE ZWMR_CONFERENCIA_ATUAL_USERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  CASE sy-ucomm.

    WHEN 'OK_NEXT' OR '9200ENTER'.

      IF sy-ucomm = '9200ENTER'.
        IF vg_pallet IS INITIAL.
          EXIT.
        ENDIF.
      ENDIF.

      IF vg_pallet IS INITIAL.
        PERFORM f_exibir_msg USING 'ZWM_BR' '063' '' '' '' '' '0999'.
        EXIT.
      ELSE.
        CASE vg_pallet.
          WHEN '1'.
            IF gs_lety-lety1 IS NOT INITIAL.
              gs_9000-lety = gs_lety-lety1.
              IF vg_processo = 'VENDA'.
                gs_9100-qtd_v = io_zcl1->get_quantidade( code = '1' i_matnr = gs_9100-matnr(18) ).
              ELSE.
                gs_9100_tot-qtd = io_zcl1->get_quantidade( code = '1' i_matnr = gs_9100-matnr(18) ).
                gs_9100-qtd_v = io_zcl1->get_qtd_pedido( gs_9000-ref ).
              ENDIF.
            ELSE.
              PERFORM f_exibir_msg USING 'ZWM_BR' '072' '' '' '' '' '0999'.
              EXIT.
            ENDIF.
          WHEN '2'.
            IF gs_lety-lety2 IS NOT INITIAL.
              gs_9000-lety = gs_lety-lety2.
              IF vg_processo = 'VENDA'.
                gs_9100-qtd_v = io_zcl1->get_quantidade( code = '2' i_matnr = gs_9100-matnr(18) ).
              ELSE.
                gs_9100_tot-qtd = io_zcl1->get_quantidade( code = '2' i_matnr = gs_9100-matnr(18) ).
                gs_9100-qtd_v = io_zcl1->get_qtd_pedido( gs_9000-ref ).
              ENDIF.
            ELSE.
              PERFORM f_exibir_msg USING 'ZWM_BR' '072' '' '' '' '' '0999'.
              EXIT.
            ENDIF.
          WHEN '3'.
            IF gs_lety-lety3 IS NOT INITIAL.
              gs_9000-lety = gs_lety-lety3.
              IF vg_processo = 'VENDA'.
                gs_9100-qtd_v = io_zcl1->get_quantidade( code = '3' i_matnr = gs_9100-matnr(18) ).
              ELSE.
                gs_9100_tot-qtd = io_zcl1->get_quantidade( code = '3' i_matnr = gs_9100-matnr(18) ).
                gs_9100-qtd_v = io_zcl1->get_qtd_pedido( gs_9000-ref ).
              ENDIF.
            ELSE.
              PERFORM f_exibir_msg USING 'ZWM_BR' '072' '' '' '' '' '0999'.
              EXIT.
            ENDIF.
          WHEN OTHERS.
            PERFORM f_exibir_msg USING 'ZWM_BR' '064' '' '' '' '' '0999'.
            EXIT.
        ENDCASE.

        CALL SCREEN 9100.
      ENDIF.
    WHEN 'SAIR' OR 'BACK1'.

      IF gs_ekko IS NOT INITIAL.

        PERFORM f_dequeue_ezob_bloq_mat USING gs_9000-ref.

        CLEAR: gs_9000     ,
        gs_9100            ,
        gs_9100_tot        ,
        gs_ekko            ,
        gt_t311a           ,
        gt_lqua            ,
        gt_qtd_lidos       ,
        gt_item_tmp        ,
        gt_conf_tmp        ,
        gt_mat_serial_lote ,
        gv_volume          ,
        gv_active_qtd      .
      ELSE.

        CLEAR: gs_9100     ,
        gs_9100_tot        ,
        gs_9200-id         ,
        gs_9200-rbnum      ,
        gs_ekko            ,
        gt_qtd_lidos       ,
        gt_item_tmp        ,
        gt_conf_tmp        ,
        gt_mat_serial_lote ,
        gv_volume          ,
        gv_active_qtd      ,
        gv_material        .
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
