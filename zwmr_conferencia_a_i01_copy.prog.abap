*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_I01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9000 INPUT.

  DATA(io_zcl1) = NEW zclwm_tratamento( ).

  CASE sy-ucomm.
    WHEN '9000ENTER'.

      PERFORM f_seleciona_tvavr.

      PERFORM f_verificar_usuario CHANGING gv_erro.

      io_zcl1->verifica_ref(
        EXPORTING
         gs_lrf_wkqu2 = gs_lrf_wkqu
        IMPORTING
          re_error = gv_erro
        CHANGING
          gs_9000  = gs_9000
          gs_ekko  = gs_ekko
          gt_t311a = gt_t311a
        ).

      IF gv_erro IS NOT INITIAL.
        gv_msgv1 = gs_9000-ref.
        "Pedido/Grupo & Inválido
        PERFORM f_exibir_msg USING 'ZWM_BR' '002' gv_msgv1 '' '' '' '0999'.
      ENDIF.

      CHECK gv_erro IS INITIAL.

      IF gs_ekko IS NOT INITIAL.
        vg_processo = 'PEDIDO'.
        "Pedido de transferência
        PERFORM f_pedido_transferencia.
      ELSEIF gt_t311a IS NOT INITIAL.
        vg_processo = 'VENDA'.
        "Grupo de remessas
        PERFORM f_grupo_remessas.
      ENDIF.

    WHEN 'SAIR' OR 'BACK1'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9100 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR' OR 'BACK1'.
      vg_ref = gs_9000-ref.
      IF gs_ekko IS NOT INITIAL.

        PERFORM f_dequeue_ezob_bloq_mat USING gs_9000-ref.

        CLEAR: gs_9000            ,
               gs_9100            ,
               gs_9100_tot        ,
               gt_t311a           ,
               gt_lqua            ,
               gt_qtd_lidos       ,
               gt_item_tmp        ,
               gt_conf_tmp        ,
               gt_mat_serial_lote ,
               gv_volume          ,
               lidos_remessa,
               gv_active_qtd      .
      ELSE.

        CLEAR: gs_9100            ,
               gs_9100_tot        ,
               gs_9200-id         ,
               lidos_remessa,
               gt_qtd_lidos       ,
               gt_item_tmp        ,
               gt_conf_tmp        ,
               gt_mat_serial_lote ,
               gv_volume          ,
               gv_active_qtd      ,
               gv_material        .
      ENDIF.
      gs_9000-ref = vg_ref.
      LEAVE TO SCREEN 0.

    WHEN 'CLEAR'.

      IF vg_processo = 'VENDA'.
        COMMIT WORK AND WAIT.
        PERFORM f_select_lqua.
        CLEAR: gt_mat_serial_lote[],tg_pistolagem[].
        DATA(io_zcl2g) = NEW zclwm_tratamento( ).
        io_zcl2g->grp_reiniciar(
          EXPORTING
            i_remessa     = gs_9200-rbnum " Remessa
            i_pedido      = gs_9000-ref   " Pedido
            palete        = vg_pallet     " Palete
          CHANGING
            tg_lidos      = gt_qtd_lidos
            lidos_remessa = lidos_remessa
            lidos_palete  = gs_9100_tot-qtd_lidos ).
      ENDIF.

      IF vg_processo = 'PEDIDO'.
        DATA(io_zcl2) = NEW zclwm_tratamento( ).
        COMMIT WORK AND WAIT.
        PERFORM f_select_lqua.
        CLEAR: gt_mat_serial_lote[],tg_pistolagem[].
        io_zcl2->reiniciar_pedido(
        CHANGING
          tg_qtd_lidos    = gt_qtd_lidos
          tg_qtd_lidos_2  = gt_qtd_lidos_2
          lidos_remessa   = lidos_remessa
          gs_9100_tot     = gs_9100_tot
          vg_palete       = vg_pallet
          gs_9000         = gs_9000
          gs_9100         = gs_9100
          gs_9200         = gs_9200
          tg_lqua         = gt_lqua
          ).

      ENDIF.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_9200 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR' OR 'BACK1'.

      CLEAR: gs_9000            ,
             gs_9100            ,
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

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_TRANSP_ITAB_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_transp_itab_in INPUT.

  gv_lines = sy-loopc.
  gv_idx   = sy-stepl + gv_line.
  MODIFY gt_remessa FROM w_tela INDEX gv_idx.

ENDMODULE.
