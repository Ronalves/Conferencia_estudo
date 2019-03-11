*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_O01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module M_TELA_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_tela_9000 OUTPUT.

  SET PF-STATUS 'ST_9000'.

  CLEAR: gs_9000.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module M_TELA_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_tela_9100 OUTPUT.
  DATA:vl_mat TYPE matnr.
  DATA(io_util2) = NEW zclwm_tratamento( ).

  IF gs_ekko IS NOT INITIAL.
    gs_9100-matnr = io_util2->get_matnr( gs_ekko ).
    vl_mat = gs_9100-matnr(18).


    SELECT SINGLE maktg
             FROM makt
             INTO @DATA(lv_descricao)
             WHERE matnr = @vl_mat
              AND spras = @sy-langu.

    gs_9100-descricao = lv_descricao.
  ENDIF.
  SET PF-STATUS 'ST_9100'.

  IF gt_qtd_lidos[] IS INITIAL.

    IF vg_processo = 'VENDA'.

      io_util2->grp_carrega_dados_salvos(
        EXPORTING
          vg_palete     = vg_pallet       " Palete
          i_remessa     = gs_9200-rbnum   " Remessa
          i_pedido      = gs_9000-ref     " Pedido
        CHANGING
          lidos_remessa = lidos_remessa
          lidos_palete  = gs_9100_tot-qtd_lidos
      ).

      SELECT *
        FROM ztwm_sepa_imei_p AS a
        INTO TABLE tg_ztwm_sepa_imei
        WHERE ebeln = gs_9000-ref
        AND ot_conf = abap_false
*          AND conf  = abap_true
          AND tanum = space.

      IF sy-subrc = 0.
        LOOP AT tg_ztwm_sepa_imei INTO DATA(el_sep).
          IF gs_9200-rbnum = el_sep-rbnum.
            gs_qtd_lidos-matnr = gs_9100-matnr.
            gs_qtd_lidos-qtd_lidos = 1.
            gs_qtd_lidos-qtd = gs_9100-qtd_v.
            gs_qtd_lidos-serial = el_sep-imeicod1.

            APPEND gs_qtd_lidos TO gt_qtd_lidos.
            APPEND el_sep-imeicod1 TO tg_pistolagem.
          ENDIF.
        ENDLOOP.
      ENDIF.

      gs_9100-qtd_v = io_util2->get_qtd_remessa(
      rbnum = gs_9200-rbnum
      ).
    ELSEIF vg_processo = 'PEDIDO'.

      IF sy-ucomm <> 'CLEAR' AND sy-ucomm <> '9100ENTER'.
        SELECT SINGLE lidos_pedido
                 FROM ztwm_pedido_lid
                 INTO lidos_remessa
                 WHERE vbeln = gs_9000-ref.
      ENDIF.
      SELECT *
             FROM ztwm_sepa_imei_p AS a
             INTO TABLE tg_ztwm_sepa_imei
             WHERE ebeln = gs_9000-ref
               AND conf  = abap_true
               AND ot_conf = abap_false
               AND tanum = space.

      IF sy-subrc = 0.
        LOOP AT tg_ztwm_sepa_imei INTO el_sep.
          gs_qtd_lidos-matnr = gs_9100-matnr.
          gs_qtd_lidos-qtd_lidos = 1.
          gs_qtd_lidos-qtd = gs_9100-qtd_v.
          gs_qtd_lidos-serial = el_sep-imeicod1.

          APPEND gs_qtd_lidos TO gt_qtd_lidos.
          APPEND el_sep-imeicod1 TO tg_pistolagem.
        ENDLOOP.
      ENDIF.

*Quantidade faltante do Pallet
      DATA:vl_code TYPE c.
      vl_code = vg_pallet.
*      gs_9100_tot-qtd = io_util2->get_quantidade( i_matnr = gs_9100-matnr(18) code = vl_code ).
*      gs_9100_tot-qtd = gs_9100_tot-qtd - lidos_remessa.
    ENDIF.
  ENDIF.


  IF vg_processo = 'VENDA'.
    IF vg_parcial IS INITIAL.
      gs_9100_tot-qtd = gs_9100-qtd_v - gs_9100_tot-qtd_lidos.
      IF  gs_9100_tot-qtd < 0.
        CLEAR gs_9100_tot-qtd.
      ENDIF.
    ELSE.
      CLEAR vg_parcial.
    ENDIF.
  ENDIF.

  IF gs_ekko IS NOT INITIAL. " Pedido de transferência

    CASE sy-ucomm.
      WHEN '9000ENTER'.
        PERFORM f_enter_9000.
      WHEN '9100ENTER'.
        PERFORM f_select_lqua.
        PERFORM f_enter_valida_lqua_9100.
      WHEN 'FECHAVOL'.
        PERFORM checa_quant.
      WHEN OTHERS.
    ENDCASE.

  ELSEIF gt_t311a IS NOT INITIAL. " Grupo de remessas

    CASE sy-ucomm.
      WHEN '9200ENTER'.
        PERFORM f_grp_enter_9200.
      WHEN '9100ENTER'.
        PERFORM f_grp_enter_9200.
        PERFORM f_grp_enter_valida_lqua_9100.
      WHEN 'FECHAVOL'.
        PERFORM checa_quant.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module M_TELA_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_tela_9200 OUTPUT.

  SET PF-STATUS 'ST_9200'.

  CASE sy-ucomm.
    WHEN '9000ENTER' OR 'FECHAVOL'.
      PERFORM f_preenche_9200.
    WHEN '9200ENTER'.
      PERFORM f_enter_9200.
    WHEN 'BACK'.
      PERFORM f_back_9200.
    WHEN 'NEXT'.
      PERFORM f_next_9200.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module M_TRANSP_ITAB_OUT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_transp_itab_out OUTPUT.

  TRY.
      gv_idx = sy-stepl + gv_line.
      w_tela = gt_remessa[ gv_idx ].
    CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
  ENDTRY.

ENDMODULE.
