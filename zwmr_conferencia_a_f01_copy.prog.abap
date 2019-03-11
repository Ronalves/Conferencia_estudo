*----------------------------------------------------------------------*
***INCLUDE ZWMR_PICKING_SEM_OT_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form F_VERIFICAR_USUARIO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verificar_usuario CHANGING c_erro.

  CLEAR: c_erro,
  gs_lrf_wkqu.

  SELECT SINGLE lgnum
  FROM lrf_wkqu
  INTO gs_lrf_wkqu
  WHERE bname = sy-uname
  AND statu = 'X'.

  IF sy-subrc IS NOT INITIAL.
    "USUÁRIO NÃO ENCONTRADO
    c_erro = abap_true.
    PERFORM f_exibir_msg USING 'ZWM_BR' '001' '' '' '' '' '0999' .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_EXIBIR_MSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_I_MSGID  text
*      -->P_I_MSGV1  text
*      -->P_I_MSGV2  text
*      -->P_I_MSGV  text
*      -->P_I_MSGV4  text
*      -->P_TELA  text
*&---------------------------------------------------------------------*
FORM f_exibir_msg  USING    p_msgid TYPE t100-arbgb
      p_msgno TYPE t100-msgnr
      p_msgv1 TYPE sprot_u-var1
      p_msgv2 TYPE sprot_u-var2
      p_msgv3 TYPE sprot_u-var3
      p_msgv4 TYPE sprot_u-var4
      p_tela.

  DATA: lv_answer TYPE c.

  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid          = p_msgid
      i_lang           = sy-langu
      i_msgno          = p_msgno
      i_msgv1          = p_msgv1
      i_msgv2          = p_msgv2
      i_msgv3          = p_msgv3
      i_msgv4          = p_msgv4
*     I_SEPERATE       = ' '
*     I_CONDENSE       = ' '
      i_message_screen = p_tela
*     I_LINE_SIZE      = 0
*     I_LINES          = 0
      i_non_lmob_envt  = 'X'
*     I_MODPL          =
    IMPORTING
      o_answer         = lv_answer
* TABLES
*     T_MSG_TEXT       =
    EXCEPTIONS
      invalid_message1 = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_REF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_ref CHANGING c_erro.

  CLEAR: c_erro,
  gs_ekko,
  gt_t311a.

*Grupo de remessas
  SELECT rbnum
  FROM t311a
  INTO TABLE gt_t311a
  WHERE lgnum = gs_lrf_wkqu-lgnum
  AND refnr = gs_9000-ref.


  IF sy-subrc IS NOT INITIAL.
*Pedido de transferência
    SELECT SINGLE ebeln
    FROM ekko
    INTO gs_ekko
    WHERE ebeln = gs_9000-ref.

    IF sy-subrc IS NOT INITIAL.
      gv_msgv1 = gs_9000-ref.
      "PEDIDO/GRUPO & INVÁLIDO
      c_erro = abap_true.
      PERFORM f_exibir_msg USING 'ZWM_BR' '002' gv_msgv1 '' '' '' '0999'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PEDIDO_TRANSFERENCIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pedido_transferencia .

  DATA:vl_matnr TYPE char18.
  PERFORM f_verifica_separacao USING gv_erro.

  CHECK gv_erro IS INITIAL.

  DATA(io_zcl1) = NEW zclwm_tratamento( ).

  gs_9100-matnr = io_zcl1->get_matnr( gs_ekko ).
  vl_matnr = gs_9100-matnr(18).


  SELECT SINGLE maktg
  FROM makt
  INTO @DATA(lv_descricao)
        WHERE matnr = @vl_matnr
        AND spras = @sy-langu.

  gs_9100-descricao = lv_descricao.

  CALL SCREEN 9300.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRUPO_REMESSAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grupo_remessas .

  DATA:eg_lips TYPE ty_lips.
  PERFORM f_verifica_separacao USING gv_erro.

  CHECK gv_erro IS INITIAL.

  PERFORM f_select_remessas.

  IF gt_remessa IS NOT INITIAL.
    CALL SCREEN 9200.
  ELSE.
    "NÃO EXISTE REMESSA PARA ESSE PEDIDO/GRUPO
    PERFORM f_exibir_msg USING 'ZWM_BR' '026' '' '' '' '' '0999'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENQUEUE_EMEKPOE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enqueue_emekpoe  USING    p_ebeln
      p_ebelp
CHANGING c_bloq.

  DATA: lv_ebelp TYPE ekpo-ebelp.

  CLEAR: c_bloq.

  lv_ebelp = p_ebelp.

  CALL FUNCTION 'ENQUEUE_EMEKPOE'
    EXPORTING
      mode_ekpo      = 'E'
      mandt          = sy-mandt
      ebeln          = p_ebeln
      ebelp          = lv_ebelp
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc IS NOT INITIAL.
    c_bloq = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENTER_9000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enter_9000 .

  PERFORM f_select_lqua.
*  PERFORM f_verifica_volume.
*  PERFORM f_habilita_campo_qtd USING abap_false CHANGING gv_active_qtd.

*Validar se outro usuário já esta com o item na tela
  PERFORM f_enqueue_ezob_bloq_mat USING gs_9000-ref CHANGING gv_bloq.
  IF gv_bloq = abap_true.
    gv_msgid = sy-msgid.
    gv_msgno = sy-msgno.
    PERFORM f_exibir_msg USING gv_msgid gv_msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 '0999'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_ENTER_9200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_enter_9200 .

  PERFORM f_select_lqua.
  PERFORM f_grp_verifica_volume.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENTER_VALIDA_LQUA_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enter_valida_lqua_9100 .

  DATA: lv_material       TYPE mara-matnr,
        lv_mat_conv       TYPE char18,
        exp_material      TYPE c LENGTH 6,
        exp_lote          TYPE c LENGTH 10,
        exp_serial        TYPE c LENGTH 20,
        imp_cod_barras    TYPE c LENGTH 35,
        el_ztwm_conf_tmp2 TYPE ztwm_conf_tmp2,
        lv_total_lidos    TYPE menge_d,
        imei_error        TYPE char18,
        vg_msgv1          TYPE sprot_u-var1,
        vg_msgv2          TYPE sprot_u-var2,
        p_msgid           TYPE t100-arbgb,
        p_msgno           TYPE t100-msgnr,
        p_msgv1           TYPE sprot_u-var1,
        p_msgv2           TYPE sprot_u-var2,
        p_tela            TYPE sy-dynnr.

  CLEAR: gv_erro,
  exp_material,
  exp_lote,
  exp_serial,
  imp_cod_barras.

  DATA(io_util) = NEW zclwm_tratamento( ).
  DATA(tg_imeis) = io_util->trata_imeis( gs_9100-matnr_v ).

*  Verifica Limite de Palete.
  IF io_util->check_limite_palete( i_qtd = gs_9100-qtd_v vg_palete = vg_pallet tg_meis = tg_imeis i_ref = gs_9000-ref i_matnr = gs_9100-matnr )
    IS NOT INITIAL.
    PERFORM f_exibir_msg USING 'ZWM_BR' '080' vg_msgv1 '' '' '' '0999'.
    CLEAR: gs_9100-matnr_v.
    SET CURSOR FIELD 'GS_9100-MATNR_V'.
    EXIT.
  ENDIF.

  SELECT matnr,
  gernr
  FROM ztwm_conf_tmp2
  INTO TABLE @DATA(tg_tmp2)
        FOR ALL ENTRIES IN @tg_imeis
        WHERE matnr = @gs_9100-matnr
        AND gernr = @tg_imeis-imei.

  lv_material = gs_9100-matnr.
  lv_mat_conv = lv_material(18).

  IF io_util->checa_imeis_antes_de_input(
  EXPORTING
    tg_imeis           = tg_imeis
    i_pedido           = gs_9000-ref
    gs_9000            = gs_9000
    lv_mat_conv        = lv_mat_conv
    gs_9100            = gs_9100
    tg_tmp2            = tg_tmp2
    gt_mat_serial_lote = gt_mat_serial_lote
    gs_9100_total      = gs_9100_tot
    gs_ekko            = gs_ekko
    gt_qtd_lidos       = gt_qtd_lidos
    lv_material        = lv_material
    gt_lqua            = gt_lqua
    gs_9100_tot        = gs_9100_tot
  IMPORTING
    p_msgid            =  p_msgid    " Área funcional
    p_msgno            = p_msgno    " Nº mensagem
    p_msgv1            = p_msgv1    " Variável mensagens
    p_tela             = p_tela    " Campo do sistema ABAP: nº de tela atual
    p_msgv2            = p_msgv2    " Variável mensagens
  CHANGING
    gs_lqua            = gs_lqua    " Quantos
    )
IS NOT INITIAL.
    PERFORM f_exibir_msg USING p_msgid p_msgno p_msgv1 p_msgv2 '' '' p_tela.
    CLEAR: gs_9100-matnr_v.
    SET CURSOR FIELD 'GS_9100-MATNR_V'.
    EXIT.
  ELSE.

    LOOP AT tg_imeis INTO DATA(el_imeis).
*Grava na tabela interna o lote e serial do material
*2-Verifico se já foi separado na tabela ZTWM_COLETOR onde Sepa = X.
      DATA(vl_error2) = io_util->checa_imei_separado(
            imei  = el_imeis-imei
            ref   = gs_9000-ref
            ).
      IF vl_error2 IS NOT INITIAL.
        vg_msgv1 = el_imeis-imei.
        PERFORM f_exibir_msg USING 'ZWM_BR' '071' vg_msgv1 '' '' '' '0999'.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        EXIT.
      ENDIF.

      DATA(vl_error) =  io_util->check_imei(
      EXPORTING
        i_imei       = el_imeis-imei
        i_material   = lv_mat_conv
      IMPORTING
        e_imei_error = imei_error ).
      IF vl_error IS NOT INITIAL.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        gv_erro = abap_true.
        "Imei não associado ao material
        vg_msgv1 = el_imeis-imei.
        vg_msgv2 = gs_9100-matnr.
        PERFORM f_exibir_msg USING 'ZWM_BR' '054' vg_msgv1 vg_msgv2 '' '' '0999'.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        EXIT.
      ENDIF.

*3-Verifico se IMEI já foi cadastrado na ztwm_conf_tmp2.
      vl_error =  io_util->verifica_imei_cadastrado(
      tg_tmp2 = tg_tmp2
      imei    = el_imeis-imei
      matnr   = gs_9100-matnr
      ).
      IF vl_error IS NOT INITIAL.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        gv_erro = abap_true.
        "Imei já Lido para esse Material
        vg_msgv1 = el_imeis-imei.
        vg_msgv2 = gs_9100-matnr.
        PERFORM f_exibir_msg USING 'ZWM_BR' '049' vg_msgv1 vg_msgv2 '' '' '0999'.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
      ELSE.
*4-Verifico de IMEI já foi lido na ztwm_sepa_imei_p
        SELECT *
        FROM ztwm_sepa_imei_p
        INTO TABLE @DATA(tg_ztwm_sepa_imei)
              WHERE ebeln = @gs_9000-ref
              AND imeicod1 = @el_imeis-imei
              AND conf     = @abap_true.
        IF sy-subrc = 0.
          "Imei já Lido para esse Material
          vg_msgv1 = el_imeis-imei.
          vg_msgv2 = gs_9100-matnr.
          PERFORM f_exibir_msg USING 'ZWM_BR' '049' vg_msgv1 vg_msgv2 '' '' '0999'.
          CLEAR: gs_9100-matnr_v.
          SET CURSOR FIELD 'GS_9100-MATNR_V'.
          EXIT.
        ELSE.
*1-Caso seja um ou mais Imeis armazeno tudo em tabela interna.
          LOOP AT tg_imeis INTO DATA(el_imeis2).
            APPEND el_imeis2 TO tg_pistolagem.
          ENDLOOP.
        ENDIF.

*5-Verifico se IMEI já foi lido para temporária gt_mat_serial_lote.
        READ TABLE gt_mat_serial_lote INTO DATA(el_serial) WITH KEY matnr =  gs_9100-matnr
              serial = el_imeis-imei.
        IF sy-subrc = 0.
          vg_msgv1 = el_serial-serial.
          vg_msgv2 = gs_9100-matnr.
          "Imei já Lido para esse Material
          PERFORM f_exibir_msg USING 'ZWM_BR' '049' vg_msgv1 vg_msgv2 '' '' '0999'.
          CLEAR: gs_9100-matnr_v.
          SET CURSOR FIELD 'GS_9100-MATNR_V'.
          EXIT.
        ENDIF.

*6-Apendo a leitura atual na gt_mat_serial_lote para que possa verificar se IMEI será utilizado
*nas próximas leituras a fim de evitar erro de ler mesmo IMEI.
        APPEND VALUE #( matnr  = gs_9100-matnr
        serial = gs_9100-matnr_v
        ) TO gt_mat_serial_lote.

        gs_9100-serial = el_imeis-imei.
      ENDIF.

*7-Checo se a quantidade lidos vai ultrapassar a quantidade existente no menge da EKPO.
      io_util->checa_limite_pedido_conf(
      EXPORTING
        gs_ekko      = gs_ekko
        gt_qtd_lidos = gt_qtd_lidos
        vl_matnr     = lv_material
        RECEIVING
        vl_error     = vl_error
        ).

*** check somente quando for pedido de transferencia
*    SELECT COUNT( * )
*    FROM ekko
*    WHERE ebeln = gs_ekko-ebeln.
*    IF sy-dbcnt > 0.
*      IF gs_9100_tot-qtd = 0.
*        vl_error = abap_true.
*      ENDIF.
*    ENDIF.

      IF vl_error IS NOT INITIAL.
        PERFORM f_exibir_msg USING 'ZWM_BR' '070' '' '' '' '' '0999'.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        EXIT.
      ENDIF.

*8-Validar Material.
      TRY.
          gs_lqua = gt_lqua[ matnr = gs_9100-matnr ].
*9-Validar o status do material é livre utilização pelo bestq = space.
          TRY.
              gs_lqua = gt_lqua[ matnr = gs_9100-matnr bestq = space ].
              SET CURSOR FIELD 'GS_9100-QTD_V'.
            CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
              CLEAR: gs_9100,
              gs_9100_tot-qtd,
              gs_9100-matnr_v.
              SET CURSOR FIELD 'GS_9100-MATNR_V'.
              gv_erro = abap_true.
              "Utilizar somente Material de Livre Utilização
              PERFORM f_exibir_msg USING 'ZWM_BR' '055' '' '' '' '' '0999'.
          ENDTRY.
        CATCH cx_sy_itab_line_not_found INTO lv_erro.
          CLEAR: gs_9100.
          SET CURSOR FIELD 'GS_9100-MATNR_V'.
          gv_erro = abap_true.
          "Material Inválido
          PERFORM f_exibir_msg USING 'ZWM_BR' '010' '' '' '' '' '0999'.
      ENDTRY.

      IF gv_erro           IS INITIAL AND
      gs_9100-matnr_v   IS NOT INITIAL.

*10-Sumariza mais um lido.
        gs_9100_tot-qtd_lidos = gs_9100_tot-qtd_lidos + 1.

*11-Indica na Sepamei que foi conferido.
        DATA:el_ztwm_sepa_imei_p TYPE ztwm_sepa_imei_p.
        SELECT SINGLE * FROM ztwm_sepa_imei_p INTO el_ztwm_sepa_imei_p WHERE ebeln = gs_9000-ref
        AND imeicod1 = el_imeis-imei.
        el_ztwm_sepa_imei_p-ebeln = gs_9000-ref.
        el_ztwm_sepa_imei_p-imeicod1 = el_imeis-imei.
        el_ztwm_sepa_imei_p-conf = abap_true.
        MODIFY ztwm_sepa_imei_p FROM el_ztwm_sepa_imei_p.
        COMMIT WORK AND WAIT.
*---------------------------------------------------------------------------
*12-Atualiza quantidade de lidos da remessa.
        DATA:el_ztwm_pedido_lid TYPE ztwm_pedido_lid.
        el_ztwm_pedido_lid-vbeln = gs_9000-ref.
        ADD 1 TO lidos_remessa.
        el_ztwm_pedido_lid-lidos_pedido = lidos_remessa.
        MODIFY ztwm_pedido_lid FROM el_ztwm_pedido_lid.
*---------------------------------------------------------------------------
*13-Atualiza quantidade de lidos do palete.
        DATA:eg_ztwm_pale_p_lid TYPE ztwm_pale_p_lid.
        eg_ztwm_pale_p_lid-palete = vg_pallet.
        eg_ztwm_pale_p_lid-vbeln  = gs_9000-ref.
        eg_ztwm_pale_p_lid-lidos_palete = 1.
        eg_ztwm_pale_p_lid-imei = el_imeis-imei.
        MODIFY ztwm_pale_p_lid FROM eg_ztwm_pale_p_lid.
*---------------------------------------------------------------------------
        COMMIT WORK.
*14-Apenda lidos.
        APPEND VALUE #( matnr     = gs_9100-matnr
        qtd       = gs_lqua-verme
        qtd_lidos = 1
        serial    = gs_9100-serial ) TO gt_qtd_lidos.
*15-Apenda indicando que foi lido neste momento
        APPEND VALUE #( matnr     = gs_9100-matnr
        qtd       = gs_lqua-verme
        qtd_lidos = 1
        serial    = gs_9100-serial ) TO gt_qtd_lidos_2.

*16-Subtrai menos um da quantidade faltante do Palete.
*      gs_9100_tot-qtd = gs_9100_tot-qtd - 1.

      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT  tg_pistolagem[].
  DELETE ADJACENT DUPLICATES FROM tg_pistolagem COMPARING ALL FIELDS.
  CLEAR gs_9100-matnr_v.
  SET CURSOR FIELD 'GS_9100-MATNR_V'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_ENTER_VALIDA_LQUA_9100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_enter_valida_lqua_9100 .

  DATA: lv_material     TYPE mara-matnr,
        exp_material    TYPE c LENGTH 6,
        exp_lote        TYPE c LENGTH 10,
        exp_serial      TYPE c LENGTH 20,
        imp_cod_barras  TYPE c LENGTH 35,
        lv_total_lidos  TYPE lfimg,
        lv_total_lidos2 TYPE lfimg,
        lv_quant        TYPE lfimg,
        lv_flag         TYPE c,
        imei_error      TYPE char18,
        vg_msgv1        TYPE sprot_u-var1,
        vg_msgv2        TYPE sprot_u-var2,
        lv_mat_conv     TYPE char18,
        p_msgid         TYPE t100-arbgb,
        p_msgno         TYPE t100-msgnr,
        p_msgv1         TYPE sprot_u-var1,
        p_msgv2         TYPE sprot_u-var2,
        p_tela          TYPE sy-dynnr.

  CLEAR: gv_erro,
  exp_material,
  exp_lote,
  exp_serial,
  imp_cod_barras.

  DATA(io_util) = NEW zclwm_tratamento( ).

*1-Caso seja um ou vários Imeis de uma vez armazeno em linhas de tab interna
  DATA(tg_imeis) = io_util->trata_imeis( gs_9100-matnr_v ).

  IF gs_9100-matnr_v IS NOT INITIAL.

    SELECT matnr,
           gernr
           FROM ztwm_conf_tmp2
           INTO TABLE @DATA(tg_tmp2)
           FOR ALL ENTRIES IN @tg_imeis
           WHERE matnr = @gs_9100-matnr
           AND gernr = @tg_imeis-imei.

    lv_material = gs_9100-matnr.

    DESCRIBE TABLE tg_imeis LINES DATA(vl_lines).

    IF io_util->checa_limite_remessa(
    i_remessa    =  gs_9200-rbnum
    lv_tot_lidos = lidos_remessa
    vl_lines = vl_lines
    ) IS NOT INITIAL.
*QUANTIDADE DO PEDIDO ATINGIDA.
      PERFORM f_exibir_msg USING 'ZWM_BR' '081' '' '' '' '' '0999'.
      EXIT.
    ENDIF.

    IF io_util->grpcheca_imeis_antes_de_input(
       EXPORTING
         tg_imeis           = tg_imeis
         i_pedido           = gs_9000-ref   " Nº do documento de compras
         gs_9000            = gs_9000
         lv_mat_conv        = lv_mat_conv  " Campo de comprimento 18
         gs_9100            = gs_9100
         tg_tmp2            = tg_tmp2    " Controle de Volumes - Temporária
         gt_mat_serial_lote = gt_mat_serial_lote
*         gs_9100_total      = gs_9100_total
         gs_ekko            = gs_ekko    " Cabeçalho do documento de compra
         gt_qtd_lidos       = gt_qtd_lidos
         lv_material        = lv_material    " Nº do material
         gt_lqua            = gt_lqua
         gs_9100_tot        = gs_9100_tot
         gs_9200            = gs_9200
         lv_total_lidos     = lv_total_lidos
         lt_calc_volume     = lt_calc_volume
       IMPORTING
         p_msgid            = p_msgid     " Área funcional
         p_msgno            = p_msgno     " Nº mensagem
         p_msgv1            = p_msgv1   " Variável mensagens
         p_tela             = p_tela   " Campo do sistema ABAP: nº de tela atual
         p_msgv2            = p_msgv2  " Variável mensagens
       CHANGING
         gs_lqua            = gs_lqua   " Quantos
    ) IS NOT INITIAL.
      CLEAR: gs_9100-matnr_v.
      SET CURSOR FIELD 'GS_9100-MATNR_V'.
      PERFORM f_exibir_msg USING p_msgid p_msgno p_msgv1 p_msgv2 '' '' p_tela.
      EXIT.

    ENDIF.

    LOOP AT tg_imeis INTO DATA(el_imeis).
      DATA(vl_tabix) = sy-tabix.

*Grava na tabela interna o lote e serial do material
*2-Aqui verifico se Imei já foi separado na Tab ztwm_coletor Onde o campo Sepa é = X.
      DATA(vl_error2) = io_util->checa_imei_separado( imei = el_imeis-imei ref = gs_9000-ref ).
      IF vl_error2 IS NOT INITIAL.
        vg_msgv1 = el_imeis-imei.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        PERFORM f_exibir_msg USING 'ZWM_BR' '071' vg_msgv1 '' '' '' '0999'.
        EXIT.
      ENDIF.

*3-Verifica se IMEI já foi utilizado.
      SELECT *
            FROM ztwm_sepa_imei_p
            INTO TABLE @DATA(tg_ztwm_sepa_imei)
            WHERE ebeln = @gs_9000-ref
            AND imeicod1 = @el_imeis-imei
            AND conf     = @abap_true.
      IF sy-subrc = 0.
        "Imei já Lido para esse Material
        vg_msgv1 = el_imeis-imei.
        vg_msgv2 = gs_9100-matnr.
        CLEAR: gs_9100-matnr_v.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
        PERFORM f_exibir_msg USING 'ZWM_BR' '049' vg_msgv1 vg_msgv2 '' '' '0999'.
        EXIT.
      ELSE.
        LOOP AT tg_imeis INTO DATA(el_imeis2).
          APPEND el_imeis2 TO tg_pistolagem.
        ENDLOOP.
      ENDIF.

      gs_9100-serial = el_imeis-imei.

*4-Checa Limite do Grupo.
      io_util->checa_limite_pedido_grup(
      EXPORTING
        ebeln        = gs_9000-ref
        gt_qtd_lidos = gt_qtd_lidos
        vl_matnr     = gs_9100-matnr   " Nº do material
        RECEIVING
        vl_error     = DATA(vl_error)
        ).
      IF vl_error IS NOT INITIAL.
*QUANTIDADE DO PEDIDO ATINGIDA.
        PERFORM f_exibir_msg USING 'ZWM_BR' '070' '' '' '' '' '0999'.
        EXIT.
      ENDIF.

*5-Controle de registros lidos.
      APPEND VALUE #( matnr  = gs_9100-matnr
      serial = gs_9100-matnr_v
      ) TO gt_mat_serial_lote.

      IF gv_erro        IS INITIAL AND
      gs_9100-matnr_v   IS NOT INITIAL.

*6-Verificar se a quantidade lida é maior que a separada por material
        ADD 1 TO lv_total_lidos.
        CLEAR: lv_flag.
        LOOP AT lt_calc_volume INTO DATA(ls_calc_volume) WHERE matnr = gs_9100-matnr AND ztp_vol = 'M'.
          IF lv_total_lidos > ls_calc_volume-qtd_unidade.
            DELETE gt_mat_serial_lote WHERE matnr = gs_9100-matnr AND serial = gs_9100-matnr_v.
            "QUANTIDADE LIDA MAIOR QUE QUANTIDADE DO VOLUME
            CLEAR: gs_9100.
            SET CURSOR FIELD 'GS_9100-MATNR_V'.
            gv_erro = abap_true.
            PERFORM f_exibir_msg USING 'ZWM_BR' '028' '' '' '' '' '0999'.
          ELSE.
            IF ls_calc_volume-qtd_volume > ls_calc_volume-vol_fechado.
              IF lv_total_lidos <= ls_calc_volume-qtd_unidade.
                lv_flag = abap_true.
                EXIT.
              ELSE.
                DELETE gt_mat_serial_lote WHERE matnr = gs_9100-matnr AND serial = gs_9100-matnr_v.
                "FECHAR VOLUME
                CLEAR: gs_9100.
                gv_erro = abap_true.
                PERFORM f_exibir_msg USING 'ZWM_BR' '029' '' '' '' '' '0999'.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CHECK gv_erro IS INITIAL.

        IF lv_flag IS INITIAL.
          READ TABLE lt_calc_volume INTO ls_calc_volume WITH KEY matnr = gs_9100-matnr ztp_vol = 'I'.
          IF sy-subrc IS INITIAL.
            IF lv_total_lidos > ( ls_calc_volume-qtd_unidade * ls_calc_volume-qtd_volume ).
              DELETE gt_mat_serial_lote WHERE matnr = gs_9100-matnr AND serial = gs_9100-matnr_v.
              "QUANTIDADE LIDA MAIOR QUE QUANTIDADE DO VOLUME
              CLEAR: gs_9100.
              SET CURSOR FIELD 'GS_9100-MATNR_V'.
              gv_erro = abap_true.
              PERFORM f_exibir_msg USING 'ZWM_BR' '028' '' '' '' '' '0999'.
            ENDIF.
          ENDIF.
        ENDIF.

        CHECK gv_erro IS INITIAL.
        SET CURSOR FIELD 'GS_9100-MATNR_V'.
      ENDIF.

      IF gv_erro        IS INITIAL AND
      gs_9100-matnr_v   IS NOT INITIAL.

        DATA:el_ztwm_sepa_imei_p TYPE ztwm_sepa_imei_p.
        SELECT SINGLE * FROM ztwm_sepa_imei_p INTO el_ztwm_sepa_imei_p WHERE ebeln = gs_9000-ref
        AND imeicod1 = el_imeis-imei.
        el_ztwm_sepa_imei_p-ebeln = gs_9000-ref.
        el_ztwm_sepa_imei_p-imeicod1 = el_imeis-imei.
        el_ztwm_sepa_imei_p-conf = abap_true.
        el_ztwm_sepa_imei_p-mandt = sy-mandt.
        el_ztwm_sepa_imei_p-rbnum = gs_9200-rbnum.
        MODIFY ztwm_sepa_imei_p FROM el_ztwm_sepa_imei_p.
        COMMIT WORK AND WAIT.

        DATA:eg_ztwm_conf_venda TYPE ztwm_conf_venda.
        eg_ztwm_conf_venda-ebeln = gs_9000-ref.
        eg_ztwm_conf_venda-imei = el_imeis-imei.
        eg_ztwm_conf_venda-mandt = sy-mandt.
        eg_ztwm_conf_venda-palete = vg_pallet.
        eg_ztwm_conf_venda-quantidade = 1.
        eg_ztwm_conf_venda-vbeln = gs_9200-rbnum.
        ADD 1 TO lidos_remessa.
        ADD 1 TO gs_9100_tot-qtd_lidos.
        MODIFY ztwm_conf_venda FROM eg_ztwm_conf_venda.
        COMMIT WORK.

        APPEND VALUE #( matnr     = gs_9100-matnr
        qtd       = gs_lqua-verme
        qtd_lidos = 1
        serial    = gs_9100-serial ) TO gt_qtd_lidos.

        APPEND VALUE #( matnr     = gs_9100-matnr
        qtd       = gs_lqua-verme
        qtd_lidos = 1
        serial    = gs_9100-serial ) TO gt_qtd_lidos_2.

      ENDIF.

    ENDLOOP.
    SORT  tg_pistolagem[].
    DELETE ADJACENT DUPLICATES FROM tg_pistolagem COMPARING ALL FIELDS.
    CLEAR gs_9100-matnr_v.
    SET CURSOR FIELD 'GS_9100-MATNR_V'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SELECT_LQUA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_select_lqua .

  CLEAR: gt_lqua.

  SELECT *
  FROM lqua
  INTO TABLE @gt_lqua
  WHERE lgnum = @gs_lrf_wkqu-lgnum
  AND lgpla = @gs_9000-ref
  AND lgtyp = @gv_lgtyp "Substituição por TVARV
  AND bestq = @space
  AND verme > 0.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_SEPARACAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_verifica_separacao CHANGING c_erro.
  DATA:el_ztwm_saldo_conf TYPE ztwm_saldo_conf.
  CLEAR: c_erro.

  PERFORM f_verificar_usuario CHANGING gv_erro.

*VERIFICA SE O PEDIDO FOI SEPARADO
  SELECT mandt, matnr, bestq, verme
  FROM lqua
  INTO TABLE @DATA(ls_lqua)
        WHERE lgnum = @gs_lrf_wkqu-lgnum
        AND lgpla = @gs_9000-ref
        AND lgtyp = @gv_lgtyp "'CF1' "Tipo Depósito Conferência
        AND verme > 0.

  IF sy-subrc IS NOT INITIAL.
    gv_msgv1 = gs_9000-ref.
    "PEDIDO/GRUPO NÃO SEPARADO
    c_erro = abap_true.
    PERFORM f_exibir_msg USING 'ZWM_BR' '015' gv_msgv1 '' '' '' '0999'.
  ELSE.


*  Ajuste da conferencia (Quando o primeiro passo é feito pelo Standard (LT0E).
    SELECT * FROM ztwm_saldo_conf
    INTO TABLE @DATA(lt_saldo_trans)
          WHERE ebeln = @gs_9000-ref.

    IF lt_saldo_trans[] IS INITIAL.

      LOOP AT ls_lqua INTO DATA(ws_lqua).


        APPEND VALUE #( mandt = ws_lqua-mandt
        ebeln = gs_9000-ref
        ebelp = sy-tabix
        matnr = ws_lqua-matnr
        bestq = ws_lqua-bestq
        menge = ws_lqua-verme ) TO lt_saldo_trans.

      ENDLOOP.
      MODIFY ztwm_saldo_conf FROM TABLE lt_saldo_trans.

    ELSE.

      READ TABLE ls_lqua INTO ws_lqua INDEX 1.
      LOOP AT lt_saldo_trans INTO DATA(el_saldo_trans).
        DATA(vl_tabix) = sy-tabix.
        el_saldo_trans-bestq = ws_lqua-bestq.
        el_saldo_trans-menge = ws_lqua-verme.
        MODIFY lt_saldo_trans FROM el_saldo_trans INDEX vl_tabix.
      ENDLOOP.

      MODIFY ztwm_saldo_conf FROM TABLE lt_saldo_trans.
      COMMIT WORK.

    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_HABILITA_CAMPO_QTD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_habilita_campo_qtd USING p_x CHANGING c_active_qtd.

  c_active_qtd = p_x.

  IF p_x IS NOT INITIAL.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'GS_9100-QTD_V'.
          screen-output    = 1.
          screen-active    = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'GS_9100-QTD_V'.
          screen-output    = 0.
          screen-active    = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_VOLUME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_verifica_volume .

*VOLUME
  SELECT MAX( anzpk )
  FROM ztwm_conf_tmp
  INTO gv_volume
  WHERE ebeln = gs_9000-ref.

  IF gv_volume IS INITIAL.
    gv_volume = 1.
    gs_9100_tot-vol_de  = gv_volume.
    gs_9100_tot-vol_ate = gv_volume.
  ELSE.
    gv_volume = gv_volume + 1.
    gs_9100_tot-vol_de  = gv_volume .
    gs_9100_tot-vol_ate = gv_volume .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENQUEUE_EZOB_BLOQ_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_enqueue_ezob_bloq_mat  USING p_refnr
CHANGING c_bloq.
  CLEAR: c_bloq.

  CALL FUNCTION 'ENQUEUE_EZOB_BLOQ_MAT'
    EXPORTING
      mode_ztwm_bloq_mat = 'E'
      mandt              = sy-mandt
      refnr              = p_refnr
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    c_bloq = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DEQUEUE_EZOB_BLOQ_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_dequeue_ezob_bloq_mat USING p_refnr.

  CALL FUNCTION 'DEQUEUE_EZOB_BLOQ_MAT'
    EXPORTING
      mode_ztwm_bloq_mat = 'E'
      mandt              = sy-mandt
      refnr              = p_refnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_QTD_FALTANTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_verifica_qtd_faltante USING p_matnr CHANGING c_qtd_faltante.

  DATA: lv_qtd_faltante TYPE p LENGTH 13.

  CLEAR: c_qtd_faltante.

  LOOP AT gt_lqua INTO DATA(ls_lqua) WHERE matnr = p_matnr.
    lv_qtd_faltante = lv_qtd_faltante + ls_lqua-verme.
  ENDLOOP.

  c_qtd_faltante = lv_qtd_faltante.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_VERIFICA_QTD_FALTANTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_grp_verifica_qtd_faltante USING p_matnr CHANGING c_qtd_faltante.

  DATA: lv_qtd_faltante TYPE p LENGTH 13.

  CLEAR: c_qtd_faltante.

  SELECT SUM( lfimg )
  FROM ztwm_conf_tmp
  INTO @DATA(lv_lfimg)
        WHERE ebeln = @gs_9000-ref
        AND vbeln = @gs_9200-rbnum
        AND matnr = @p_matnr.

  LOOP AT gt_lips INTO DATA(ls_lips) WHERE matnr = p_matnr.
    lv_qtd_faltante = lv_qtd_faltante + ls_lips-lfimg.
  ENDLOOP.

  IF lv_qtd_faltante IS NOT INITIAL.
    lv_qtd_faltante = lv_qtd_faltante - lv_lfimg.
  ENDIF.

  READ TABLE gt_lqua INTO gs_lqua WITH KEY matnr = p_matnr bestq = space.
  IF sy-subrc IS INITIAL.
    IF gs_lqua-verme < lv_qtd_faltante.
      lv_qtd_faltante = gs_lqua-verme.
    ENDIF.
  ENDIF.

  c_qtd_faltante = lv_qtd_faltante.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ADD_ITEM_TMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_add_item_tmp  USING    p_ebeln
      p_ebelp
      p_menge.

  READ TABLE gt_item_tmp INTO DATA(ls_item_tmp) WITH KEY ebeln = p_ebeln ebelp = p_ebelp.
  IF sy-subrc IS INITIAL.

    DATA(lv_tabix) = sy-tabix.
    ls_item_tmp-menge = ls_item_tmp-menge +  p_menge.
    MODIFY gt_item_tmp FROM ls_item_tmp INDEX lv_tabix.

  ELSE.

    APPEND VALUE #( ebeln = p_ebeln
    ebelp = p_ebelp
    menge = p_menge ) TO gt_item_tmp.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ADD_CONF_TMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_add_conf_tmp  USING    p_ebeln
      p_vbeln
      p_ebelp
      p_matnr
      p_lote
      p_serial
      p_volume
      p_qtd_lidos
      p_meins.

  APPEND VALUE #( ebeln = p_ebeln
  vbeln = p_vbeln
  ebelp = p_ebelp
  matnr = p_matnr
  charg = p_lote
  gernr = p_serial
  anzpk = p_volume
  lfimg = p_qtd_lidos
  meins = p_meins ) TO gt_conf_tmp.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_FECHAR_VOLUME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_fechar_volume .

  DATA: lt_item            TYPE TABLE OF ztwm_saldo_conf,
        lv_lgnum           TYPE ltak-lgnum,
        lv_bwlvs           TYPE ltak-bwlvs,
        lv_benum           TYPE ltak-benum,
        lv_lznum           TYPE ltak-lznum,
        lv_lfimg           TYPE lfimg,
        vl_menge           TYPE ztwm_saldo_conf-menge,
        gt_ztwm_coletor    TYPE TABLE OF ztwm_coletor,
        gt_ztwm_coletor_it TYPE TABLE OF ztwm_coletor_it,
        gt_ztwm_coletor_he TYPE TABLE OF ztwm_coletor_he.

  IF gt_qtd_lidos IS INITIAL.
    "Não foi Conferido nennhum Item
    PERFORM f_exibir_msg USING 'ZWM_BR' '024' '' '' '' '' '0999' .
    SET SCREEN 9100.
    LEAVE SCREEN.
  ELSE.

    PERFORM f_select_lqua.
    PERFORM f_grp_verifica_volume.
    PERFORM f_verifica_volume.

    CLEAR: gt_item_tmp,
    gt_conf_tmp,
    gt_ltap_creat.

    SELECT *
    FROM ztwm_saldo_conf
    INTO TABLE lt_item
    WHERE ebeln = gs_9000-ref
    AND menge > 0.

    READ TABLE lt_item INTO DATA(el_item) INDEX 1.
    IF sy-tabix = 0.
      CLEAR lt_item[].
      APPEND el_item TO lt_item.
    ENDIF.

    IF lt_item IS NOT INITIAL.

      SORT lt_item BY ebeln matnr ebelp ASCENDING.

*LIVRE UTILIZAÇÃO
      SORT gt_qtd_lidos.
      DELETE ADJACENT DUPLICATES FROM gt_qtd_lidos COMPARING ALL FIELDS.
      LOOP AT gt_qtd_lidos INTO gs_qtd_lidos.

        DATA(lv_tabix) = sy-tabix.

        LOOP AT lt_item INTO DATA(ls_item) WHERE matnr = gs_qtd_lidos-matnr AND bestq = space.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY matnr = gs_qtd_lidos-matnr.
          IF sy-subrc IS INITIAL.
            CLEAR vl_menge.
            vl_menge = gs_qtd_lidos-qtd_lidos.              "* 10000.

            IF vl_menge >= ls_item-menge.

              PERFORM f_add_item_tmp USING ls_item-ebeln
                    ls_item-ebelp
                    ls_item-menge.

              PERFORM f_add_conf_tmp USING ls_item-ebeln
                    ''
                    ls_item-ebelp
                    gs_qtd_lidos-matnr
                    space "ls_mat_serial_lote-lote
                    space "ls_mat_serial_lote-serial
                    gv_volume
                    ls_item-menge
                    gs_lqua-meins.

            ELSE.

              PERFORM f_add_item_tmp USING ls_item-ebeln
                    ls_item-ebelp
                    gs_qtd_lidos-qtd_lidos.

              PERFORM f_add_conf_tmp USING ls_item-ebeln
                    ''
                    ls_item-ebelp
                    gs_qtd_lidos-matnr
                    space "ls_mat_serial_lote-lote
                    space "ls_mat_serial_lote-serial
                    gv_volume
                    gs_qtd_lidos-qtd_lidos
                    gs_lqua-meins.

              CLEAR: gs_qtd_lidos-qtd_lidos.

            ENDIF.
          ENDIF.

          MODIFY gt_qtd_lidos FROM gs_qtd_lidos INDEX lv_tabix.

          IF gs_qtd_lidos-qtd_lidos IS INITIAL.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM gt_qtd_lidos COMPARING matnr.

      LOOP AT gt_qtd_lidos INTO gs_qtd_lidos.

        CLEAR: lv_lfimg.
        LOOP AT gt_conf_tmp INTO DATA(ls_conf_tmp) WHERE matnr = gs_qtd_lidos-matnr.
          ADD ls_conf_tmp-lfimg TO lv_lfimg.
        ENDLOOP.

        READ TABLE gt_conf_tmp INTO gs_conf_tmp WITH KEY matnr = gs_qtd_lidos-matnr.
        IF sy-subrc IS INITIAL.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY matnr = gs_conf_tmp-matnr.
          IF sy-subrc IS INITIAL.

            lv_lgnum = gs_lrf_wkqu-lgnum.
            lv_benum = gs_lqua-lgpla.
            lv_lznum = gv_volume.
            lv_bwlvs = '951'.

            APPEND VALUE #(  matnr  = gs_lqua-matnr
            werks  = gs_lqua-werks
            lgort  = gs_lqua-lgort
            charg  = gs_lqua-charg
            anfme  = lv_lfimg
            altme  = gs_lqua-meins
            squit  = abap_true
            vltyp  = 'CF1'
            vlpla  = gs_lqua-lgpla ) TO gt_ltap_creat.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA(io_util) = NEW zclwm_tratamento( ).
      CLEAR gv_erro.
      IF gs_ekko IS NOT INITIAL.
        "Pedido de transferência
        io_util->checa_limite_pedido(
        EXPORTING
          lfimg    = lv_lfimg
        CHANGING
          gs_ekko  = gs_ekko    " Cabeçalho do documento de compra
          RECEIVING
          vg_error = gv_erro
          ).
      ELSEIF gt_t311a IS NOT INITIAL.
        "Grupo de remessas

      ENDIF.

      IF gv_erro IS NOT INITIAL.
        PERFORM f_exibir_msg USING 'ZWM_BR' '052' '' '' '' '' '0999' .
        EXIT.
      ENDIF.

      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum                = lv_lgnum
          i_bwlvs                = lv_bwlvs
          i_benum                = lv_benum
          i_lznum                = lv_lznum
          i_commit_work          = 'X'
          i_bname                = sy-uname
        TABLES
          t_ltap_creat           = gt_ltap_creat
        EXCEPTIONS
          no_to_created          = 1
          bwlvs_wrong            = 2
          betyp_wrong            = 3
          benum_missing          = 4
          betyp_missing          = 5
          foreign_lock           = 6
          vltyp_wrong            = 7
          vlpla_wrong            = 8
          vltyp_missing          = 9
          nltyp_wrong            = 10
          nlpla_wrong            = 11
          nltyp_missing          = 12
          rltyp_wrong            = 13
          rlpla_wrong            = 14
          rltyp_missing          = 15
          squit_forbidden        = 16
          manual_to_forbidden    = 17
          letyp_wrong            = 18
          vlpla_missing          = 19
          nlpla_missing          = 20
          sobkz_wrong            = 21
          sobkz_missing          = 22
          sonum_missing          = 23
          bestq_wrong            = 24
          lgber_wrong            = 25
          xfeld_wrong            = 26
          date_wrong             = 27
          drukz_wrong            = 28
          ldest_wrong            = 29
          update_without_commit  = 30
          no_authority           = 31
          material_not_found     = 32
          lenum_wrong            = 33
          matnr_missing          = 34
          werks_missing          = 35
          anfme_missing          = 36
          altme_missing          = 37
          lgort_wrong_or_missing = 38
          error_message          = 39
          OTHERS                 = 40.

      IF sy-subrc <> 0.

        gv_msgid = sy-msgid.
        gv_msgno = sy-msgno.

        PERFORM f_exibir_msg USING gv_msgid gv_msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 '0999'.

      ELSEIF sy-subrc IS INITIAL.

        DATA(vl_ot) = sy-msgv1.
        LOOP AT tg_ztwm_sepa_imei INTO DATA(el_imei).
          el_imei-tanum = vl_ot.
          MODIFY tg_ztwm_sepa_imei FROM el_imei INDEX sy-tabix.
        ENDLOOP.
        MODIFY ztwm_sepa_imei_p FROM TABLE tg_ztwm_sepa_imei.


        DATA: lv_number_range(7)   TYPE n,    "-- Variable to hold Newly generated Number Range
              lv_year              TYPE inri-toyear,              "-- Variable to hold Year
              lv_subobject         TYPE  zeseqnum,     "-- Variable to hold Company Code
              lv_rc                TYPE inri-returncode,            "-- Variable to hold the Return Code
              lv_number_range2(10) TYPE c.

        lv_year = sy-datum(4).         "--- Assign Current Year
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'                "-- This hold the Newly generated Number
            object                  = 'ZWM0002'       "---- Passing the Number Range Object
            subobject               = lv_subobject    "-- Pass the Company code
            toyear                  = lv_year            "-- Pass the Year
          IMPORTING
            number                  = lv_number_range "-- Newly generated Number
            returncode              = lv_rc                   "-- The Return Code Number
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        lv_number_range2 = |PLT{ lv_number_range }|.

*------------------------------------------------------------------------------------------------------
        DATA(lv_tanum) = vl_ot.
        DATA(lv_werks) = gs_lqua-werks.
        DATA(lv_lgort) = gs_lqua-lgort.
        DATA(lv_vltyp) = gs_lqua-lgtyp.
        DATA(lv_vlpla) = gs_lqua-lgpla.
        DATA(lv_anfme) = ls_item-menge.
        DATA: lv_vgbel TYPE lips-vgbel.
        DATA: lv_vbeln TYPE lips-vbeln.

        READ TABLE gt_lqua INTO DATA(ls_lqua) INDEX 1.
        IF sy-subrc EQ 0.
          lv_vgbel = ls_lqua-lgpla.

          SELECT SINGLE vbeln
          FROM lips
          INTO lv_vbeln
          WHERE vgbel EQ lv_vgbel.

        ENDIF.

        SELECT *
               FROM ztwm_sepa_imei_p
               INTO TABLE @DATA(tg_ztwm_sepa_imei)
               WHERE ebeln = @gs_9000-ref
               AND conf  = @abap_true
               AND ot_conf = @space.
        IF sy-subrc EQ 0.

          LOOP AT tg_ztwm_sepa_imei ASSIGNING FIELD-SYMBOL(<fs_ztwm_sepa_imei>).
            <fs_ztwm_sepa_imei>-ot_conf = abap_true.
          ENDLOOP.

        ENDIF.

        APPEND VALUE #( mandt = sy-mandt
        betyp = gs_lqua-betyp
        benum = gs_9000-ref
        vbeln = lv_vbeln
        tanum = lv_tanum
        lgnum = lv_lgnum
        werks = lv_werks
        lgort = lv_lgort
        usuario = sy-uname
        data    = sy-datum
              peso    = vg_peso
              hora    = sy-uzeit ) TO gt_ztwm_coletor.
*------------------------------------------------------------------------
*        Atualiza Peso
        DATA:vg_conv TYPE char10.
        vg_conv = vg_peso.
        io_util->atualiza_peso(
        EXPORTING
          i_remessa = lv_vbeln
          peso_tela = vg_conv
          ).
*------------------------------------------------------------------------
*        DATA:el_pedido TYPE ztwm_remessa_li2.
**        SELECT SINGLE * FROM ztwm_remessa_lid INTO el_pedido WHERE vbeln = lv_vbeln.
**        IF sy-subrc = 0.
*          el_pedido-lidos_remessa = lv_lfimg + el_pedido-lidos_remessa.
*          MODIFY ztwm_remessa_li2 FROM el_pedido.
*          COMMIT WORK.
*        ELSE.
*          el_pedido-lidos_remessa = lv_lfimg.
*          el_pedido-rbnum = gs_9000-ref.
*          el_pedido-vbeln = lv_vbeln.
*          MODIFY ztwm_remessa_li2 FROM el_pedido.
*          COMMIT WORK.
*        ENDIF.
*        gs_9100_tot-qtd = gs_9100_tot-qtd - el_pedido-lidos_remessa.
*------------------------------------------------------------------------

        APPEND VALUE #( mandt         =  sy-mandt       "MANDT
        tanum         =  lv_tanum       "OT
        tp_dep_orig   =  lv_vltyp       "TIPO DEP. ORIG.
        posicao_orig  =  lv_vlpla       "POSIÇÃO ORIG.
        ud_orig       =  gs_lqua-lenum  "UD ORIG.
        tipo_dep_dest =  'EBQ'          "TIPO DEP. DEST.
        posicao_dest  =  gs_9000-ref    "POSIÇÃO DEST.
*                        UD_DEST =                    "UD DEST.
        matnr         = gs_lqua-matnr        "MATERIAL
        menge         = lv_lfimg )      "QUANTIDADE
        TO gt_ztwm_coletor_he.

        LOOP AT tg_pistolagem INTO DATA(ls_ztpp_imei_apont).

          APPEND  VALUE #( mandt         =  sy-mandt                     "MANDT
          tanum         =  lv_tanum                     "OT
          imei          =  ls_ztpp_imei_apont-imei  "IMEI
          matnr         =  gs_lqua-matnr                  "MATERIAL
          pallet_sisap    = lv_number_range2 )
*                       STATUS_EITQ   =
          TO gt_ztwm_coletor_it.

        ENDLOOP.

        MODIFY ztwm_coletor FROM TABLE gt_ztwm_coletor.
        MODIFY ztwm_coletor_he FROM TABLE gt_ztwm_coletor_he.
        MODIFY ztwm_coletor_it FROM TABLE gt_ztwm_coletor_it.
        MODIFY ztwm_sepa_imei_p FROM TABLE tg_ztwm_sepa_imei.
        COMMIT WORK AND WAIT.
        CLEAR:gt_ztwm_coletor[],gt_ztwm_coletor_he[],gt_ztwm_coletor_it[],tg_ztwm_sepa_imei[].
*---------------------------------------------------------------------------------------------
*Atualiza salvos na base.
        DATA:el_pedido TYPE ztwm_remessa_li2.
        DATA(vl_salvos) = io_util->get_salvos_na_base( i_ref = gs_9000-ref ).
        el_pedido-lidos_remessa = lv_lfimg + vl_salvos.
        MODIFY ztwm_remessa_li2 FROM el_pedido.
        COMMIT WORK.
*---------------------------------------------------------------------------------------------
*Limpa Lidos Palete pois no processo eles só criam OT ao fechar o Palete.
        io_util->limpa_lidos_palete(
        EXPORTING
          i_vbeln  = gs_9000-ref
          i_palete =  vg_pallet   " 1. tipo de unidade de depósito
        CHANGING
          gs_9100_tot = gs_9100_tot
          gs_9100  = gs_9100
          ).
*---------------------------------------------------------------------------------------------

        IF gt_item_tmp IS NOT INITIAL.

          SELECT *
          FROM ztwm_item_tmp
          INTO TABLE @DATA(lt_item_aux)
                FOR ALL ENTRIES IN @gt_item_tmp
                WHERE ebeln = @gt_item_tmp-ebeln
                AND ebelp = @gt_item_tmp-ebelp.

          IF sy-subrc IS INITIAL.
            LOOP AT lt_item_aux INTO DATA(ls_item_aux).

              READ TABLE gt_item_tmp INTO gs_item_tmp WITH KEY ebeln = ls_item_aux-ebeln
              ebelp = ls_item_aux-ebelp.
              IF sy-subrc IS INITIAL.
                lv_tabix = sy-tabix.
                gs_item_tmp-menge = ( gs_item_tmp-menge + ls_item_aux-menge ).
                MODIFY gt_item_tmp FROM gs_item_tmp INDEX lv_tabix.
              ENDIF.
            ENDLOOP.
          ENDIF.

          MODIFY ztwm_item_tmp FROM TABLE gt_item_tmp.
        ENDIF.

        IF gt_conf_tmp IS NOT INITIAL.
          MODIFY ztwm_conf_tmp2 FROM TABLE gt_conf_tmp.
*** Ini CMV - 15/05/2018.
          CLEAR: gs_conf_hist, gt_conf_hist.

*          gt_conf_hist[] = gt_conf_tmp[].
          MOVE-CORRESPONDING  gt_conf_tmp[] TO gt_conf_hist[].

          LOOP AT gt_conf_hist  INTO gs_conf_hist.
            gs_conf_hist-data  = sy-datum.
            gs_conf_hist-hora  = sy-uzeit.
            gs_conf_hist-uname = sy-uname.
            gs_conf_hist-werks = gs_lqua-werks.

            MODIFY gt_conf_hist FROM gs_conf_hist.
          ENDLOOP.

          MODIFY ztwm_conf_hist FROM TABLE gt_conf_hist.
*** Fim CMV - 15/05/2018.
        ENDIF.

        PERFORM f_select_lqua.

*VERFICAR SE EXISTE MATERIAL EM LIVRE UTILIZAÇÃO E QUALIDADE
        READ TABLE gt_lqua INTO gs_lqua WITH KEY bestq = space.
        IF sy-subrc IS NOT INITIAL.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY bestq = 'Q'.
          IF sy-subrc IS INITIAL.
            "EXISTE MATERIAL BLOQUEADO. A CONFERÊNCIA NÃO É PERMITIDA.
            PERFORM f_exibir_msg USING 'ZWM_BR' '022' '' '' '' '' '0999' .
          ENDIF.

        ENDIF.

*        IF gt_lqua IS INITIAL.
        gv_msgv1 = gs_9000-ref.
        "CONFERÊNCIA DO PEDIDO/GRUPO & FINALIZADA
        gv_msgv2 = lv_number_range2.
        PERFORM f_exibir_msg USING 'ZWM_BR' '062' gv_msgv1 gv_msgv2 '' '' '0999' .
*          LEAVE TO SCREEN 0.

        CLEAR: gs_9100,
        gs_9100_tot-qtd_lidos,
        gs_9100_tot-qtd,
        gt_mat_serial_lote,
        gt_qtd_lidos,
        vg_peso,
        vg_pallet,
        tg_pistolagem,
*        vg_processo,
        gt_qtd_lidos_2.

        IF lidos_remessa = io_util->get_qtd_pedido( gs_9000-ref ).
          SET SCREEN 9000.
          LEAVE SCREEN.
        ELSE.
          SET SCREEN 9300.
          LEAVE SCREEN.
        ENDIF.
        PERFORM f_enter_9000.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_NEXT_9200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_next_9200.

  IF gv_qtd_remessas > 7.
    gv_line  = gv_line + gv_lines.
    gv_limit = gv_qtd_remessas - gv_lines.
    IF gv_line > gv_limit.
      gv_line = gv_limit.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ENTER_9200
*&---------------------------------------------------------------------*
FORM f_enter_9200.

  DATA: lr_lgtyp TYPE RANGE OF lqua-lgtyp.

  DATA: lv_ok    TYPE c.
  DATA: lv_quant TYPE lfimg.

  lr_lgtyp = VALUE #( sign = 'I' option = 'EQ' ( low = 'CF1' ) ).

  CLEAR: gt_lqua, gt_lips.

  SELECT *
  FROM lqua
  INTO TABLE gt_lqua
  WHERE lgnum = gs_lrf_wkqu-lgnum
  AND lgpla = gs_9000-ref
  AND lgtyp IN lr_lgtyp "gv_lgtyp "= Substituição por TVARV
  AND bestq = space
  AND verme > 0.

  IF sy-subrc IS INITIAL.

*    READ TABLE gt_lqua
*    INTO DATA(el_lqua)
*    INDEX gs_9200-id.

*    IF sy-subrc = 0.
*      gs_9100-matnr = el_lqua-matnr.
*      SELECT SINGLE maktx FROM makt INTO gs_9100-descricao WHERE matnr = gs_9100-matnr.
*    ENDIF.

*    DATA(io_util) = NEW zclwm_tratamento( ).

    CLEAR: gt_lips[], gt_lips.

    SELECT vbeln
    posnr
    matnr
    lgort
    lfimg
    FROM lips
    INTO TABLE gt_lips
    FOR ALL ENTRIES IN gt_lqua
    WHERE matnr = gt_lqua-matnr.

*    io_util->get_qtd_faltante(
*    EXPORTING
*      tg_lips   = gt_lips
*      tg_lqua   = gt_lqua
*      gs_9000   = gs_9000
*      gs_9200   = gs_9200
*      i_matnr   = gs_9100-matnr
*    CHANGING
*      qtd_lidos = lv_quant
*      gs_9100   = gs_9100
*      ).

    CLEAR: lv_ok.

    READ TABLE gt_remessa INTO DATA(ls_remessa) WITH KEY id = gs_9200-id.

    IF sy-subrc IS INITIAL.

      DELETE gt_lips WHERE vbeln <> ls_remessa-rbnum.

      LOOP AT gt_lips INTO DATA(ls_lips) WHERE vbeln = ls_remessa-rbnum.

        READ TABLE gt_lqua INTO DATA(ls_lqua) WITH KEY matnr = ls_lips-matnr.

        IF sy-subrc IS INITIAL.
          gs_9100-matnr = ls_lips-matnr. "Código do material
          lv_ok = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF lv_ok = abap_true.

        SELECT SINGLE maktx
        FROM makt
        INTO gs_9100-descricao
        WHERE matnr = gs_9100-matnr.

        DATA(io_util) = NEW zclwm_tratamento( ).

        READ TABLE gt_lqua INTO DATA(el_lqua) WITH KEY matnr = gs_9100-matnr.

        IF sy-subrc = 0.

          io_util->get_qtd_faltante(
          EXPORTING
            tg_lips   = gt_lips
            tg_lqua   = gt_lqua
            gs_9000   = gs_9000
            gs_9200   = gs_9200
            i_matnr   = gs_9100-matnr
          CHANGING
            qtd_lidos = lv_quant
            gs_9100   = gs_9100
            ).

        ELSE.
          lv_ok = abap_true.
        ENDIF.
      ENDIF.

      IF lv_ok IS INITIAL.
        "NÃO EXISTE MATERIAL PARA A REMESSA
        PERFORM f_exibir_msg USING 'ZWM_BR' '032' '' '' '' '' '0999'.
      ELSE.
        gs_9200-rbnum = ls_remessa-rbnum.
        CLEAR: vg_pallet.
        CALL SCREEN 9300.
      ENDIF.

    ELSE.
      "ID INFORMADO NÃO EXISTENTE
      PERFORM f_exibir_msg USING 'ZWM_BR' '027' '' '' '' '' '0999'.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BACK_9200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_back_9200 .

  IF gv_qtd_remessas > 7.
    gv_line = gv_line - gv_lines.
    IF gv_line < 0.
      gv_line = 0.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_VERIFICA_VOLUME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grp_verifica_volume.

  DATA:
    ls_calc_volume  TYPE ztwm_calc_volume,
    lv_volume_total TYPE p LENGTH 13,
    lv_matnr        TYPE mara-matnr,
    tl_total        TYPE TABLE OF l2sktotal,
    tl_delitem      TYPE TABLE OF l2skdlitem.

  CLEAR: gt_lips.

  CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
    EXPORTING
      i_lgnum                       = '603'
      i_refnr                       = gs_9000-ref
    TABLES
*     T_TRITEM                      =
      t_delitem                     = tl_delitem
*     T_ERRORITEM                   =
      t_total                       = tl_total
    EXCEPTIONS
      refnr_no_found                = 1
      refnr_documents_no_found      = 2
      no_relevant_for_2step_picking = 3
      item_for_removal_not_found    = 4
      OTHERS                        = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE tl_delitem INTO DATA(el_delit) WITH KEY vbeln = gs_9200-rbnum.
  IF sy-subrc = 0.
    lv_matnr = el_delit-matnr.
  ENDIF.


  SELECT vbeln posnr matnr lgort lfimg
  FROM lips
  INTO TABLE gt_lips
  FOR ALL ENTRIES IN gt_lqua
  WHERE vbeln = gs_9200-rbnum
  AND matnr = gt_lqua-matnr.


  IF ( sy-subrc = 0 ) AND ( gs_9100-matnr_v IS NOT INITIAL ).
*    PERFORM zf_add_zeros_esquerda USING gs_9100-matnr_v(6) CHANGING lv_matnr.
    READ TABLE gt_lips TRANSPORTING NO FIELDS WITH KEY matnr = lv_matnr.
    IF ( sy-subrc <> 0 ).
      PERFORM f_exibir_msg USING 'ZWM_BR' '000' TEXT-003 '' '' '' '0999' .
      CLEAR: gt_lips[], gt_lips, gs_9100-matnr_v.
      RETURN.
    ENDIF.
  ENDIF.


  CLEAR: lt_calc_volume.

  SELECT *
  FROM ztwm_calc_volume
  INTO TABLE lt_calc_volume
  WHERE vbeln = gs_9200-rbnum.

  IF lt_calc_volume IS NOT INITIAL.

    CLEAR: lv_volume_total.
    LOOP AT lt_calc_volume INTO ls_calc_volume.
      lv_volume_total = lv_volume_total + ls_calc_volume-qtd_volume.
    ENDLOOP.

    SELECT MAX( anzpk )
    FROM ztwm_conf_tmp
    INTO gv_volume
    WHERE ebeln = gs_9000-ref
    AND vbeln = gs_9200-rbnum.

    gs_9100_tot-vol_de  = gv_volume + 1.
    gs_9100_tot-vol_ate = lv_volume_total.

  ELSE.

    DATA: lv_saldo     TYPE menge_d,
          lv_caixa     TYPE p LENGTH 13,
          lv_c_valor   TYPE c LENGTH 15,
          lv_c_inteiro TYPE c LENGTH 15,
          lv_c_decimal TYPE c LENGTH 15,
          lv_restante  TYPE p LENGTH 13,
          lv_volume    TYPE menge_d.

    LOOP AT gt_lips INTO DATA(ls_lips).

      CLEAR: lv_volume, ls_calc_volume.

      lv_saldo = ls_lips-lfimg.

      READ TABLE gt_lqua
      INTO gs_lqua
      WITH KEY matnr = ls_lips-matnr.

      IF sy-subrc IS INITIAL.

        IF gs_lqua-verme < lv_saldo.
          lv_saldo = gs_lqua-verme.
        ENDIF.

        ls_calc_volume-vbeln = ls_lips-vbeln.
        ls_calc_volume-matnr = ls_lips-matnr.

*       Verificar Volume
        SELECT SINGLE lvsme
        FROM mlgn
        INTO @DATA(lv_lvsme)
              WHERE matnr = @ls_lips-matnr
              AND lgnum = @gs_lrf_wkqu-lgnum.

        IF sy-subrc IS INITIAL.

          IF lv_lvsme IS NOT INITIAL.

*           Calcular Volume CAIXA MASTER
            SELECT SINGLE umrez
            FROM marm
            INTO @DATA(lv_marm_umrez)
                  WHERE matnr = @ls_lips-matnr
                  AND meinh = @lv_lvsme.

*** <<< Inicio -  G.Oliveira - MGS - ISSUE00016 - 06.11.2017 07:30:12
            CLEAR: lv_c_valor,
            lv_c_inteiro,
            lv_c_decimal.

            lv_c_valor  = ( lv_saldo / lv_marm_umrez ).

            CONDENSE lv_c_valor NO-GAPS.

            SPLIT lv_c_valor AT '.' INTO lv_c_inteiro lv_c_decimal.

            lv_caixa  = lv_c_inteiro.
*            lv_caixa  = ( lv_saldo / ls_marm-umrez ).
*** >>> Fim -  G.Oliveira - MGS - ISSUE00016 - 06.11.2017 07:30:30

            lv_volume = ( lv_caixa * lv_marm_umrez ).

            ls_calc_volume-qtd_unidade = lv_marm_umrez.
            ls_calc_volume-qtd_volume  = lv_caixa.
            ls_calc_volume-ztp_vol     = 'M'.
* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:31:32 - Início
*            APPEND ls_calc_volume TO lt_calc_volume.
            DATA: ls_calc_vol1  TYPE ztwm_calc_volume.
            READ TABLE lt_calc_volume INTO ls_calc_vol1 WITH KEY vbeln = ls_calc_volume-vbeln
            matnr = ls_calc_volume-matnr
            ztp_vol = ls_calc_volume-ztp_vol.
            IF sy-subrc IS INITIAL.   "achou o registro na tabela  (sumarizar total de volumes)
              ls_calc_volume-qtd_volume = ls_calc_volume-qtd_volume + ls_calc_vol1-qtd_volume.
              MODIFY lt_calc_volume FROM ls_calc_volume INDEX sy-tabix.
            ELSE.
              APPEND ls_calc_volume TO lt_calc_volume.
            ENDIF.
* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:33:57 - Início
*** <<< Inicio -  G.Oliveira - MGS - ISSUE00016 - 06.11.2017 07:31:24
*            DO.
*              CLEAR: lv_restante.
*              IF lv_volume < lv_saldo.
*
*                lv_restante = lv_saldo - lv_volume.
*                ADD lv_restante TO lv_volume.
*
*                ls_calc_volume-qtd_unidade = 1.
*                ls_calc_volume-qtd_volume  = 1.
*                ls_calc_volume-ztp_vol    = 'I'.
*                APPEND ls_calc_volume TO lt_calc_volume.
*
*              ELSEIF lv_volume = lv_saldo.
*                EXIT.
*              ENDIF.
*            ENDDO.

            CLEAR: lv_restante.
            lv_restante = lv_saldo - lv_volume.

            IF lv_restante > 0.
              ls_calc_volume-qtd_unidade = 1.
              ls_calc_volume-qtd_volume  = lv_restante.
              ls_calc_volume-ztp_vol    = 'I'.

* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:31:32 - Início
*              APPEND ls_calc_volume TO lt_calc_volume.
              READ TABLE lt_calc_volume INTO ls_calc_vol1 WITH KEY vbeln = ls_calc_volume-vbeln
              matnr = ls_calc_volume-matnr
              ztp_vol = ls_calc_volume-ztp_vol     .
              IF sy-subrc IS INITIAL. "achou o registro na tabela  (sumarizar total de volumes)
                ls_calc_volume-qtd_volume = ls_calc_volume-qtd_volume + ls_calc_vol1-qtd_volume.
                MODIFY lt_calc_volume FROM ls_calc_volume INDEX sy-tabix.
              ELSE.
                APPEND ls_calc_volume TO lt_calc_volume.
              ENDIF.
* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:36:50 - Fim

            ENDIF.
*** >>> Fim -  G.Oliveira - MGS - ISSUE00016 - 06.11.2017 07:31:35

          ELSE.

            ls_calc_volume-qtd_unidade = 1.
            ls_calc_volume-qtd_volume  = lv_saldo.
            ls_calc_volume-ztp_vol     = 'I'.
* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:38:44 - Início
*           APPEND ls_calc_volume TO lt_calc_volume.
            CLEAR ls_calc_vol1 .
            READ TABLE lt_calc_volume INTO ls_calc_vol1 WITH KEY vbeln = ls_calc_volume-vbeln
            matnr = ls_calc_volume-matnr
            ztp_vol = ls_calc_volume-ztp_vol     .
            IF sy-subrc IS INITIAL. "achou o registro na tabela  (sumarizar total de volumes)
              ls_calc_volume-qtd_volume = ls_calc_volume-qtd_volume + ls_calc_vol1-qtd_volume.
              MODIFY lt_calc_volume FROM ls_calc_volume INDEX sy-tabix.
            ELSE.
              APPEND ls_calc_volume TO lt_calc_volume.
            ENDIF.
* SAP - MOD - CH I1801-0155 - Reinaldo Carvalho - 23.07.2018 19:38:56 - Fim
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

* --- inclusão do transportar na etiqueta
    IF gs_9000-transp IS NOT INITIAL.
      LOOP AT lt_calc_volume ASSIGNING FIELD-SYMBOL(<fs_calc_volume>).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_9000-transp
          IMPORTING
            output = <fs_calc_volume>-lifnr.
      ENDLOOP.
    ENDIF.

    MODIFY ztwm_calc_volume FROM TABLE lt_calc_volume.

* chamado I1804-0109
*    WAIT UP TO 3 SECONDS.
* chamado I1804-0109

    gs_9100_tot-vol_de = 1.

    LOOP AT lt_calc_volume INTO ls_calc_volume.
      gs_9100_tot-vol_ate = gs_9100_tot-vol_ate + ls_calc_volume-qtd_volume.
    ENDLOOP.

    SELECT SINGLE padest
    INTO @DATA(lv_padest)
          FROM ztwm_ctrl_impres
          WHERE lgnum = @gs_lrf_wkqu-lgnum
          AND tcode = @sy-tcode
          AND chave = ''.

*--- IMPRIMIR ETIQUETAS
    CALL FUNCTION 'ZFMWM_ETIQUETA_VOLUME'
      EXPORTING
        i_vbeln           = gs_9200-rbnum
*       I_VOLUME          =
        i_imp             = lv_padest
        i_copia           = 1
      EXCEPTIONS
        remessa_not_found = 1
        volume_not_found  = 2
        error_set_printer = 3
        OTHERS            = 4.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GRP_FECHAR_VOLUME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grp_fechar_volume .

  DATA: lt_item            TYPE TABLE OF ztwm_saldo_conf,
        lv_lgnum           TYPE ltak-lgnum,
        lv_bwlvs           TYPE ltak-bwlvs,
        lv_benum           TYPE ltak-benum,
        lv_lznum           TYPE ltak-lznum,
        lv_lfimg           TYPE lfimg,
        lv_flag            TYPE c,
        gt_ztwm_coletor    TYPE TABLE OF ztwm_coletor,
        gt_ztwm_coletor_it TYPE TABLE OF ztwm_coletor_it,
        gt_ztwm_coletor_he TYPE TABLE OF ztwm_coletor_he.

  READ TABLE gt_qtd_lidos INTO DATA(ls_qtd_lidos) INDEX 1.

  IF gt_qtd_lidos IS INITIAL.
    "NÃO FOI CONFERIDO NENNHUM ITEM
    PERFORM f_exibir_msg USING 'ZWM_BR' '024' '' '' '' '' '0999' .
  ELSE.

    CLEAR: gt_conf_tmp,
    gt_ltap_creat.

    SELECT *
    FROM ztwm_saldo_conf
    INTO TABLE lt_item
    WHERE ebeln = gs_9000-ref
    AND menge > 0.

    READ TABLE lt_item INTO DATA(el_item) INDEX 1.
    IF sy-tabix = 0.
      CLEAR lt_item[].
      APPEND el_item TO lt_item.
    ENDIF.

    IF lt_item IS NOT INITIAL.

      SORT lt_item BY ebeln matnr ebelp ASCENDING.

*LIVRE UTILIZAÇÃO
      LOOP AT gt_qtd_lidos INTO gs_qtd_lidos.

        DATA(lv_tabix) = sy-tabix.

        IF lv_tabix = 1.
          CLEAR: lv_flag.
          LOOP AT lt_calc_volume INTO DATA(ls_calc_volume) WHERE matnr   = ls_qtd_lidos-matnr
                AND ztp_vol = 'M'.
            DATA(lv_tabix1) = sy-tabix.
            IF ls_calc_volume-qtd_volume > ls_calc_volume-vol_fechado.
              ls_calc_volume-vol_fechado = ls_calc_volume-vol_fechado + 1.

              MODIFY lt_calc_volume FROM ls_calc_volume INDEX lv_tabix1.
              lv_flag = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF lv_flag IS INITIAL.
            LOOP AT lt_calc_volume INTO ls_calc_volume WHERE matnr   = ls_qtd_lidos-matnr
            AND ztp_vol = 'I'.
              lv_tabix1 = sy-tabix.
              IF ls_calc_volume-qtd_volume > ls_calc_volume-vol_fechado.
                ls_calc_volume-vol_fechado = ls_calc_volume-vol_fechado + 1.

                MODIFY lt_calc_volume FROM ls_calc_volume INDEX lv_tabix1.

                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          MODIFY ztwm_calc_volume FROM TABLE lt_calc_volume.

        ENDIF.

        LOOP AT lt_item INTO DATA(ls_item) WHERE matnr = gs_qtd_lidos-matnr AND bestq = space.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY matnr = gs_qtd_lidos-matnr.
          IF sy-subrc IS INITIAL.

            READ TABLE gt_lips INTO DATA(ls_lips) WITH KEY matnr = gs_lqua-matnr.
            IF sy-subrc IS INITIAL.

*              TRY.
*                  DATA(ls_mat_serial_lote) = gt_mat_serial_lote[ matnr = gs_qtd_lidos-matnr serial = gs_qtd_lidos-serial ].
*                CATCH cx_sy_itab_line_not_found INTO DATA(lv_erro).
*                  CLEAR: ls_mat_serial_lote.
*              ENDTRY.
              IF gt_mat_serial_lote IS NOT INITIAL.
*                DATA(ls_mat_serial_lote) = gt_mat_serial_lote[ matnr = gs_qtd_lidos-matnr serial = gs_qtd_lidos-serial ].
                DATA ls_mat_serial_lote LIKE LINE OF gt_mat_serial_lote.
              ENDIF.
              IF gs_qtd_lidos-qtd_lidos >= ls_item-menge.

                PERFORM f_add_conf_tmp USING ls_item-ebeln
                      ls_lips-vbeln
                      ls_lips-posnr
                      gs_qtd_lidos-matnr
                      ls_mat_serial_lote-lote
                      ls_mat_serial_lote-serial
                      gs_9100_tot-vol_de
                      ls_item-menge
                      gs_lqua-meins.

                gs_qtd_lidos-qtd_lidos = gs_qtd_lidos-qtd_lidos - ls_item-menge.

              ELSE.

                PERFORM f_add_conf_tmp USING ls_item-ebeln
                      ls_lips-vbeln
                      ls_lips-posnr
                      gs_qtd_lidos-matnr
                      ls_mat_serial_lote-lote
                      ls_mat_serial_lote-serial
                      gs_9100_tot-vol_de
                      gs_qtd_lidos-qtd_lidos
                      gs_lqua-meins.

                CLEAR: gs_qtd_lidos-qtd_lidos.

              ENDIF.
            ENDIF.
          ENDIF.

          MODIFY gt_qtd_lidos FROM gs_qtd_lidos INDEX lv_tabix.

          IF gs_qtd_lidos-qtd_lidos IS INITIAL.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM gt_qtd_lidos COMPARING matnr.

      LOOP AT gt_qtd_lidos INTO gs_qtd_lidos.

        CLEAR: lv_lfimg.
        LOOP AT gt_conf_tmp INTO DATA(ls_conf_tmp) WHERE matnr = gs_qtd_lidos-matnr.
          ADD ls_conf_tmp-lfimg TO lv_lfimg.
        ENDLOOP.

        SORT gt_conf_tmp ASCENDING.
        DELETE ADJACENT DUPLICATES FROM gt_conf_tmp COMPARING ALL FIELDS.

        READ TABLE gt_conf_tmp INTO gs_conf_tmp WITH KEY matnr = gs_qtd_lidos-matnr.
        IF sy-subrc IS INITIAL.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY matnr = gs_conf_tmp-matnr.
          IF sy-subrc IS INITIAL.

            lv_lgnum = gs_lrf_wkqu-lgnum.
            lv_benum = gs_9200-rbnum.
            lv_lznum = gv_volume.
            lv_bwlvs = '951'.

            APPEND VALUE #(  matnr  = gs_lqua-matnr
            werks  = gs_lqua-werks
            lgort  = gs_lqua-lgort
            charg  = gs_lqua-charg
            anfme  = lv_lfimg
            altme  = gs_lqua-meins
            squit  = abap_true
*                             vltyp  = 'CF1'
            vltyp  = gv_lgtyp "Substituição por TVARV
            vlpla  = gs_lqua-lgpla ) TO gt_ltap_creat.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum                = lv_lgnum
          i_bwlvs                = lv_bwlvs
          i_benum                = lv_benum
          i_lznum                = lv_lznum
          i_commit_work          = 'X'
          i_bname                = sy-uname
        TABLES
          t_ltap_creat           = gt_ltap_creat
        EXCEPTIONS
          no_to_created          = 1
          bwlvs_wrong            = 2
          betyp_wrong            = 3
          benum_missing          = 4
          betyp_missing          = 5
          foreign_lock           = 6
          vltyp_wrong            = 7
          vlpla_wrong            = 8
          vltyp_missing          = 9
          nltyp_wrong            = 10
          nlpla_wrong            = 11
          nltyp_missing          = 12
          rltyp_wrong            = 13
          rlpla_wrong            = 14
          rltyp_missing          = 15
          squit_forbidden        = 16
          manual_to_forbidden    = 17
          letyp_wrong            = 18
          vlpla_missing          = 19
          nlpla_missing          = 20
          sobkz_wrong            = 21
          sobkz_missing          = 22
          sonum_missing          = 23
          bestq_wrong            = 24
          lgber_wrong            = 25
          xfeld_wrong            = 26
          date_wrong             = 27
          drukz_wrong            = 28
          ldest_wrong            = 29
          update_without_commit  = 30
          no_authority           = 31
          material_not_found     = 32
          lenum_wrong            = 33
          matnr_missing          = 34
          werks_missing          = 35
          anfme_missing          = 36
          altme_missing          = 37
          lgort_wrong_or_missing = 38
          error_message          = 39
          OTHERS                 = 40.

      IF sy-subrc <> 0.

        gv_msgid = sy-msgid.
        gv_msgno = sy-msgno.
        PERFORM f_exibir_msg USING gv_msgid gv_msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 '0999'.

      ELSEIF sy-subrc IS INITIAL.
        DATA(vl_ot) = sy-msgv1.

        SELECT *
        FROM ztwm_sepa_imei_p AS a
        INTO TABLE tg_ztwm_sepa_imei
        WHERE ebeln = gs_9000-ref
        AND conf  = abap_true
        AND tanum = space.

        LOOP AT tg_ztwm_sepa_imei INTO DATA(el_imei).
          el_imei-tanum = vl_ot.
          MODIFY tg_ztwm_sepa_imei FROM el_imei INDEX sy-tabix.
        ENDLOOP.
        MODIFY ztwm_sepa_imei_p FROM TABLE tg_ztwm_sepa_imei.
        COMMIT WORK.
        DATA: lv_number_range(7)   TYPE n,    "-- Variable to hold Newly generated Number Range
              lv_year              TYPE inri-toyear,              "-- Variable to hold Year
              lv_subobject         TYPE  zeseqnum,     "-- Variable to hold Company Code
              lv_rc                TYPE inri-returncode,            "-- Variable to hold the Return Code
              lv_number_range2(10) TYPE c.

        lv_year = sy-datum(4).         "--- Assign Current Year
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'                "-- This hold the Newly generated Number
            object                  = 'ZWM0002'       "---- Passing the Number Range Object
            subobject               = lv_subobject    "-- Pass the Company code
            toyear                  = lv_year            "-- Pass the Year
          IMPORTING
            number                  = lv_number_range "-- Newly generated Number
            returncode              = lv_rc                   "-- The Return Code Number
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        lv_number_range2 = |PLT{ lv_number_range }|.

*------------------------------------------------------------------------------------------------------
        DATA(lv_tanum) = vl_ot.
        DATA(lv_werks) = gs_lqua-werks.
        DATA(lv_lgort) = gs_lqua-lgort.
        DATA(lv_vltyp) = gs_lqua-lgtyp.
        DATA(lv_vlpla) = gs_lqua-lgpla.
        DATA(lv_anfme) = ls_item-menge.

        READ TABLE gt_remessa INTO DATA(ls_remessa) WITH KEY id = gs_9200-id.

        SELECT *
        FROM ztwm_sepa_imei_p
        INTO TABLE @DATA(tg_ztwm_sepa_imei)
              WHERE ebeln = @gs_9000-ref
              AND conf  = @abap_true.
        IF sy-subrc EQ 0.

          LOOP AT tg_ztwm_sepa_imei ASSIGNING FIELD-SYMBOL(<fs_ztwm_sepa_imei>).
            <fs_ztwm_sepa_imei>-ot_conf = abap_true.
          ENDLOOP.

        ENDIF.

        APPEND VALUE #( mandt = sy-mandt
        betyp   = gs_lqua-betyp
        benum   = gs_9000-ref
        vbeln   = gs_9200-rbnum
        tanum   = lv_tanum
        lgnum   = lv_lgnum
        werks   = lv_werks
        lgort   = lv_lgort
        usuario = sy-uname
        data    = sy-datum
        peso    = vg_peso
        hora    = sy-uzeit ) TO gt_ztwm_coletor.

        APPEND VALUE #( mandt         =  sy-mandt       "MANDT
        tanum         =  lv_tanum       "OT
        tp_dep_orig   =  lv_vltyp       "TIPO DEP. ORIG.
        posicao_orig  =  lv_vlpla       "POSIÇÃO ORIG.
        ud_orig       =  gs_lqua-lenum  "UD ORIG.
        tipo_dep_dest =  'EBQ'          "TIPO DEP. DEST.
        posicao_dest  =  gs_9000-ref    "POSIÇÃO DEST.
*                        UD_DEST =                    "UD DEST.
        matnr         = gs_lqua-matnr        "MATERIAL
        menge         = lv_lfimg )      "QUANTIDADE
        TO gt_ztwm_coletor_he.

        LOOP AT tg_pistolagem INTO DATA(ls_ztpp_imei_apont).

          APPEND  VALUE #( mandt         =  sy-mandt                     "MANDT
          tanum         =  lv_tanum                     "OT
          imei          =  ls_ztpp_imei_apont-imei  "IMEI
          matnr         =  gs_lqua-matnr                  "MATERIAL
          pallet_sisap    = lv_number_range2 )
*                       STATUS_EITQ   =
          TO gt_ztwm_coletor_it.

        ENDLOOP.

*------------------------------------------------------------------------
*        Atualiza Peso
        DATA(io_util) = NEW zclwm_tratamento( ).
        DATA:vg_conv TYPE char10.
        vg_conv = vg_peso.
        io_util->atualiza_peso(
        EXPORTING
          i_remessa = ls_remessa-rbnum
          peso_tela = vg_conv
          ).
*---------------------------------------------------------------------------------------------
        MODIFY ztwm_coletor FROM TABLE gt_ztwm_coletor.
        MODIFY ztwm_coletor_he FROM TABLE gt_ztwm_coletor_he.
        MODIFY ztwm_coletor_it FROM TABLE gt_ztwm_coletor_it.
        MODIFY ztwm_sepa_imei_p FROM TABLE tg_ztwm_sepa_imei.
        COMMIT WORK AND WAIT.
*---------------------------------------------------------------------------------------------
        SELECT *
               FROM ztwm_conf_venda
               INTO TABLE @DATA(tl_conf_venda)
               FOR ALL ENTRIES IN @tg_pistolagem
               WHERE ebeln = @gs_9000-ref
                 AND imei  = @tg_pistolagem-imei.
        IF sy-subrc = 0.
          LOOP AT tl_conf_venda INTO DATA(el_conf_venda).
            el_conf_venda-definitivo = abap_true.
            MODIFY tl_conf_venda FROM el_conf_venda INDEX sy-tabix.
          ENDLOOP.
          MODIFY ztwm_conf_venda FROM TABLE tl_conf_venda.
          COMMIT WORK.
        ENDIF.
        CLEAR tg_pistolagem[].
*---------------------------------------------------------------------------------------------
        IF gt_conf_tmp IS NOT INITIAL.
          MODIFY ztwm_conf_tmp FROM TABLE gt_conf_tmp.
          CLEAR: gs_conf_hist, gt_conf_hist.

          MOVE-CORRESPONDING  gt_conf_tmp[] TO gt_conf_hist[].

          LOOP AT gt_conf_hist  INTO gs_conf_hist.
            gs_conf_hist-data  = sy-datum.
            gs_conf_hist-hora  = sy-uzeit.
            gs_conf_hist-uname = sy-uname.
            gs_conf_hist-werks = gs_lqua-werks.

            MODIFY gt_conf_hist FROM gs_conf_hist.
          ENDLOOP.

          MODIFY ztwm_conf_hist FROM TABLE gt_conf_hist.
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        PERFORM f_select_lqua.

*VERFICAR SE EXISTE MATERIAL EM LIVRE UTILIZAÇÃO E QUALIDADE
        READ TABLE gt_lqua INTO gs_lqua WITH KEY bestq = space.
        IF sy-subrc IS NOT INITIAL.

          READ TABLE gt_lqua INTO gs_lqua WITH KEY bestq = 'Q'.
          IF sy-subrc IS INITIAL.
            "EXISTE MATERIAL BLOQUEADO. A CONFERÊNCIA NÃO É PERMITIDA.
            PERFORM f_exibir_msg USING 'ZWM_BR' '022' '' '' '' '' '0999' .
          ENDIF.

        ENDIF.

        IF gt_lqua IS INITIAL.
          gv_msgv1 = gs_9000-ref.
          "CONFERÊNCIA DO PEDIDO/GRUPO & FINALIZADA
          gv_msgv2 = lv_number_range2.
          PERFORM f_exibir_msg USING 'ZWM_BR' '062' gv_msgv1 gv_msgv2 '' '' '0999' .
          SET SCREEN 9000.
          LEAVE SCREEN.

        ENDIF.

        DATA(v_temp_matnr) = gs_9100-matnr.
        DATA(v_temp_qtdv) = gs_9100-qtd_v.
        DATA(v_qtd) = gs_9100_tot-qtd.

        CLEAR: gs_9100,
        vg_peso,
        gs_9100_tot-qtd_lidos,
        gs_9100_tot-qtd,
        gt_mat_serial_lote,
        gt_qtd_lidos,
        gt_mat_serial_lote,
        gv_material.

*Verificar volumes
        PERFORM f_grp_verifica_volume.

*Verificar as remessas
        PERFORM f_select_remessas.

        IF gt_remessa[] IS INITIAL.

          "GRUPO FINALIZADO
          gv_msgv1 = lv_number_range2.
          PERFORM f_exibir_msg USING 'ZWM_BR' '033' gv_msgv1 '' '' '' '0999' .
          CLEAR: gs_9200-id,
          gs_9200-rbnum,
          vg_peso,
          vg_pallet,
          tg_pistolagem.

          SET SCREEN 9000.
          LEAVE SCREEN.
        ELSE.
          READ TABLE gt_remessa INTO ls_remessa WITH KEY rbnum = gs_9200-rbnum.
          IF sy-subrc IS NOT INITIAL.
            "REMESSA FINALIZADA
            gv_msgv1 = lv_number_range2.
            PERFORM f_exibir_msg USING 'ZWM_BR' '031' gv_msgv1 '' '' '' '0999' .

            CLEAR: gs_9200-id,
            gs_9200-rbnum,
            vg_peso,
            vg_pallet,
            tg_pistolagem.

            sy-ucomm = 'FECHAVOL'.

            SET SCREEN 9200.
            LEAVE SCREEN.

          ELSE.

            CLEAR: gs_9100-matnr_v.
            SET CURSOR FIELD 'GS_9100-MATNR_V'.
            gs_9100_tot-qtd = v_qtd.
            gs_9100-matnr   = v_temp_matnr.
            gs_9100-qtd_v   = v_temp_qtdv.
            "CONFERÊNCIA DO PEDIDO/GRUPO & PARCIAL
            gv_msgv1 = gs_9000-ref.
            gv_msgv2 = lv_number_range2.
            PERFORM f_exibir_msg USING 'ZWM_BR' '074' gv_msgv1 gv_msgv2 '' '' '0999' .
            vg_parcial = abap_true.
          ENDIF.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VERIFICA_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_GS_9100_MATNR_V  text
*&---------------------------------------------------------------------*
FORM f_verifica_material USING p_matnr_v.

  DATA: lv_material    TYPE mara-matnr,
        exp_material   TYPE c LENGTH 6,
        exp_lote       TYPE c LENGTH 10,
        exp_serial     TYPE c LENGTH 20,
        imp_cod_barras TYPE c LENGTH 35.

  IF p_matnr_v IS NOT INITIAL.

    imp_cod_barras = p_matnr_v.

    DATA(lv_length) = strlen( imp_cod_barras ).

    READ TABLE gt_lips INTO DATA(gs_lips) INDEX 1.

*    IF lv_length < 6 OR ( lv_length = 6 AND gs_lips-lgort = 'PA01' ).
    IF lv_length < 6 OR ( lv_length = 6 AND gs_lips-lgort = gv_lgort ). "Substituição por TVARV
      CLEAR: gs_9100.
      SET CURSOR FIELD 'GS_9100-MATNR_V'.
      gv_erro = abap_true.
      "CÓDIGO DE BARRAS INVÁLIDO
      PERFORM f_exibir_msg USING 'ZWM_BR' '019' '' '' '' '' '0999'.
    ELSE.
      CALL FUNCTION 'ZFMWM_VALIDA_MAT_ETIQUETA'
        EXPORTING
          i_cod_barras               = imp_cod_barras
        IMPORTING
          e_material                 = exp_material
          e_lote                     = exp_lote
          e_serial                   = exp_serial
        EXCEPTIONS
          ex_material_nao_encontrado = 1
          ex_cod_barras_invalido     = 2
          OTHERS                     = 3.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = exp_material
          IMPORTING
            output       = lv_material
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

        IF lv_material IS NOT INITIAL.
          IF gv_material <> lv_material.
            CLEAR: gs_9100.
            SET CURSOR FIELD 'GS_9100-MATNR_V'.
            gv_erro = abap_true.
            "MATERIAL DIFERENTE DO LIDO ANTERIOMENTE
            PERFORM f_exibir_msg USING 'ZWM_BR' '030' 'MATERIAL DIFERENTE DO LIDO ANTERIORMENTE' '' '' '' '0999'.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_SELECT_REMESSAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_select_remessas .

  DATA: lv_rem_ok TYPE c.

  CLEAR: gt_remessa.

*Grupo de remessas
  SELECT rbnum
  FROM t311a
  INTO TABLE gt_t311a
  WHERE lgnum = gs_lrf_wkqu-lgnum
  AND refnr = gs_9000-ref.

  SELECT *
  FROM ztwm_calc_volume
  INTO TABLE @DATA(lt_calc_volume)
        FOR ALL ENTRIES IN @gt_t311a
        WHERE vbeln = @gt_t311a-rbnum.

  LOOP AT gt_t311a INTO DATA(ls_t311a).
    CLEAR: lv_rem_ok.

    LOOP AT lt_calc_volume INTO DATA(ls_calc_volume) WHERE vbeln = ls_t311a-rbnum.
      IF ls_calc_volume-qtd_volume > ls_calc_volume-vol_fechado.
        lv_rem_ok = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_rem_ok IS INITIAL.
      DELETE gt_t311a WHERE rbnum = ls_calc_volume-vbeln.
    ENDIF.
    CLEAR: ls_calc_volume.
  ENDLOOP.

  LOOP AT gt_t311a INTO ls_t311a.
    APPEND VALUE #( id    = sy-tabix
    rbnum = ls_t311a-rbnum ) TO gt_remessa.
  ENDLOOP.

  DATA(io_01) = NEW zclwm_tratamento( ).
  LOOP AT gt_remessa INTO DATA(el_remessa).
    DATA(vl_tabix) = sy-tabix.
    DATA(vl_limite) = io_01->get_limite_remessa(
      i_remessa = el_remessa-rbnum
    ).

    DATA(vl_lidos) = io_01->get_lidos_remessa(
                       i_remessa = el_remessa-rbnum
                        i_pedido  = gs_9000-ref
          ).

    IF vl_lidos = vl_limite.
      DELETE gt_remessa INDEX vl_tabix.
    ENDIF.
  ENDLOOP.
  gv_qtd_remessas = lines( gt_remessa ).


ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PREENCHE_9200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_preenche_9200 .

  CLEAR: gs_9200,
  gs_9100_tot.

  IF gt_remessa IS INITIAL.
    "GRUPO FINALIZADO
    PERFORM f_exibir_msg USING 'ZWM_BR' '033' '' '' '' '' '0999'.
    SET SCREEN 9000.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_ADD_ZEROS_ESQUERDA
*&---------------------------------------------------------------------*
FORM zf_add_zeros_esquerda USING pu_matnr TYPE char6
CHANGING pc_matnr TYPE mara-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = pu_matnr
    IMPORTING
      output       = pc_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

ENDFORM.

FORM f_verifica_transp CHANGING c_erro.

  DATA: lv_lifnr TYPE lfa1-lifnr.

  CLEAR: c_erro, lv_lifnr.

* SAP - Melhoria -  Reinaldo Carvalho - 30.08.2018 12:15:05 - Início
* --- removendo a obrigatoriedade de informar o código da transportadora
  IF gs_9000-transp IS INITIAL.
    RETURN.
  ENDIF.
* SAP - Melhoria - Reinaldo Carvalho - 30.08.2018 12:15:07 - Fim

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_9000-transp
    IMPORTING
      output = lv_lifnr.

  SELECT COUNT( * ) FROM lfa1 INTO @DATA(count) WHERE lifnr = @lv_lifnr.

  IF count IS INITIAL.
    c_erro = abap_true.
    gv_msgv1 = gs_9000-transp.
    PERFORM f_exibir_msg USING 'ZWM_BR' '046' gv_msgv1 '' '' '' '0999'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECIONA_TVAVR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_seleciona_tvavr .

* Objeto da TVARV
  DATA: o_tvarv TYPE REF TO lcl_tvarv.

  CREATE OBJECT o_tvarv
    EXPORTING
      prefix    = 'ZWM'
      separator = '_'.

  o_tvarv->get_parameter(
  EXPORTING
    suffix = 'CONFERENCIA_LGTYP'
  IMPORTING
    value  = gv_lgtyp ).

  o_tvarv->get_parameter(
  EXPORTING
    suffix = 'CONFERENCIA_LGORT'
  IMPORTING
    value  = gv_lgort ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECA_QUANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM checa_quant .
  IF gt_qtd_lidos IS INITIAL.
    "Não foi Conferido nennhum Item
    PERFORM f_exibir_msg USING 'ZWM_BR' '024' '' '' '' '' '0999' .
    SET SCREEN 9100.
    LEAVE SCREEN.
  ENDIF.

  CALL SCREEN 9400.

ENDFORM.
