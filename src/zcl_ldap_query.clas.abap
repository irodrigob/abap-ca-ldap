CLASS zcl_ldap_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_dn | <p class="shorttext synchronized">DN de conexión a LDAP</p>
    METHODS constructor
      IMPORTING iv_serverid TYPE ldap_serv OPTIONAL
      RAISING   zcx_ldap.
    "! <p class="shorttext synchronized">Devuelve el usuario de la cuenta a partir del email</p>
    "! @parameter iv_dn | <p class="shorttext synchronized">DN de conexión a LDAP</p>
    "! @parameter iv_usermail | <p class="shorttext synchronized">Mail del usuario</p>
    "! @parameter rv_account_name | <p class="shorttext synchronized">Nombre de la cuenta</p>
    METHODS get_accountname_from_usermail
      IMPORTING iv_usermail            TYPE string
                iv_dn                  TYPE ldap_base OPTIONAL
      RETURNING VALUE(rv_account_name) TYPE string
      RAISING   zcx_ldap.
    "! <p class="shorttext synchronized">Consulta generica a LDAP</p>
    "! @parameter iv_dn | <p class="shorttext synchronized">DN de conexión a LDAP</p>
    "! @parameter it_attributes | <p class="shorttext synchronized">Atributos</p>
    "! @parameter iv_filter | <p class="shorttext synchronized">Filtro</p>
    "! @parameter rt_values | <p class="shorttext synchronized">Atributos y sus valores</p>
    METHODS query_generic
      IMPORTING iv_dn            TYPE ldap_base OPTIONAL
                it_attributes    TYPE ldapastab
                iv_filter        TYPE ldapdefs-filt
      RETURNING VALUE(rt_values) TYPE zldap_i_attributes_values
      RAISING   zcx_ldap.
    "! <p class="shorttext synchronized">Desconexión</p>
    METHODS disconnect.
    "! <p class="shorttext synchronized">Información del usuario</p>
    "! @parameter iv_dn | <p class="shorttext synchronized">DN de conexión a LDAP</p>
    "! @parameter iv_accountname | <p class="shorttext synchronized">Nombre de la cuenta</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Obtener grupos asignados</p>
    "! @parameter iv_sapgroup | <p class="shorttext synchronized">Obtener grupos de SAP asignados</p>
    "! @parameter iv_accountexp | <p class="shorttext synchronized">Fecha de caducidad del usuario</p>
    "! @parameter iv_accountcontrol | <p class="shorttext synchronized">Datos de contro de la cuenta</p>
    "! @parameter et_raw_values | <p class="shorttext synchronized">Todos los valores obtenidos</p>
    METHODS get_accountname_info
      IMPORTING iv_accountname    TYPE string
                iv_dn             TYPE ldap_base OPTIONAL
                iv_group          TYPE sap_bool DEFAULT abap_true
                iv_sapgroup       TYPE sap_bool DEFAULT abap_true
                iv_accountexp     TYPE sap_bool DEFAULT abap_true
                iv_accountcontrol TYPE sap_bool DEFAULT abap_true
      EXPORTING et_all_values     TYPE zldap_i_attributes_values
      RAISING   zcx_ldap.
  PROTECTED SECTION.
    CONSTANTS: cv_base_dn TYPE ldap_base VALUE 'DC=puig,DC=amic,DC=net'.
    CONSTANTS: BEGIN OF cs_attributes,
                 principalname   TYPE stringval VALUE 'userPrincipalName',
                 mail            TYPE stringval VALUE 'mail',
                 accountname     TYPE stringval VALUE 'sAMAccountName',
                 groups          TYPE stringval VALUE 'MEMBEROF',
                 sap_groups      TYPE stringval VALUE 'SAPGROUPS',
                 account_expires TYPE stringval VALUE 'ACCOUNTEXPIRES',
                 account_control TYPE stringval VALUE 'USERACCOUNTCONTROL',
               END OF cs_attributes.
    "! <p class="shorttext synchronized">Conexión con servidor</p>
    "! Si no se pasa ID de servidor se conectará al marcado por defecto
    "! @parameter iv_serverid | <p class="shorttext synchronized">ID servidor</p>
    METHODS connect
      IMPORTING
                iv_serverid TYPE ldap_serv OPTIONAL
      RAISING   zcx_ldap.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ldap_query IMPLEMENTATION.

  METHOD constructor.
    connect( iv_serverid ).
  ENDMETHOD.

  METHOD connect.
    DATA lv_serverid TYPE ldapserver-serverid.

    IF iv_serverid IS NOT INITIAL.
      SELECT SINGLE serverid INTO lv_serverid
      FROM ldapserver
      WHERE serverid = iv_serverid.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_ldap
          EXPORTING
            textid = zcx_ldap=>serverid_no_exist.
      ENDIF.
    ELSE.
      SELECT SINGLE serverid INTO lv_serverid
      FROM ldapserver
      WHERE dfault = abap_true.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_ldap
          EXPORTING
            textid = zcx_ldap=>serverid_no_exist.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'LDAP_SYSTEMBIND'
      EXPORTING
        serverid     = lv_serverid
      EXCEPTIONS
        no_authoriz  = 1
        config_error = 2
        nomore_conns = 3
        ldap_failure = 4
        not_alive    = 5
        other_error  = 6
        OTHERS       = 7.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ldap
        EXPORTING
          textid = zcx_ldap=>error_connect_to_server.
    ENDIF.

  ENDMETHOD.

  METHOD get_accountname_from_usermail.
    CLEAR: rv_account_name.


    DATA(lt_values) = query_generic(
      EXPORTING
        iv_dn         = iv_dn
        it_attributes = VALUE #( ( name = cs_attributes-accountname  ) )
        iv_filter     = CONV #( |({ cs_attributes-mail }={ iv_usermail })| ) ).

    IF lt_values IS NOT INITIAL.
      IF lt_values[ 1 ]-value IS NOT INITIAL.
        rv_account_name = lt_values[ 1 ]-value.
      ELSE.
        RAISE EXCEPTION TYPE zcx_ldap
          EXPORTING
            textid = zcx_ldap=>principalname_not_exist.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ldap
        EXPORTING
          textid = zcx_ldap=>principalname_not_exist.
    ENDIF.

  ENDMETHOD.

  METHOD query_generic.
    DATA lt_entries TYPE ldapetab.
    DATA(lv_dn) = COND #( WHEN iv_dn IS NOT INITIAL THEN iv_dn ELSE cv_base_dn ).

    CLEAR rt_values.

    CALL FUNCTION 'LDAP_READ'
      EXPORTING
        base         = lv_dn
        filter       = iv_filter
        attributes   = it_attributes
      IMPORTING
        entries      = lt_entries
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        ldap_failure = 3
        not_alive    = 4
        other_error  = 5
        OTHERS       = 6.
    IF sy-subrc = 0.
      LOOP AT lt_entries ASSIGNING FIELD-SYMBOL(<ls_entries>).

        LOOP AT <ls_entries>-attributes ASSIGNING FIELD-SYMBOL(<ls_attributes_read>).

          rt_values = VALUE #( BASE rt_values FOR <wa> IN <ls_attributes_read>-vals ( attribute = <ls_attributes_read>-name
                                                                                      value = <wa>-val )  ).

        ENDLOOP.

      ENDLOOP.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ldap
        EXPORTING
          textid = zcx_ldap=>error_query.
    ENDIF.
  ENDMETHOD.

  METHOD disconnect.
    CALL FUNCTION 'LDAP_UNBIND'
      EXCEPTIONS
        conn_outdate = 0
        ldap_failure = 0
        not_alive    = 0
        other_error  = 0
        OTHERS       = 0.
  ENDMETHOD.

  METHOD get_accountname_info.
    DATA lt_attributes TYPE ldapastab .

    CLEAR et_all_values.

    IF iv_group = abap_true.
      INSERT VALUE #( name = cs_attributes-groups ) INTO TABLE lt_attributes.
    ENDIF.
    IF iv_sapgroup = abap_true.
      INSERT VALUE #( name = cs_attributes-sap_groups ) INTO TABLE lt_attributes.
    ENDIF.
    IF iv_accountexp = abap_true.
      INSERT VALUE #( name = cs_attributes-account_expires ) INTO TABLE lt_attributes.
    ENDIF.
    IF iv_accountcontrol = abap_true.
      INSERT VALUE #( name = cs_attributes-account_control ) INTO TABLE lt_attributes.
    ENDIF.

    et_all_values = query_generic(
      EXPORTING
        iv_dn         = iv_dn
        it_attributes = lt_attributes
        iv_filter     = CONV #( |({ cs_attributes-accountname }={ iv_accountname })| ) ).

  ENDMETHOD.

ENDCLASS.
