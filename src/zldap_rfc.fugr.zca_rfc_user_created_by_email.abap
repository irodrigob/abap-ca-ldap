FUNCTION zca_rfc_user_created_by_email .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IV_MAIL) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_USER) TYPE  SYUNAME
*"  EXCEPTIONS
*"      FAILURE_CONNECT_LDAP
*"      USERNAME_NOT_EXIST
*"----------------------------------------------------------------------

  TRY .
      DATA(lo_ldap) = NEW zcl_ldap_query( ).

      ev_user = lo_ldap->get_accountname_from_usermail( EXPORTING iv_usermail     = CONV #( iv_mail ) ).

      IF ev_user IS INITIAL.
        RAISE username_not_exist.
      ENDIF.
    CATCH zcx_ldap.
      RAISE username_not_exist.
    CATCH cx_root.
      RAISE failure_connect_ldap.
  ENDTRY.

ENDFUNCTION.
