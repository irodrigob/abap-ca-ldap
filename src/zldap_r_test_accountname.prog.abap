*&---------------------------------------------------------------------*
*& Report ZLDAP_R_TEST_ACCOUNTNAME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zldap_r_test_accountname.

PARAMETERS p_name TYPE ad_smtpadr OBLIGATORY LOWER CASE.
PARAMETERS p_servid TYPE ldap_serv.
PARAMETERS p_dn TYPE ldap_base.


START-OF-SELECTION.

  TRY.

      DATA(lo_ldap) = NEW zcl_ldap_query( iv_serverid = p_servid ).

      DATA(lv_username) = lo_ldap->get_accountname_from_usermail( EXPORTING iv_usermail     = CONV #( p_name )
                                                                            iv_dn           = p_dn ).

      WRITE:/'Account name: ', lv_username.

    CATCH zcx_ldap INTO DATA(lo_excep).
      WRITE:/ lo_excep->get_text( ).
  ENDTRY.
