*&---------------------------------------------------------------------*
*& Report ZLDAP_R_TEST_USERINFO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zldap_r_test_userinfo.

PARAMETERS p_user TYPE string OBLIGATORY.
PARAMETERS p_servid TYPE ldap_serv.
PARAMETERS p_dn TYPE ldap_base.
PARAMETERS p_group AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_sapgrp AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_acntex AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_acntcn AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.

  TRY.

      DATA(lo_ldap) = NEW zcl_ldap_query( iv_serverid = p_servid ).

      lo_ldap->get_accountname_info( EXPORTING iv_accountname = p_user
                                               iv_dn = p_dn
                                               iv_group = p_group
                                               iv_sapgroup = p_sapgrp
                                               iv_accountexp = p_acntex
                                               iv_accountcontrol = p_acntcn
                                     IMPORTING et_all_values = DATA(lt_all_values)  ).

      cl_demo_output=>display_data( lt_all_values ).

    CATCH zcx_ldap INTO DATA(lo_excep).
      WRITE:/ lo_excep->get_text( ).
  ENDTRY.
