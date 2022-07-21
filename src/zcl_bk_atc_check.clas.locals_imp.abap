CLASS meta_data DEFINITION
    FINAL.
PUBLIC SECTION.

  methods constructor
    importing
      check_moves type abap_bool
      check_computes type abap_bool.
  INTERFACES if_ci_atc_check_meta_data.

    data check_moves type abap_bool.
  data check_computes type abap_bool.
ENDCLASS.

CLASS meta_data IMPLEMENTATION.

  METHOD if_ci_atc_check_meta_data~get_attributes.
    attributes = value #(
      ( name = `CheckMoves` kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean value = ref #( check_moves ) )
      ( name = `CheckComputes` kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean value = ref #( check_computes ) )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_checked_object_types.
    types = VALUE #(
      ( 'PROG' )
      ( 'CLAS' )
      ( 'FUGR' )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_description.
    description = `Steampunk API POC: Detect move statements`.
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_finding_code_infos.
    finding_code_infos = VALUE #(
*      ( code = zbj_cl_ci_test_steam_api=>finding_codes-dummy text = `This is a dummy finding` severity = if_ci_atc_check=>finding_severities-note )
*      ( code = zbj_cl_ci_test_steam_api=>finding_codes-move text = `The move statement is obsolete` pseudo_comment = 'MOVE_OK' )
*      ( code = zbj_cl_ci_test_steam_api=>finding_codes-db_access text = `Database access: &1` pseudo_comment = 'DB_OK' )
    ).
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_category.

  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~uses_checksums.

  ENDMETHOD.

    METHOD constructor.
    me->check_moves = check_moves.
    me->check_computes = check_computes.
  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~get_quickfix_code_infos.

  ENDMETHOD.

  METHOD if_ci_atc_check_meta_data~is_remote_enabled.

  ENDMETHOD.

ENDCLASS.
