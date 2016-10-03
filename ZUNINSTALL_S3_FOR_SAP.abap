*&---------------------------------------------------------------------*
*& Report  ZUNINSTALL_S3_FOR_SAP
*&
*&---------------------------------------------------------------------*
*& Jordi Escoda, RocketSteam. 13th Sept 2016
*& This program will uninstall for S3 for SAP.
*& DO NOT USE unless you are sure you want to delete S3 for SAP
*&
*& WARNING: Tested on SAP Netweaver 750. Use at your own risk on other
*& Netweaver versions!. Problems may arise with FM SEO_CLASS_DELETE_COMPLETE
*&---------------------------------------------------------------------*
*  Selection texts:
*  P_DEL  Delete /RS3/* objects
*--------------------------------------------------------------------*
REPORT  zuninstall_s3_for_sap.

PARAMETERS: p_del AS CHECKBOX.

*--------------------------------------------------------------------*
* Class lcx_exception
*--------------------------------------------------------------------*
CLASS lcx_exception DEFINITION FINAL
                    INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    METHODS:
      constructor IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
                            previous LIKE previous OPTIONAL.
    CLASS-METHODS:
      raise_from_sy_msg IMPORTING previous LIKE previous OPTIONAL
                        RAISING   lcx_exception.
    METHODS:
      get_text REDEFINITION.
ENDCLASS.                    "lcx_exception DEFINITION

*--------------------------------------------------------------------*
* Class lcx_exception
*--------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.
*--------------------------------------------------------------------*
* Constructor
*--------------------------------------------------------------------*
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    if_t100_message~t100key = textid.
  ENDMETHOD.                    "constructor

*--------------------------------------------------------------------*
* Raises filling with sy-msg
*--------------------------------------------------------------------*
  METHOD raise_from_sy_msg.
    DATA: ls_t100key LIKE if_t100_message=>t100key.

    ls_t100key-msgid = sy-msgid.
    ls_t100key-msgno = sy-msgno.
    ls_t100key-attr1 = sy-msgv1.
    ls_t100key-attr2 = sy-msgv2.
    ls_t100key-attr3 = sy-msgv3.
    ls_t100key-attr4 = sy-msgv4.

    RAISE EXCEPTION TYPE lcx_exception
      EXPORTING
        textid   = ls_t100key
        previous = previous.
  ENDMETHOD.                    "raise_from_sy_msg

*--------------------------------------------------------------------*
* Returns text
*--------------------------------------------------------------------*
  METHOD get_text.
    DATA: lv_msg TYPE string.

    lv_msg = super->get_text( ).
    REPLACE ALL OCCURRENCES OF '&' IN lv_msg WITH space.
    result = lv_msg.
  ENDMETHOD.                    "get_text

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_uninstall DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uninstall DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      execute.

  PRIVATE SECTION.
    CLASS-DATA:
      lv_m_corrnum TYPE e070-trkorr,
      lt_m_tadir   TYPE STANDARD TABLE OF tadir.
    CLASS-METHODS:
      select,
      delete_prog_all
        RAISING lcx_exception,
      delete_clas_all
        RAISING lcx_exception,
      delete_fugr_all
        RAISING lcx_exception,
      delete_tabl_all
        RAISING lcx_exception,
      delete_ttyp_all
        RAISING lcx_exception,
      delete_dtel_all
        RAISING lcx_exception,
      delete_doma_all
        RAISING lcx_exception,
      delete_msag_all
        RAISING lcx_exception,
      delete_devc_all
        RAISING lcx_exception,
      delete_tobj_all,
      delete_prog
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_clas
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_fugr
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_tabl
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_ttyp
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_dtel
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_doma
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_msag
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      delete_tobj
        IMPORTING im_obj_name TYPE tadir-obj_name,
      delete_devc
        IMPORTING im_obj_name TYPE tadir-obj_name
        RAISING   lcx_exception,
      ddif_object_delete
        IMPORTING im_objname TYPE ddobjname
                  im_objtype TYPE ddobjtyp
        RAISING   lcx_exception,
      delete_tadir
        IMPORTING im_object   TYPE tadir-object
                  im_obj_name TYPE tadir-obj_name,
      is_final_class
        IMPORTING im_obj_name              TYPE tadir-obj_name
        RETURNING VALUE(re_is_final_class) TYPE abap_bool,
      class_has_subclass
        IMPORTING im_obj_name            TYPE tadir-obj_name
        RETURNING VALUE(re_has_subclass) TYPE abap_bool,
      package_has_subpackages
        IMPORTING im_obj_name               TYPE tadir-obj_name
        RETURNING VALUE(re_has_subpackages) TYPE abap_bool
        RAISING   lcx_exception,
      delete_smodi,
      delete_reposrc,
      check_netweaver_version
        RAISING lcx_exception.

ENDCLASS.                    "lcl_uninstall DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_uninstall IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_uninstall IMPLEMENTATION.
  METHOD execute.
    DATA: lv_msg TYPE string.
    DATA: lr_exception TYPE REF TO lcx_exception.

    IF p_del = abap_true.
      IF sy-sysid <> 'IDE'. "Prevents delete on original system
        select( ).
        IF lt_m_tadir[] IS INITIAL.
          WRITE:/ 'No existing objects /RS3/'.
        ENDIF.
        TRY.
            check_netweaver_version( ).
            delete_prog_all( ).
            delete_fugr_all( ).
            delete_clas_all( ).
            delete_tabl_all( ).
            delete_ttyp_all( ).
            delete_dtel_all( ).
            delete_doma_all( ).
            delete_msag_all( ).
            delete_tobj_all( ).
            delete_devc_all( ).
            delete_reposrc( ).
            delete_smodi( ).
          CATCH lcx_exception INTO lr_exception.
            lv_msg = lr_exception->get_text( ).
            FORMAT COLOR COL_NEGATIVE.
            WRITE:/ 'Exception:', lv_msg.
            FORMAT COLOR OFF.
        ENDTRY.
      ELSE.
        FORMAT COLOR COL_NEGATIVE.
        WRITE:/ 'Original system!!!. Do not run!!!'.
        FORMAT COLOR OFF.
      ENDIF.
    ELSE.
      WRITE:/ 'Nothing deleted'.
    ENDIF.
  ENDMETHOD.                    "execute

  METHOD select.

    SELECT *
        INTO TABLE lt_m_tadir
    FROM tadir
    WHERE pgmid = 'R3TR'
      AND obj_name LIKE '/RS3/%'.

  ENDMETHOD.                    "select

  METHOD delete_prog_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'PROG'.
      delete_prog( ls_tadir-obj_name ).
    ENDLOOP.
  ENDMETHOD.                    "delete_prog_all

  METHOD delete_clas_all.
    DATA: ls_tadir TYPE tadir.

*   Delete final classes
    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'CLAS'
                         AND obj_name CN 'CX'.
      IF is_final_class( ls_tadir-obj_name ) = abap_true.
        delete_clas( ls_tadir-obj_name ).
      ENDIF.
    ENDLOOP.

*   Delete rest of classes
    DO 5 TIMES.
      select( ).
      LOOP AT lt_m_tadir INTO ls_tadir
                         WHERE object = 'CLAS'
                         AND obj_name(7) <> '/RS3/CX'.
        IF class_has_subclass( ls_tadir-obj_name ) = abap_false.
          delete_clas( ls_tadir-obj_name ).
        ENDIF.
      ENDLOOP.
    ENDDO.

*   Delete exception classes.
    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'CLAS'
                         AND obj_name(7) = '/RS3/CX'.
      delete_clas( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_clas_all

  METHOD delete_fugr_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'FUGR'.
      delete_fugr( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_fugr_all


  METHOD delete_tabl_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'TABL'.
      delete_tabl( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_tabl_all

  METHOD delete_ttyp_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'TTYP'.
      delete_ttyp( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_ttyp_all

  METHOD delete_dtel_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'DTEL'.
      delete_dtel( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_dtel_all

  METHOD delete_doma_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'DOMA'.
      delete_doma( im_obj_name = ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_doma_all

  METHOD delete_msag_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'MSAG'.
      delete_msag( ls_tadir-obj_name ).
    ENDLOOP.

  ENDMETHOD.                    "delete_msag_all

  METHOD delete_tobj_all.
    DATA: ls_tadir TYPE tadir.

    LOOP AT lt_m_tadir INTO ls_tadir
                       WHERE object = 'TOBJ'.
      delete_tobj( ls_tadir-obj_name ).
    ENDLOOP.
  ENDMETHOD.                    "delete_tobj_all

  METHOD delete_devc_all.
    DATA: ls_tadir TYPE tadir.

    DO 3 TIMES.
      select( ).
      LOOP AT lt_m_tadir INTO ls_tadir
                         WHERE object = 'DEVC'.
        IF package_has_subpackages( ls_tadir-obj_name ) = abap_false.
          delete_devc( ls_tadir-obj_name ).
        ENDIF.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.                    "delete_devc_all


  METHOD delete_prog.
    DATA: lv_program TYPE sy-repid.
    DATA: lv_msg TYPE string.

    lv_program = im_obj_name.
    CALL FUNCTION 'SCWG_TOOLFLAG_SET'.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        corrnumber         = lv_m_corrnum
        program            = lv_program
        suppress_checks    = 'X'
        suppress_popup     = 'X'
      EXCEPTIONS
        enqueue_lock       = 1
        object_not_found   = 2
        permission_failure = 3
        reject_deletion    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

    delete_tadir( im_object = 'PROG'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'PROG', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_prog

  METHOD delete_clas.
    DATA: lv_clskey TYPE seoclskey.
    DATA: lv_msg TYPE string.

    lv_clskey = im_obj_name.
    CALL FUNCTION 'SCWG_TOOLFLAG_SET'.

    TRY.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey               = lv_clskey
            genflag              = abap_false
            authority_check      = abap_false
            suppress_docu_delete = abap_false
            suppress_commit      = abap_false
            suppress_corr        = abap_true
            suppress_dialog      = abap_true
          CHANGING
            corrnr               = lv_m_corrnum
          EXCEPTIONS
            not_existing         = 1
            is_interface         = 2
            db_error             = 3
            no_access            = 4
            other                = 5
            OTHERS               = 6.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          lcx_exception=>raise_from_sy_msg( ).
        ENDIF.
      CATCH cx_sy_dyn_call_param_not_found.
*       IN Netweaver 700 parameters  suppress_corr and suppress_dialog are not existing
*       Probably the system will claim:
        "No valid change license exists for namespace /RS3/"
*       Note this program has been tested for Netweaver 750.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey               = lv_clskey
            genflag              = abap_false
            authority_check      = abap_false
            suppress_docu_delete = abap_false
            suppress_commit      = abap_false
*           suppress_corr        = abap_true
*           suppress_dialog      = abap_true
          CHANGING
            corrnr               = lv_m_corrnum
          EXCEPTIONS
            not_existing         = 1
            is_interface         = 2
            db_error             = 3
            no_access            = 4
            other                = 5
            OTHERS               = 6.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          lcx_exception=>raise_from_sy_msg( ).
        ENDIF.
    ENDTRY.

    delete_tadir( im_object = 'CLAS'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'CLAS', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_clas

  METHOD is_final_class.
    SELECT SINGLE clsfinal
            INTO re_is_final_class
    FROM seoclassdf
    WHERE clsname = im_obj_name
      AND category <> '40'. "Not exception classes
  ENDMETHOD.                    "is_final_class

  METHOD class_has_subclass.
    DATA: ls_seometarel TYPE seometarel.

    SELECT SINGLE *
        INTO ls_seometarel
    FROM seometarel
    WHERE refclsname = im_obj_name.

    IF sy-subrc = 0.
      re_has_subclass = abap_true.
    ENDIF.
  ENDMETHOD.                    "class_has_subclass

  METHOD delete_fugr.
    DATA: lv_area TYPE rs38l-area.
    DATA: lv_msg TYPE string.
    DATA: lv_namespace TYPE rs38l-namespace.
    DATA: lv_group TYPE rs38l-area.
    DATA: lv_prog TYPE trdir-name.
    DATA: lt_tfdir TYPE STANDARD TABLE OF tfdir,
          ls_tfdir TYPE tfdir.

    lv_area = im_obj_name.
    CALL FUNCTION 'SCWG_TOOLFLAG_SET'.

*   Builds program name /RS3/USER -> /RS3/SAPLUSER
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.
    CONCATENATE lv_namespace 'SAPL' lv_group INTO lv_prog.

*   Selects all Function Modules pertaining to Function Group
    SELECT *
      INTO TABLE lt_tfdir
    FROM tfdir
    WHERE pname = lv_prog.

    LOOP AT lt_tfdir INTO ls_tfdir.
      CALL FUNCTION 'FUNCTION_DELETE'
        EXPORTING
          funcname      = ls_tfdir-funcname
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        lcx_exception=>raise_from_sy_msg( ).
      ENDIF.
      WRITE:/ 'Function Module', ls_tfdir-funcname, 'Deleted successfully'.
    ENDLOOP.

    CALL FUNCTION 'FUNCTION_POOL_DELETE'
      EXPORTING
        pool             = lv_area
      EXCEPTIONS
        functions_exists = 1
        not_found        = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

    delete_tadir( im_object = 'FUGR'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'FUGR', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_fugr

  METHOD delete_tabl.
    DATA: lv_objname TYPE ddobjname.

    lv_objname = im_obj_name.

    ddif_object_delete( im_objname = lv_objname
                        im_objtype = 'TABL' ).

    delete_tadir( im_object = 'TABL'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'TABL', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_tabl


  METHOD delete_ttyp.
    DATA: lv_objname TYPE ddobjname.

    lv_objname = im_obj_name.

    ddif_object_delete( im_objname = lv_objname
                        im_objtype = 'TTYP' ).

    delete_tadir( im_object = 'TTYP'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'TTYP', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_ttyp

  METHOD delete_dtel.
    DATA: lv_objname TYPE ddobjname.

    lv_objname = im_obj_name.

    ddif_object_delete( im_objname = lv_objname
                        im_objtype = 'DTEL' ).

    delete_tadir( im_object = 'DTEL'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'DTEL', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_DTEL


  METHOD delete_doma.
    DATA: lv_objname TYPE ddobjname.

    lv_objname = im_obj_name.

    ddif_object_delete( im_objname = lv_objname
                        im_objtype = 'DOMA' ).

    delete_tadir( im_object = 'DOMA'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'DOMA', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_doma

  METHOD delete_msag.
*   Coce based on RS_DELETE_MESSAGE_ID
    DATA: lv_object TYPE dokil-object.
    DATA: lv_msg TYPE string.

    CALL FUNCTION 'SCWG_TOOLFLAG_SET'.

    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = 'NA'
        element  = im_obj_name
        addition = '   '
      IMPORTING
        object   = lv_object.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id                        = 'NA'
        doku_object                    = lv_object
        generic_use                    = abap_true
        suppress_authority             = abap_true
        suppress_enqueue               = abap_true
        suppress_transport             = abap_true
      EXCEPTIONS
        header_without_text            = 1
        index_without_header           = 2
        no_authority_for_devclass_xxxx = 3
        no_docu_found                  = 4
        object_is_already_enqueued     = 5
        object_is_enqueued_by_corr     = 6
        techn_enqueue_problem          = 7
        user_break                     = 8
        OTHERS                         = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

    DELETE FROM t100a
    WHERE arbgb = im_obj_name.
    IF sy-subrc <> 0 AND sy-subrc <> 4.
      MESSAGE s018(e4) WITH 'T100A' INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ELSE.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          object    = im_obj_name
          operation = 'DELETE'
          program   = space
          type      = 'CN'.

      DELETE FROM t100o
      WHERE arbgb = im_obj_name.

      DELETE FROM t100t
      WHERE arbgb = im_obj_name.

      DELETE FROM t100u
      WHERE arbgb = im_obj_name.

      DELETE FROM t100x
      WHERE arbgb = im_obj_name.

      DELETE FROM t100
      WHERE arbgb = im_obj_name.
      IF sy-subrc <> 0 AND sy-subrc <> 4.
        MESSAGE s018(e4) WITH 'T100' INTO lv_msg.
        lcx_exception=>raise_from_sy_msg( ).
      ENDIF.
    ENDIF.
    delete_tadir( im_object = 'MSAG'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'MSAG', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_msag

  METHOD delete_tobj.
    delete_tadir( im_object = 'TOBJ'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'TOBJ', im_obj_name, 'Deleted successfully'.
  ENDMETHOD.                    "delete_tobj

  METHOD delete_devc.
*   Code Based on CL_PACKAGE->M_DELETE_DB
    DATA: lv_package_name TYPE devclass.

    lv_package_name = im_obj_name.

    DELETE FROM tdevc
    WHERE devclass = im_obj_name.

    DELETE FROM tdevct
    WHERE devclass = im_obj_name.

    DELETE FROM permission
    WHERE client_pak = im_obj_name.

    CALL FUNCTION 'WB_TREE_UPDATE_OBJECTLIST'
      EXPORTING
        p_object_type  = 'K  '    "swbm_c_type_package
        p_object_name  = lv_package_name
        p_operation    = 'DELETE' "swbm_c_op_delete
        p_package_name = lv_package_name
        p_author       = sy-uname
      EXCEPTIONS
        OTHERS         = 0.

    delete_tadir( im_object = 'DEVC'
                  im_obj_name = im_obj_name ).

    WRITE:/ 'DEVC', im_obj_name, 'Deleted successfully'.

  ENDMETHOD.                    "delete_devc

  METHOD package_has_subpackages.
    DATA: ls_tdevc TYPE tdevc.

    SELECT SINGLE *
       INTO ls_tdevc
    FROM tdevc
    WHERE parentcl = im_obj_name.

    IF sy-subrc = 0.
      re_has_subpackages = abap_true.
    ENDIF.

  ENDMETHOD.                    "package_has_subpackages


  METHOD ddif_object_delete.
    DATA: lv_msg TYPE string.

    CALL FUNCTION 'SCWG_TOOLFLAG_SET'.

    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = im_objtype
        name          = im_objname
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

  ENDMETHOD.                    "ddif_object_delete

  METHOD delete_tadir.
    DATA: ls_tadir_old TYPE tadir,
          ls_tadir_new TYPE tadir.

*   Simulates what is done in TRINT_TADIR_DELETE, skipping checks
    ls_tadir_old-pgmid = 'R3TR'.
    ls_tadir_old-object = im_object.
    ls_tadir_old-obj_name = im_obj_name.
    SELECT SINGLE *
          INTO ls_tadir_old
    FROM tadir
    WHERE pgmid = ls_tadir_old-pgmid
      AND object = ls_tadir_old-object
      AND obj_name = ls_tadir_old-obj_name.

    IF sy-subrc = 0.
      DELETE tadir FROM ls_tadir_old.
    ENDIF.
    CALL FUNCTION 'RS_MODIFY_OBJECT_PROPERTIES'
      EXPORTING
        new_adir  = ls_tadir_new
        old_adir  = ls_tadir_old
        operation = 'DELETE'.

  ENDMETHOD.                    "delete_tadir

  METHOD delete_smodi.
    DELETE FROM smodisrc
    WHERE obj_name LIKE '/RS3/%'.

    DELETE FROM smodilog
    WHERE obj_name LIKE '/RS3/%'.
  ENDMETHOD.                    "delete_smodi

  METHOD delete_reposrc.
    DATA: lt_reposrc TYPE STANDARD TABLE OF reposrc.
    DATA: ls_reposrc TYPE reposrc.

    SELECT *
       INTO TABLE lt_reposrc
    FROM reposrc
    WHERE progname LIKE '/RS3/%'.

    LOOP AT lt_reposrc INTO ls_reposrc.
      DELETE REPORT ls_reposrc-progname.
      WRITE:/ 'REPORT', ls_reposrc-progname, 'Deleted successfully'.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_netweaver_version.
    DATA: lv_msg TYPE string.
    DATA: lt_comptab TYPE STANDARD TABLE OF spam_cvers,
          ls_comptab TYPE spam_cvers.

    CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_comptab
      EXCEPTIONS
        no_release_found = 1
        wrong_release    = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

    READ TABLE lt_comptab WITH KEY component = 'SAP_BASIS'
          INTO ls_comptab.
    IF ls_comptab-release < 750.
      MESSAGE e398(00) WITH 'Program not tested for Netweaver < 750' INTO lv_msg.
      lcx_exception=>raise_from_sy_msg( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.                    "lcl_uninstall IMPLEMENTATION

START-OF-SELECTION.
  lcl_uninstall=>execute( ).
