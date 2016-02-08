#Example usage: 

data lo_applog type ref to zbc_application_log.
lo_applog = zbc_application_log=>get_instance( object = 'ZPA_BEN' subobject = 'BENDOC_FAX' external_id = '01234' ).
lo_applog->add_text( 'test' ).
lo_applog->add_text( 'test2' ).
lo_applog->add_text( 'test3' ).
lo_applog->save( ).
lo_applog->display( ).
