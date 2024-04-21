CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    CLASS-DATA: lines TYPE board_type.

    CLASS-METHODS: check_win
      IMPORTING player TYPE player_type
      RETURNING VALUE(is_win) TYPE abap_bool.

    CLASS-METHODS: count_marks
      IMPORTING player TYPE player_type
      RETURNING VALUE(count) TYPE i.

    CLASS-METHODS: is_draw
      RETURNING VALUE(is_draw) TYPE abap_bool.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    lines = board.
    x_count = count_marks( player_enum-one ).
    o_count = count_marks( player_enum-two ).

    " Validate board configuration
    IF x_count < o_count OR x_count - o_count > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check if game should have already ended
    IF check_win( player_enum-one ) OR check_win( player_enum-two ).
      IF x_count + o_count < 9.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
    ENDIF.

    " Determine game state
    IF check_win( player_enum-one ).
      state = state_enum-win.
    ELSEIF check_win( player_enum-two ).
      state = state_enum-win.
    ELSEIF is_draw( ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    LOOP AT lines INTO DATA(line).
      IF line = player && player && player.
        is_win = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: idx TYPE i.
    DO 3 TIMES.
      idx = sy-index.
      IF lines[ 1 ][ idx ] = player AND
         lines[ 2 ][ idx ] = player AND
         lines[ 3 ][ idx ] = player.
        is_win = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF lines[ 1 ][ 1 ] = player AND
       lines[ 2 ][ 2 ] = player AND
       lines[ 3 ][ 3 ] = player.
      is_win = abap_true.
      RETURN.
    ENDIF.

    IF lines[ 1 ][ 3 ] = player AND
       lines[ 2 ][ 2 ] = player AND
       lines[ 3 ][ 1 ] = player.
      is_win = abap_true.
      RETURN.
    ENDIF.

    is_win = abap_false.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT lines INTO DATA(line).
      count += strlen( line ) - strlen( replace( line, player, '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD is_draw.
    is_draw = count_marks( player_enum-one ) + count_marks( player_enum-two ) = 9
              AND NOT check_win( player_enum-one )
              AND NOT check_win( player_enum-two ).
  ENDMETHOD.

ENDCLASS.
