CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE player_type.
    METHODS count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
                VALUE(o_count) TYPE i.
    METHODS board_full
      IMPORTING board TYPE board_type
      RETURNING VALUE(is_full) TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type,
          x_count TYPE i,
          o_count TYPE i,
          full    TYPE abap_bool.

    winner = check_winner( board ).
    full = board_full( board ).
    count_marks( IMPORTING board = board EXPORTING x_count = x_count o_count = o_count ).

    IF x_count - o_count > 1 OR o_count > x_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order'.
    ENDIF.

    IF winner IS NOT INITIAL AND ( x_count + o_count = 9 OR full = abap_true ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Game played after win'.
    ENDIF.

    IF winner IS INITIAL.
      IF full = abap_true.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ELSE.
      state = state_enum-win.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    result = space.
    LOOP AT board INTO DATA(line) FROM 1 TO 3.
      IF line CS 'XXX'.
        result = player_enum-one.
      ELSEIF line CS 'OOO'.
        result = player_enum-two.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DATA: diagonala TYPE player_type,
          diagonalb TYPE player_type.

    diagonala = board  & board  & board .
    diagonalb = board  & board  & board .

    IF diagonala = 'XXX' OR diagonalb = 'XXX'.
      result = player_enum-one.
    ELSEIF diagonala = 'OOO' OR diagonalb = 'OOO'.
      result = player_enum-two.
    ENDIF.

    " Check each column
    DO 3 TIMES.
      DATA(column) = board[ 1 ]( sy-index ) & board[ 2 ]( sy-index ) & board[ 3 ]( sy-index ).
      IF column = 'XXX'.
        result = player_enum-one.
      ELSEIF column = 'OOO'.
        result = player_enum-two.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD count_marks.
    x_count = 0.
    o_count = 0.
    LOOP AT board INTO DATA(line).
      x_count = x_count + strlen( replace( val = line sub = ' ' with = '' ) ).
      o_count = o_count + strlen( replace( val = line sub = ' ' with = '' ) ).
    ENDLOOP.
    x_count = x_count - o_count.
    o_count = ( x_count + o_count ) - x_count.
  ENDMETHOD.

  METHOD board_full.
    is_full = abap_true.
    LOOP AT board INTO DATA(line).
      IF line CS space.
        is_full = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
