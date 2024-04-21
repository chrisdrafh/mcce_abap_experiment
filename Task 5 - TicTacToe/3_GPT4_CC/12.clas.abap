CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF board_row_type,
             col1 TYPE c LENGTH 1,
             col2 TYPE c LENGTH 1,
             col3 TYPE c LENGTH 1,
           END OF board_row_type,
           board_type TYPE TABLE OF board_row_type WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE c LENGTH 1 VALUE 'X',
                 two TYPE c LENGTH 1 VALUE 'O',
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
    METHODS check_win FOR BOARD_TYPE
      IMPORTING player TYPE c LENGTH 1
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_symbols
      IMPORTING symbol TYPE c LENGTH 1
                board  TYPE board_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_symbols( player_enum-one, board ).
    o_count = count_symbols( player_enum-two, board ).

    " Validate board state
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = `Invalid turn order or excess moves`.
    ENDIF.

    " Check win conditions
    IF check_win( player_enum-one ) AND check_win( player_enum-two ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = `Multiple winners not possible`.
    ELSEIF check_win( player_enum-one ).
      state = state_enum-win.
    ELSEIF check_win( player_enum-two ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    result = abap_false.

    LOOP AT board INTO DATA(row).
      " Check horizontal
      IF row-col1 = player AND row-col2 = player AND row-col3 = player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check verticals and diagonals
    IF ( board[ 1 ]-col1 = player AND board[ 2 ]-col1 = player AND board[ 3 ]-col1 = player ) OR
       ( board[ 1 ]-col2 = player AND board[ 2 ]-col2 = player AND board[ 3 ]-col2 = player ) OR
       ( board[ 1 ]-col3 = player AND board[ 2 ]-col3 = player AND board[ 3 ]-col3 = player ) OR
       ( board[ 1 ]-col1 = player AND board[ 2 ]-col2 = player AND board[ 3 ]-col3 = player ) OR
       ( board[ 1 ]-col3 = player AND board[ 2 ]-col2 = player AND board[ 3 ]-col1 = player ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_symbols.
    count = 0.
    LOOP AT board INTO DATA(row).
      IF row-col1 = symbol.
        count += 1.
      ENDIF.
      IF row-col2 = symbol.
        count += 1.
      ENDIF.
      IF row-col3 = symbol.
        count += 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
