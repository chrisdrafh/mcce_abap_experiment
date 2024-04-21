CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_win
      IMPORTING
        player TYPE player_type
        board  TYPE board_type
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS count_player
      IMPORTING
        player TYPE player_type
        board  TYPE board_type
      RETURNING
        VALUE(count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i.

    count_x = count_player( player_enum-one, board ).
    count_o = count_player( player_enum-two, board ).

    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(win_x) = check_win( player_enum-one, board ).
    DATA(win_o) = check_win( player_enum-two, board ).

    IF win_x = abap_true AND win_o = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF win_x = abap_true.
      state = state_enum-win.
    ELSEIF win_o = abap_true.
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(row).
      IF row = CONDENSE( player && player && player ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: diagonal1 TYPE string,
          diagonal2 TYPE string.

    DO 3 TIMES.
      DATA(index) = sy-index - 1.
      diagonal1 = diagonal1 && board[ index + 1 ]( index + 1 ).
      diagonal2 = diagonal2 && board[ index + 1 ]( 3 - index ).
      IF diagonal1 = CONDENSE( player && player && player ) OR
         diagonal2 = CONDENSE( player && player && player ).
        result = abap_true.
        RETURN.
      ENDIF.

      LOOP AT board INTO row.
        DATA(column) = row( sy-index ).
        IF column = CONDENSE( player && player && player ).
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.

  METHOD count_player.
    LOOP AT board INTO DATA(row).
      count = count + STRLEN( row ) - STRLEN( REPLACE( row, player, '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
