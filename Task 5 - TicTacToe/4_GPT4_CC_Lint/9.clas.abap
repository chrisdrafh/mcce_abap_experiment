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
    METHODS count_char
      IMPORTING board    TYPE board_type
                char     TYPE c LENGTH 1
      RETURNING VALUE(count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA(count_x) = count_char( board = board char = 'X' ).
    DATA(count_o) = count_char( board = board char = 'O' ).

    " Check if the board state is valid
    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for winner
    DATA(winner) = check_winner( board ).
    IF winner IS NOT INITIAL.
      " Validate if the game continued after a win
      IF winner = 'X' AND count_x = count_o + 1.
        RETURN 'Win'.
      ELSEIF winner = 'O' AND count_x = count_o.
        RETURN 'Win'.
      ELSE.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
    ENDIF.

    " Check for draw or ongoing game
    IF count_x + count_o = 9.
      RETURN state_enum-draw.
    ELSE.
      RETURN state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(line).
      " Check horizontal
      IF line CS 'XXX'.
        result = 'X'.
        RETURN.
      ELSEIF line CS 'OOO'.
        result = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check vertical and diagonal
    DATA(grid) = VALUE abap_trans( LET x = 1 y = 2 z = 3 IN
                                   DIMENSION( COLS = 3 ROWS = 3 )
                                   INIT VALUE #(
                                      ( board  board  board  )
                                      ( board  board  board  )
                                      ( board  board  board  )
                                      ( board  board  board  )
                                      ( board  board  board  ) ) ).

    LOOP AT grid INTO DATA(column_or_diagonal).
      IF column_or_diagonal CS 'XXX'.
        result = 'X'.
        RETURN.
      ELSEIF column_or_diagonal CS 'OOO'.
        result = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_char.
    LOOP AT board INTO DATA(row).
      count += strlen( replace( val = row sub = | | with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
