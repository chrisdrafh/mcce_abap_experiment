CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3.

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
    METHODS check_board_validity
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_for_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_symbols
      IMPORTING board      TYPE board_type
      EXPORTING count_x    TYPE i
                count_o    TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i.

    " Validate the board first
    check_board_validity( board ).

    " Count symbols
    count_symbols( IMPORTING board = board
                   EXPORTING count_x = count_x
                             count_o = count_o ).

    " Check if there's a winner
    DATA(winner) = check_for_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_board_validity.
    DATA: count_x TYPE i,
          count_o TYPE i.

    count_symbols( IMPORTING board = board
                   EXPORTING count_x = count_x
                             count_o = count_o ).

    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Additional checks for already-won scenario to be added here
  ENDMETHOD.

  METHOD check_for_winner.
    " Implementation to check all win conditions: rows, columns, diagonals
    " Return 'X' or 'O' if there's a winner
  ENDMETHOD.

  METHOD count_symbols.
    " Count the number of X's and O's on the board
    count_x = 0.
    count_o = 0.
    LOOP AT board INTO DATA(line).
      count_x = count_x + strlen( regex_replace( val = line regex = '[^X]' with = '' ) ).
      count_o = count_o + strlen( regex_replace( val = line regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
