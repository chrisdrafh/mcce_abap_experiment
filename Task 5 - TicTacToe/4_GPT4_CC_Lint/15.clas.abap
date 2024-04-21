CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE boolean.
    METHODS count_occurrences
      IMPORTING board       TYPE board_type
                player      TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_occurrences( board = board player = player_enum-one ).
    o_count = count_occurrences( board = board player = player_enum-two ).

    " Check for invalid board condition
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid turn order or extra moves'.
    ENDIF.

    " Check for win conditions
    IF check_win( board ).
      " Ensure that no extra moves were made after a win
      IF x_count = o_count + 1 OR o_count = x_count - 1.
        RETURN state_enum-win.
      ELSE.
        RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Game continued after a win'.
      ENDIF.
    ENDIF.

    " Check for a draw
    IF x_count + o_count = 9 AND NOT check_win( board ).
      RETURN state_enum-draw.
    ENDIF.

    " If no win or draw, the game is ongoing
    RETURN state_enum-ongoing_game.
  ENDMETHOD.

  METHOD check_win.
    " Define win conditions (rows, columns, diagonals)
    LOOP AT board INTO DATA(row) WHERE row CS player.
      IF row = player && row = player && row = player.
        RETURN abap_true.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    IF board  = player AND board  = player AND board  = player OR
       board  = player AND board  = player AND board  = player OR
       board  = player AND board  = player AND board  = player OR
       board  = player AND board  = player AND board  = player OR
       board  = player AND board  = player AND board  = player.
      RETURN abap_true.
    ENDIF.
    
    RETURN abap_false.
  ENDMETHOD.

  METHOD count_occurrences.
    count = 0.
    LOOP AT board INTO DATA(line).
      count += strlen( line ) - strlen( replace( val = line sub = player with = '' ) ).
    ENDLOOP.
    RETURN count.
  ENDMETHOD.

ENDCLASS.
