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

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_win FOR TESTING
      IMPORTING player TYPE player_type
                board  TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS count_occurrences
      IMPORTING value_to_count TYPE c
                board          TYPE board_type
      RETURNING VALUE(count)   TYPE i.

ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: num_x TYPE i,
          num_o TYPE i.

    num_x = count_occurrences( value_to_count = 'X' board = board ).
    num_o = count_occurrences( value_to_count = 'O' board = board ).

    " Check for invalid board state
    IF num_x < num_o OR num_x > num_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for a win condition for both players
    DATA: x_wins TYPE abap_bool,
          o_wins TYPE abap_bool.

    x_wins = check_win( player = player_enum-one board = board ).
    o_wins = check_win( player = player_enum-two board = board ).

    IF x_wins AND o_wins.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ELSEIF x_wins.
      state = state_enum-win.
    ELSEIF o_wins.
      state = state_enum-win.
    ELSEIF num_x + num_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_occurrences.
    count = 0.
    LOOP AT board INTO DATA(row).
      count = count + strlen( replace( val = row sub = ' ' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win.
    DATA: lines TYPE TABLE OF string.

    " Add all rows to the lines
    lines = board.

    " Add all columns to the lines
    DO 3 TIMES.
      DATA(column) = |{ board[ 1 ]+sy-index(1) }{ board[ 2 ]+sy-index(1) }{ board[ 3 ]+sy-index(1) }|.
      APPEND column TO lines.
    ENDDO.

    " Add diagonals to the lines
    APPEND |{ board[ 1 ]+1(1) }{ board[ 2 ]+2(1) }{ board[ 3 ]+3(1) }| TO lines.
    APPEND |{ board[ 3 ]+1(1) }{ board[ 2 ]+2(1) }{ board[ 1 ]+3(1) }| TO lines.

    " Check for winning condition
    LOOP AT lines INTO DATA(line).
      IF line CS |{ player }{ player }{ player }|.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    result = abap_false.
  ENDMETHOD.

ENDCLASS.
