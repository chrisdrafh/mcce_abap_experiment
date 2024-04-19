CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF board_line,
             cells TYPE string LENGTH 3,
           END OF board_line,
           board_type TYPE STANDARD TABLE OF board_line WITH DEFAULT KEY INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 x TYPE c LENGTH 1 VALUE 'X',
                 o TYPE c LENGTH 1 VALUE 'O',
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
    METHODS check_win FOR board TYPE board_type RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_player_moves FOR player TYPE c LENGTH 1 USING board TYPE board_type RETURNING VALUE(count) TYPE i.
    METHODS is_valid_board FOR board TYPE board_type RETURNING VALUE(valid) TYPE abap_bool RAISING cx_parameter_invalid.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_player_moves( player_enum-x, board ).
    o_count = count_player_moves( player_enum-o, board ).

    " Check for an invalid board scenario
    IF NOT is_valid_board( board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Determine game result
    IF check_win( board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    " Check all win conditions (rows, columns, diagonals)
    LOOP AT board INTO DATA(line).
      IF line-cells = 'XXX' OR line-cells = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      DATA(column) = |{ board[ 1 ]-cells+sy-index(1) }{ board[ 2 ]-cells+sy-index(1) }{ board[ 3 ]-cells+sy-index(1) }|.
      IF column = 'XXX' OR column = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    DATA(diagonal1) = |{ board[ 1 ]-cells+1(1) }{ board[ 2 ]-cells+2(1) }{ board[ 3 ]-cells+3(1) }|.
    DATA(diagonal2) = |{ board[ 1 ]-cells+3(1) }{ board[ 2 ]-cells+2(1) }{ board[ 3 ]-cells+1(1) }|.
    IF diagonal1 = 'XXX' OR diagonal1 = 'OOO' OR diagonal2 = 'XXX' OR diagonal2 = 'OOO'.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD count_player_moves.
    " Count moves for a specified player
    LOOP AT board INTO DATA(line).
      count += strlen( replace( val = line-cells sub = ' ' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD is_valid_board.
    " Validate the game board
    IF abs( x_count - o_count ) > 1 OR x_count < o_count.
      valid = abap_false.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid board: incorrect number of player moves'.
    ELSE.
      valid = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
