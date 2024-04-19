CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-DATA: winner TYPE player_type.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_occurrences
      IMPORTING
        char  TYPE c LENGTH 1
        board TYPE board_type
      RETURNING
        VALUE(count) TYPE i.

    METHODS check_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(has_winner) TYPE abap_bool.

    METHODS validate_board
      IMPORTING
        board TYPE board_type.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i.

    validate_board( board ).

    count_x = count_occurrences( player_enum-one, board ).
    count_o = count_occurrences( player_enum-two, board ).

    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_winner( board ).
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_occurrences.
    count = 0.
    LOOP AT board INTO DATA(line).
      count += strlen( replace( val = line sub = ` ` with = `` ) ).
      count += count( val = line sub = char ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    DATA: win_patterns TYPE TABLE OF string WITH NON-UNIQUE DEFAULT KEY.

    win_patterns = VALUE #(
      ( `XXX......` ) ( `...XXX...` ) ( `......XXX` )  " Rows
      ( `X..X..X..` ) ( `.X..X..X.` ) ( `..X..X..X` )  " Columns
      ( `X...X...X` ) ( `..X.X.X..` )                " Diagonals
    ).

    LOOP AT win_patterns INTO DATA(pattern).
      IF replace( val = board INTO CORRESPONDING #( pattern ) sub = 'X' with = winner ) = pattern.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    has_winner = abap_false.
  ENDMETHOD.

  METHOD validate_board.
    DATA: board_string TYPE string.

    CONCATENATE LINES OF board INTO board_string.
    IF strlen( replace( val = board_string sub = ` ` with = `` ) ) > 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
