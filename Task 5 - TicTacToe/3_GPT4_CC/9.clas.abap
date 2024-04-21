CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS: get_state
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE string
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: check_win FOR TESTING
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(is_winner) TYPE abap_bool.

    METHODS: count_marks FOR TESTING
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(x_count) TYPE i
        VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA(x_count) = 0.
    DATA(o_count) = 0.
    x_count, o_count = count_marks( board ).

    " Validate move count to determine turn order
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order or excessive moves'.
    ENDIF.

    " Check for win conditions
    IF check_win( board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(row).
      " Horizontal
      IF row = 'XXX' OR row = 'OOO'.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Vertical and diagonal checks
    DO 3 TIMES.
      " Vertical
      IF board[1](col:1) = board[2](col:1) AND board[2](col:1) = board[3](col:1) AND board[1](col:1) IS NOT INITIAL.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonal
    IF ( board  = board  AND board  = board  AND board  IS NOT INITIAL ) OR
       ( board  = board  AND board  = board  AND board  IS NOT INITIAL ).
      is_winner = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count = x_count + strlen( replace( val = row regex = '[^X]' with = '' ) ).
      o_count = o_count + strlen( replace( val = row regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
