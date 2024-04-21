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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    CLASS-METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS: count_marks
      IMPORTING
        board TYPE board_type
        mark  TYPE player_type
      RETURNING
        VALUE(count) TYPE i.
    CLASS-METHODS: check_winner
      IMPORTING
        board TYPE board_type
        mark  TYPE player_type
      RETURNING
        VALUE(is_winner) TYPE abap_bool.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board, player_enum-one ).
    o_count = count_marks( board, player_enum-two ).

    " Validate board state
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for winners
    IF check_winner( board, player_enum-one ).
      IF o_count < x_count - 1.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ELSE.
        state = state_enum-win.
      ENDIF.
    ELSEIF check_winner( board, player_enum-two ).
      IF x_count <> o_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ELSE.
        state = state_enum-win.
      ENDIF.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    count = 0.
    LOOP AT board INTO DATA(row).
      count += strlen( row ) - strlen( replace( val = row sub = mark with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    " Check rows, columns and diagonals for a win condition
    is_winner = abap_false.
    LOOP AT board INTO DATA(row) FROM 1 TO 3.
      IF row = mark && mark && mark.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA(column) TYPE c LENGTH 3.
    DO 3 TIMES.
      column = board[ 1 ][ sy-index ] && board[ 2 ][ sy-index ] && board[ 3 ][ sy-index ].
      IF column = mark && mark && mark.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF board[ 1 ][ 1 ] = mark AND board[ 2 ][ 2 ] = mark AND board[ 3 ][ 3 ] = mark OR
       board[ 1 ][ 3 ] = mark AND board[ 2 ][ 2 ] = mark AND board[ 3 ][ 1 ] = mark.
      is_winner = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
