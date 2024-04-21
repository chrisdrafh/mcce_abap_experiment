CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3,
           tt_counts TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-METHODS: check_board_validity
      IMPORTING
        board TYPE board_type
      RAISING
        cx_parameter_invalid.

    CLASS-METHODS: check_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(winner) TYPE player_type
      RAISING
        cx_parameter_invalid.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type.

    " Check board validity
    check_board_validity( board ).

    " Check for a winner
    TRY.
        winner = check_winner( board ).
      CATCH cx_parameter_invalid.
        " If an invalid parameter exception is caught, re-raise it
        RAISE cx_parameter_invalid.
    ENDTRY.

    " Determine game state based on winner check
    IF winner IS INITIAL.
      " No winner, check if board is full or game is ongoing
      IF lines( board ) = 3 AND strlen( condense( board[ 1 ] ) & condense( board[ 2 ] ) & condense( board[ 3 ] ) ) = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ELSE.
      state = state_enum-win.
    ENDIF.
  ENDMETHOD.

  METHOD check_board_validity.
    DATA: count_x TYPE i,
          count_o TYPE i,
          total_count TYPE i.

    LOOP AT board INTO DATA(row).
      count_x = count_x + strlen( replace_all( val = row regex = '[^X]' with = '' ) ).
      count_o = count_o + strlen( replace_all( val = row regex = '[^O]' with = '' ) ).
    ENDLOOP.

    total_count = count_x + count_o.

    IF total_count <> 9 AND ( count_x - count_o NOT IN 0 TO 1 ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    DATA: lines TYPE tt_counts.

    " Horizontal, Vertical, and Diagonal Checks
    " Add implementation based on actual game rules and logic to determine a winner
    " For example:
    " - Check each row for three X's or O's
    " - Check each column for three X's or O's
    " - Check diagonals for three X's or O's

    " Placeholder to represent a logic which checks for a winner
    " Replace it with actual conditions
    lines = VALUE #(
                     ( COND #( WHEN line EQ 'XXX' OR line EQ 'OOO' THEN 1 ELSE 0 ) FOR line IN board ) " Horizontal
                     ( 1 ) " Verticals and Diagonals calculated similarly
                   ).

    IF 1 IN lines.
      winner = COND #( WHEN 'XXX' IN board OR 'OOO' IN board THEN 'X' ELSE 'O' ).
    ELSE.
      winner = ''.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
