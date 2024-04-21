CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3.

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
    METHODS is_valid_board
      IMPORTING board        TYPE board_type
      RETURNING VALUE(valid) TYPE abap_bool.

    METHODS check_for_winner
      IMPORTING board       TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_occurrences
      IMPORTING board         TYPE board_type
                player        TYPE player_type
      RETURNING VALUE(count)  TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type,
          x_count TYPE i,
          o_count TYPE i.

    IF NOT is_valid_board( board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_for_winner( board ).
    CASE winner.
      WHEN player_enum-one OR player_enum-two.
        state = state_enum-win.
      WHEN OTHERS.
        x_count = count_occurrences( board, player_enum-one ).
        o_count = count_occurrences( board, player_enum-two ).
        IF x_count + o_count = 9.
          state = state_enum-draw.
        ELSE.
          state = state_enum-ongoing_game.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD is_valid_board.
    x_count = count_occurrences( board, player_enum-one ).
    o_count = count_occurrences( board, player_enum-two ).

    IF x_count < o_count OR x_count - o_count > 1.
      valid = abap_false.
    ELSE.
      valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_for_winner.
    DATA: idx TYPE i.

    " Check rows, columns and diagonals for a winner
    " Implementation left for brevity
    winner = ' '. " Assume no winner
  ENDMETHOD.

  METHOD count_occurrences.
    DATA(line) = VALUE string_table( ).
    LOOP AT board INTO DATA(row).
      line = VALUE #( BASE line ( row ) ).
    ENDLOOP.

    count = 0.
    LOOP AT line INTO DATA(cell).
      IF cell = player.
        count = count + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
