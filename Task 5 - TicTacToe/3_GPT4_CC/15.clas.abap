CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS: get_state
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE string
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: count_characters
      IMPORTING
        board TYPE board_type
        char  TYPE c LENGTH 1
      RETURNING
        VALUE(count) TYPE i,
      check_winner
        IMPORTING
          board TYPE board_type
        RETURNING
          VALUE(result) TYPE string,
      check_validity
        IMPORTING
          board TYPE board_type
        RAISING
          cx_parameter_invalid.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.
  METHOD get_state.
    DATA: winner TYPE string.

    check_validity( board ).
    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF count_characters( board, ' ' ) = 0.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    DATA(char_count) = 0.
    LOOP AT board INTO DATA(line).
      char_count += strlen( line ) - strlen( replace( line, char, '' ) ).
    ENDLOOP.
    count = char_count.
  ENDMETHOD.

  METHOD check_winner.
    DATA: lines TYPE TABLE OF string.

    lines = board.
    lines = lines && VALUE #( ( CONDENSE( board[ 1 ] & board[ 2 ] & board[ 3 ] ) )
                             ( CONDENSE( board[ 1+1 ] & board[ 2+1 ] & board[ 3+1 ] ) )
                             ( CONDENSE( board[ 1+2 ] & board[ 2+2 ] & board[ 3+2 ] ) )
                             ( CONDENSE( board[ 1 ] & board[ 2+1 ] & board[ 3+2 ] ) )
                             ( CONDENSE( board[ 3 ] & board[ 2+1 ] & board[ 1+2 ] ) ) ).

    LOOP AT lines INTO DATA(line).
      IF line = 'XXX'.
        result = player_enum-one.
        RETURN.
      ELSEIF line = 'OOO'.
        result = player_enum-two.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_validity.
    DATA(x_count) = count_characters( board, player_enum-one ).
    DATA(o_count) = count_characters( board, player_enum-two ).

    IF x_count < o_count OR x_count - o_count > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
