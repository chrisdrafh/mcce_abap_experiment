CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3 WITH DEFAULT KEY.

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

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS: count_characters
      IMPORTING
        board TYPE board_type
        char  TYPE c LENGTH 1
      RETURNING
        VALUE(count) TYPE i,
      check_win
        IMPORTING board TYPE board_type
                  player TYPE player_type
        RETURNING VALUE(result) TYPE abap_bool,
      check_draw
        IMPORTING board TYPE board_type
        RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_characters( board = board char = player_enum-one ).
    o_count = count_characters( board = board char = player_enum-two ).

    IF x_count - o_count NOT IN 0 AND 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( board = board player = player_enum-one ).
      IF o_count >= x_count OR check_win( board = board player = player_enum-two ).
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ELSE.
        state = state_enum-win.
      ENDIF.
    ELSEIF check_win( board = board player = player_enum-two ).
      IF x_count > o_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ELSE.
        state = state_enum-win.
      ENDIF.
    ELSEIF check_draw( board = board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    DATA: line TYPE string.
    count = 0.
    LOOP AT board INTO line.
      count = count + strlen( line ) - strlen( replace( val = line sub = char with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win.
    result = abap_false.
    " Check rows
    LOOP AT board INTO DATA(line).
      IF line CP ( player && player && player ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    " Check columns
    DO 3 TIMES.
      IF board[ 1 ]( sy-index ) = player AND
         board[ 2 ]( sy-index ) = player AND
         board[ 3 ]( sy-index ) = player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.
    " Check diagonals
    IF board[ 1 ]( 1 ) = player AND board[ 2 ]( 2 ) = player AND board[ 3 ]( 3 ) = player OR
       board[ 1 ]( 3 ) = player AND board[ 2 ]( 2 ) = player AND board[ 3 ]( 1 ) = player.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_draw.
    DATA: line TYPE string.
    result = abap_true.
    LOOP AT board INTO line.
      IF line CS ' '.
        result = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
