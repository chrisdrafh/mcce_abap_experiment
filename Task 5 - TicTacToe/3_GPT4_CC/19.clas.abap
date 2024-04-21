CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

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
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: check_win
      IMPORTING board        TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS: count_marks
      IMPORTING board        TYPE board_type
      IMPORTING mark         TYPE player_type
      RETURNING VALUE(count) TYPE i.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board = board mark = player_enum-one ).
    o_count = count_marks( board = board mark = player_enum-two ).

    " Check basic validity of the board
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = VALUE scx_t100key( msgid = 'ZCUSTOM' msgno = '001' msgty = 'E' ).
    ENDIF.

    " Check if there's a winner
    IF check_win( board ).
      IF x_count = o_count OR x_count < o_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid
          EXPORTING textid = VALUE scx_t100key( msgid = 'ZCUSTOM' msgno = '002' msgty = 'E' ).
      ELSE.
        state = state_enum-win.
      ENDIF.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(line).
      " Check horizontal wins
      IF line CS 'XXX' OR line CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check vertical and diagonal wins
    DO 3 TIMES.
      " Vertical check
      IF board[ 1 ]( sy-index ) = board[ 2 ]( sy-index ) AND
         board[ 2 ]( sy-index ) = board[ 3 ]( sy-index ) AND
         board[ 1 ]( sy-index ) IS NOT INITIAL.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonal check
    IF ( board[ 1 ]( 1 ) = board[ 2 ]( 2 ) AND board[ 2 ]( 2 ) = board[ 3 ]( 3 ) OR
         board[ 1 ]( 3 ) = board[ 2 ]( 2 ) AND board[ 2 ]( 2 ) = board[ 3 ]( 1 ) ) AND
       board[ 2 ]( 2 ) IS NOT INITIAL.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    count = 0.
    LOOP AT board INTO DATA(row).
      count += strlen( row ) - strlen( replace( val = row sub = mark with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
