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

    METHODS: constructor,
             get_state
               IMPORTING board        TYPE board_type
               RETURNING VALUE(state) TYPE string
               RAISING   cx_parameter_invalid.
    
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: validate_board
               IMPORTING board TYPE board_type
               RAISING   cx_parameter_invalid,
             count_marks
               IMPORTING board TYPE board_type
               RETURNING VALUE(x_count) TYPE i
               RETURNING VALUE(o_count) TYPE i,
             check_winner
               IMPORTING board TYPE board_type
               RETURNING VALUE(winner) TYPE player_type,
             check_full_board
               IMPORTING board TYPE board_type
               RETURNING VALUE(is_full) TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type.

    validate_board( board ).

    x_count = count_marks( board ).
    o_count = count_marks( board ).

    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF check_full_board( board ) = abap_true.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board ).
    o_count = count_marks( board ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA: idx TYPE i.

    x_count = 0.
    o_count = 0.

    LOOP AT board INTO DATA(row).
      DO 3 TIMES.
        idx = sy-index.
        IF row+idx(1) = 'X'.
          x_count = x_count + 1.
        ELSEIF row+idx(1) = 'O'.
          o_count = o_count + 1.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    " Implement the logic to check if there is a winner
  ENDMETHOD.

  METHOD check_full_board.
    DATA: idx TYPE i.

    is_full = abap_true.

    LOOP AT board INTO DATA(row).
      DO 3 TIMES.
        idx = sy-index.
        IF row+idx(1) = space.
          is_full = abap_false.
          EXIT.
        ENDIF.
      ENDDO.
      IF is_full = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
