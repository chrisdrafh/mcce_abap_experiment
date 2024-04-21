CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF board_type,
             line TYPE c LENGTH 3 OCCURS 3,
           END OF board_type.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE c LENGTH 1 VALUE 'X',
                 two TYPE c LENGTH 1 VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS: check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE player_enum OPTIONAL.
    CLASS-METHODS: count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(count_x) TYPE i
                VALUE(count_o) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i,
          winner  TYPE player_enum OPTIONAL.

    count_marks(
      EXPORTING board = board
      IMPORTING count_x = count_x
                count_o = count_o ).

    IF abs( count_x - count_o ) > 1 OR count_o > count_x.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(line).
      IF line = 'XXX'.
        result = player_enum-one.
        RETURN.
      ELSEIF line = 'OOO'.
        result = player_enum-two.
        RETURN.
      ENDIF.
    ENDLOOP.

    DO 3 TIMES.
      DATA(column) = |{ board[1](sy-index) }{ board[2](sy-index) }{ board[3](sy-index) }|.
      IF column = 'XXX'.
        result = player_enum-one.
        RETURN.
      ELSEIF column = 'OOO'.
        result = player_enum-two.
        RETURN.
      ENDIF.
    ENDDO.

    DATA(diag1) = |{ board  }{ board  }{ board  }|.
    DATA(diag2) = |{ board  }{ board  }{ board  }|.
    IF diag1 = 'XXX' OR diag2 = 'XXX'.
      result = player_enum-one.
      RETURN.
    ELSEIF diag1 = 'OOO' OR diag2 = 'OOO'.
      result = player_enum-two.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(line).
      count_x = count_x + strlen( regex_replace( val = line regex = '[^X]' with = '' ) ).
      count_o = count_o + strlen( regex_replace( val = line regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
