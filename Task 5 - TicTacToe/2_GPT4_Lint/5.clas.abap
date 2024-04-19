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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: check_board_validity
      IMPORTING board        TYPE board_type
      RAISING   cx_parameter_invalid,
             check_winner
      IMPORTING board        TYPE board_type
      RETURNING VALUE(winner) TYPE player_type,
             count_marks
      IMPORTING board        TYPE board_type
                player       TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type.

    x_count = count_marks( board = board player = player_enum-one ).
    o_count = count_marks( board = board player = player_enum-two ).

    " Check for valid board state
    check_board_validity( board ).

    " Determine if there is a winner
    winner = check_winner( board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_board_validity.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board = board player = player_enum-one ).
    o_count = count_marks( board = board player = player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid board: Incorrect turn order or too many plays by one player.'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      " Check for horizontal win
      IF <line>(1) = <line>(2) AND <line>(2) = <line>(3) AND <line>(1) IS NOT INITIAL.
        winner = <line>(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check for vertical and diagonal win
    DO 3 TIMES ASSIGNING FIELD-SYMBOL(<i>).
      " Vertical win
      IF board[1](<i>) = board[2](<i>) AND board[2](<i>) = board[3](<i>) AND board[1](<i>) IS NOT INITIAL.
        winner = board[1](<i>).
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonal win
    IF ( board  = board  AND board  = board  AND board  IS NOT INITIAL ) OR
       ( board  = board  AND board  = board  AND board  IS NOT INITIAL ).
      winner = board .
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      count += STRLEN( CONDENSE( <line> ) USING NO-GAPS ) WHERE ( <line> = player ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
