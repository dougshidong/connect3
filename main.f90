program main

    use boardclass
    use prec
    use globals
    use ai

    implicit none

    type(board) :: game
    type(act) :: move
    integer :: iarg
    CHARACTER(len=32) :: arg
    integer(pi) :: player
    integer(pi4):: v, alpha, beta, rounds, mdepth

    big = huge(v)
    imax = 5
    jmax = 4
    iam = p1
    heis = p2
    write(*,*) iargc()

    do iarg = 1, iargc()
        call getarg(iarg, arg)
        if(arg == '-7x6') then
            imax = 5
            jmax = 4
        else if(arg == '-white') then
            write(*,*) 'White Player Selected'
        else if(arg == '-black') then
            iam = p2
            heis = p1
            write(*,*) 'Black Player Selected'
        else
            write(*,*) 'Invalid argument: ', arg
        end if
    end do

    call initialize_board(game)
    call printboard(game)

    player = heis
    if(iam.eq.p2) then
        read(*,'(2I1,A1)') move%i,move%j,move%d
        call movepiece(game, move)
        call printboard(game)
    end if

    rounds = 0
    do while(game%winner.eq.empty)
        rounds = rounds + 1
!       read(*,'(2I1,A1)') move%i,move%j,move%d
        if(player.eq.iam) then
            player = heis
            mdepth = 10
        else
            player = iam
            mdepth = 10
        end if
        write(*,*) 'round:', rounds, 'player', player
        call alphabeta(v, game, -huge(alpha), huge(beta), player, 0, mdepth, move)
        write(*,'(A,2I1,A1," ",I0)') 'Best Move: ', move%i, move%j, move%d, v
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
        if(rounds.eq.50) exit
    end do

    deallocate(game%squares)

    write(*,*) game%winner

    if(game%winner.eq.iam) then
        write(*,'(A,I4,A)') 'I won in ', rounds,' moves. :)'
    endif
    if(game%winner.eq.heis) then
        write(*,'(A,I4,A)') 'I lost in ', rounds,' moves. :('
    endif
    if(game%winner.eq.empty) then
        write(*,'(A,I4,A)') 'It s a tie after ', rounds,' moves. :\'
    endif


end program
